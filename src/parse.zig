const std = @import("std");
const Pcre = @import("pcre2zig");
const Regex = Pcre.CompiledCode;
const UnicodeView = std.unicode.Utf8View;
const LineFeeder = @import("line_feeder.zig").LineFeeder;
const mem = std.mem;

pub const Command = enum { substitute, print, delete, insert, to_hold, to_pattern, exchange, next_line, quit, translate, group, jump, tmp_jump, unambiguous_print, file, nil };

pub const AddressKind = union(enum) {
    Regex: Regex,
    Line: usize,
    LastLine: void,
};

pub const Address = struct { invert: bool, kind: AddressKind };

pub const SedInstruction = struct {
    address1: ?Address = null,
    address2: ?Address = null,
    found_first: bool = false,
    cmd: union(Command) {
        insert: struct {
            before: bool,
            contents: []const u8,
        },
        to_hold: bool, // if true: copy instead of append
        to_pattern: bool,
        exchange: void,
        substitute: struct {
            regex: Regex, //
            repl: []const u8,
            flags: SubstituteFlags = .{},
        }, // s/a/b/
        print: bool,
        delete: bool, // d - true; D - false
        next_line: bool,
        quit: void,
        group: usize,
        translate: struct {
            from: []u21,
            to: []u21,
        },
        jump: usize,
        tmp_jump: struct {
            label: []const u8,
            declared: usize,
        },
        unambiguous_print: void,
        file: struct {
            read: bool,
            path: []const u8,
        },
        nil: void,
    } = .nil,
};

const SubstituteFlags = struct {
    global: bool = false,
    print: bool = false,
    nth: ?u8 = null,
};

pub const GeneratorError = error{ Unterminated, OutOfMemory, UnknownCommand, ExpectedAddress, ExpectedCommand, UnmatchedBraces, ReusedLabel, UndefinedLabel, EmptyLabel, InvalidUtf8 } || Pcre.PcreError;

const LabelHashMap = std.StringHashMap(usize);

pub const Generator = struct {
    buf: []const u8,
    index: usize = 0,
    group_level: usize = 0,
    arena: std.heap.ArenaAllocator,
    list: std.ArrayList(u8),
    _program: []SedInstruction,
    labels: LabelHashMap,

    const Self = @This();

    pub fn init(alloc: mem.Allocator, script: []const u8, error_offset: *usize) !Self {
        var ret = Self{ .arena = std.heap.ArenaAllocator.init(alloc), .list = undefined, .labels = LabelHashMap.init(alloc), .buf = script, ._program = undefined };
        ret.list = std.ArrayList(u8).init(ret.arena.allocator());
        errdefer {
            ret.deinit();
            error_offset.* = ret.index;
        }
        ret._program = try ret.work();
        try ret.initializeJumps();
        return ret;
    }

    pub fn deinit(self: *Self) void {
        self.labels.deinit();
        self.list.deinit();
        self.arena.deinit();
    }

    pub fn program(self: *Self) []SedInstruction {
        return self._program;
    }

    fn peekChar(self: *Self) ?u8 {
        if (self.index >= self.buf.len) return null;
        return self.buf[self.index];
    }

    fn nextChar(self: *Self) ?u8 {
        const ret = self.peekChar();
        self.index += 1;
        return ret;
    }

    fn skipWhitespace(self: *Self) void {
        while (self.peekChar()) |c| switch (c) {
            ' ', '\t' => self.index += 1,
            else => return,
        };
    }

    fn comment(self: *Self) void {
        while (true) {
            const c = self.nextChar() orelse return;
            if (c == '\n') {
                return;
            }
        }
    }

    fn getDelim(self: *Self) !u8 {
        const ret = self.nextChar() orelse return error.Unterminated;
        return if (ret == '\n') error.Unterminated else ret;
    }

    fn nextArg(self: *Self, delim: u8) ![]const u8 {
        var seen_bl = false;
        while (self.nextChar()) |b| {
            if (seen_bl) {
                if (b == '\n' or b == delim) {
                    _ = self.list.pop();
                }
                try self.list.append(b);
                seen_bl = false;
            } else {
                if (b == delim) {
                    // return slice
                    return self.list.toOwnedSlice();
                } else if (b == '\n') {
                    return error.Unterminated;
                }
                seen_bl = b == '\\';
                try self.list.append(b);
            }
        } else {
            return error.Unterminated;
        }
    }

    // translates ye olde sed matches into PCRE
    fn pcreArg(self: *Self, delim: u8) ![]const u8 {
        var seen_bl = false;
        while (self.nextChar()) |b| {
            if (seen_bl) {
                switch (b) {
                    '0'...'9' => {
                        try self.list.appendSlice("${");
                        try self.list.append(b);
                        try self.list.append('}');
                    },
                    '{' => {
                        try self.list.appendSlice("${");
                        while (self.nextChar()) |b2| {
                            switch (b2) {
                                '}' => break,
                                else => try self.list.append(b2),
                            }
                        } else return error.Unterminated;
                        try self.list.append('}');
                    },
                    else => {
                        if (b == '$') try self.list.append('$');
                        try self.list.append(b);
                    },
                }
                seen_bl = false;
            } else {
                if (b == delim) {
                    return self.list.toOwnedSlice();
                }
                switch (b) {
                    '&' => try self.list.appendSlice("${0}"),
                    '$' => try self.list.appendSlice("$$"),
                    '\\' => seen_bl = true,
                    else => try self.list.append(b),
                }
                seen_bl = b == '\\';
            }
        } else {
            return error.Unterminated;
        }
    }

    fn nextText(self: *Self) ![]const u8 {
        var seen_bl = false;
        while (true) {
            const b = self.nextChar() orelse '\n';
            if (b == '\n') {
                if (seen_bl) {
                    _ = self.list.pop();
                } else {
                    self.index -= 1; // leave it as semicolon
                    return self.list.toOwnedSlice();
                }
            }
            try self.list.append(b);
            seen_bl = b == '\\';
        }
    }

    fn nextLabel(self: *Self) ![]const u8 {
        while (self.nextChar()) |b| switch (b) {
            '\n', ';', ' ', '\t' => {
                self.index -= 1;
                break;
            },
            else => try self.list.append(b),
        };
        if (self.list.items.len == 0) return error.EmptyLabel;
        return self.list.toOwnedSlice();
    }

    fn nextOption(self: *Self) !?u8 {
        const b = self.nextChar() orelse return null;
        if (b == null or std.ascii.isAlpha(b)) {
            return b;
        } else {
            self.index -= 1;
            return null;
        }
    }

    fn nextNumber(self: *Self) !?usize {
        const start = self.index;
        while (self.peekChar()) |b| switch (b) {
            '0'...'9' => self.index += 1,
            else => break,
        };
        const end = self.index;
        if (start == end) {
            return null;
        } else {
            return std.fmt.parseUnsigned(usize, self.buf[start..end], 10) catch unreachable;
        }
    }

    fn maybeInvert(self: *Self) !bool {
        const b = self.peekChar() orelse return false;
        if (b == '!') {
            self.index += 1;
            return true;
        }
        return false;
    }

    fn work(self: *Self) GeneratorError![]SedInstruction {
        var output = std.ArrayList(SedInstruction).init(self.arena.allocator());
        while (true) {
            var ret = SedInstruction{};
            self.skipWhitespace();
            if (self.peekChar() == @as(u8, '#')) {
                self.comment();
                continue;
            }

            var inline_for_break = false;
            inline for (.{ "address1", "address2" }) |addr_field, i| {
                if (!inline_for_break) {
                    if (try self.nextNumber()) |line_addr| {
                        @field(ret, addr_field) = .{ .kind = .{ .Line = line_addr }, .invert = try self.maybeInvert() };
                    } else switch (self.nextChar() orelse return output.toOwnedSlice()) {
                        '/' => {
                            const r = try self.nextArg('/');
                            const invert = try self.maybeInvert();
                            @field(ret, addr_field) = .{ .kind = .{ .Regex = try Pcre.compile(r, .{}) }, .invert = invert };
                        },
                        '\\' => {
                            const r = try self.nextArg(try self.getDelim());
                            const invert = try self.maybeInvert();
                            @field(ret, addr_field) = .{ .kind = .{ .Regex = try Pcre.compile(r, .{}) }, .invert = invert };
                        },
                        '$' => {
                            @field(ret, addr_field) = .{ .kind = .{ .LastLine = {} }, .invert = try self.maybeInvert() };
                        },
                        else => {
                            if (i != 0) {
                                return error.ExpectedAddress;
                            } else {
                                self.index -= 1;
                            }
                        },
                    }
                    self.skipWhitespace();
                    if (self.peekChar() == @as(u8, ',')) {
                        self.index += 1;
                    } else {
                        // break; https://github.com/ziglang/zig/issues/11606
                        inline_for_break = true;
                    }
                }
            }

            self.skipWhitespace();

            const cmd = self.nextChar() orelse break;
            const isL = std.ascii.isLower;
            switch (cmd) {
                '{' => {
                    const my_level = self.group_level;
                    self.group_level += 1;
                    const group_cmds = try self.work();
                    if (my_level != self.group_level) return error.UnmatchedBraces;
                    defer self.arena.allocator().free(group_cmds);
                    ret.cmd = .{ .group = group_cmds.len };
                    try output.append(ret);
                    try output.appendSlice(group_cmds);
                    ret = SedInstruction{};
                },
                'p', 'P' => ret.cmd = .{ .print = isL(cmd) },
                'd', 'D' => ret.cmd = .{ .delete = isL(cmd) },
                'h', 'H' => ret.cmd = .{ .to_hold = isL(cmd) },
                'g', 'G' => ret.cmd = .{ .to_pattern = isL(cmd) },
                'n', 'N' => ret.cmd = .{ .next_line = isL(cmd) },
                'q' => ret.cmd = .quit,
                'i', 'a' => ret.cmd = .{ .insert = .{ .before = cmd == 'i', .contents = try self.nextText() } },
                's' => {
                    const delim = try self.getDelim();
                    const regex = try Pcre.compile(try self.nextArg(delim), .{});
                    const repl = try self.pcreArg(delim);
                    var opts = SubstituteFlags{};
                    while (self.nextChar()) |opt| switch (opt) {
                        'g' => opts.global = true,
                        'p' => opts.print = true,
                        '0'...'9' => opts.nth = opt - '0',
                        else => {
                            self.index -= 1;
                            break;
                        },
                    };
                    ret.cmd = .{ .substitute = .{
                        .regex = regex,
                        .repl = repl,
                        .flags = opts,
                    } };
                },
                'y' => {
                    const delim = try self.getDelim();
                    // TODO: Detect LC_COLLATE to disable UTF-8 support.
                    ret.cmd = .{ .translate = undefined };
                    inline for (.{ "from", "to" }) |direction| {
                        const raw = try self.nextArg(delim);
                        var codepoints = std.ArrayList(u21).init(self.arena.allocator());
                        var raw_utf8 = try UnicodeView.init(raw);
                        var raw_it = raw_utf8.iterator();
                        while (raw_it.nextCodepoint()) |c21| try codepoints.append(c21);
                        @field(ret.cmd.translate, direction) = codepoints.toOwnedSlice();
                    }
                },
                ':' => {
                    const label = try self.nextLabel();
                    var label_entry = try self.labels.getOrPut(label);
                    if (label_entry.found_existing) {
                        return error.ReusedLabel;
                    }
                    label_entry.value_ptr.* = output.items.len;
                    continue; // no semicolon and no instruction output needed
                },
                'x' => ret.cmd = .exchange,
                'b' => {
                    const decl = self.index;
                    const label = try self.nextLabel();
                    if (self.labels.get(label)) |pos| {
                        ret.cmd = .{ .jump = pos };
                    } else {
                        // a promise
                        ret.cmd = .{ .tmp_jump = .{
                            .label = label,
                            .declared = decl,
                        } };
                    }
                },
                '\n', ';', '}' => self.index -= 1, // endings
                else => {
                    std.log.err("Unknown command: '{c}'", .{cmd});
                    return error.UnknownCommand;
                },
            }

            self.skipWhitespace();
            // append command
            if (ret.cmd != .nil) {
                try output.append(ret);
            } else if (ret.address1 != null) {
                std.log.err("Address without command found.", .{});
                return error.ExpectedAddress;
            }
            // look for a closure
            const semicolon = self.nextChar() orelse {
                try output.append(ret);
                break;
            };
            switch (semicolon) {
                '\n', ';' => {},
                '}' => {
                    if (self.group_level == 0) {
                        return error.UnmatchedBraces;
                    }
                    self.group_level -= 1;
                    // end early
                    return output.toOwnedSlice();
                },
                else => {
                    std.log.err("Expected {{, ; or newline, got {c}", .{semicolon});
                    return error.Unterminated;
                },
            }
        }
        if (self.group_level != 0) return error.UnmatchedBraces;
        return output.toOwnedSlice();
    }

    fn initializeJumps(self: *Self) !void {
        for (self._program) |*instr| switch (instr.cmd) {
            .tmp_jump => |jmp| {
                if (self.labels.get(jmp.label)) |pos| {
                    instr.cmd = .{ .jump = pos };
                } else {
                    self.index = jmp.declared;
                    std.log.err("Tried to use undefined label '{s}'", .{jmp.label});
                    return error.UndefinedLabel;
                }
            },
            else => {},
        };
    }
};

pub const RunnerOptions = struct {
    print: bool = true,
    posix_quit: bool = false,
};

pub fn Runner(comptime Source: type, comptime Writer: type) type {
    return struct {
        in_stream: LineFeeder(Source),
        out_stream: Writer,
        opts: RunnerOptions = .{},
        alloc: std.mem.Allocator,
        input: []SedInstruction,
        pattern_space: std.ArrayList(u8),
        next_line: std.ArrayList(u8),
        hold_space: std.ArrayList(u8),
        last_line: bool = false,

        const Self = @This();
        const buf_size = 1 << 16; //max size for stream operations

        pub fn init(cleanup: fn (Source) void, out_stream: Writer, alloc: std.mem.Allocator, instructions: []SedInstruction, opts: RunnerOptions) Self {
            return .{
                .input = instructions,
                .in_stream = LineFeeder(Source).init(alloc, cleanup),
                .out_stream = out_stream,
                .pattern_space = std.ArrayList(u8).init(alloc),
                .next_line = std.ArrayList(u8).init(alloc),
                .hold_space = std.ArrayList(u8).init(alloc),
                .alloc = alloc,
                .opts = opts,
            };
        }

        pub fn deinit(self: *Self) void {
            self.in_stream.deinit();
            self.pattern_space.deinit();
            self.hold_space.deinit();
            self.next_line.deinit();
        }

        pub fn addSource(self: *Self, src: Source) !void {
            return self.in_stream.addSource(src);
        }

        pub fn remainingSources(self: *Self) usize {
            return self.in_stream.remainingSources();
        }

        fn printPatternSpace(self: *Self) !void {
            self.out_stream.print("{s}\n", .{self.pattern_space.items}) catch return error.IOError;
        }

        fn matchAddress(self: *Self, a: ?Address) !bool {
            if (a == null) return true;
            const ret = switch (a.?.kind) {
                .Line => |l| self.in_stream.line() == l,
                .LastLine => self.in_stream.isLastLine(),
                .Regex => |r| {
                    var data = try Pcre.MatchData.init(r);
                    defer data.deinit();
                    return Pcre.match(r, self.pattern_space.items, 0, &data, .{}) catch false;
                },
            };
            if (a.?.invert) return !ret else return ret;
        }

        const RunError = Pcre.PcreError || error{ OutOfMemory, DeleteCommand, QuitCommand, NotImplemented, IOError, InvalidUtf8 };
        fn runInstructions(self: *Self, input: []SedInstruction) !void {
            var i: usize = 0;
            while (i < input.len) { // no : (i += 1) because of jumps
                var item = &input[i];
                // std.debug.print("instr {}: {}\n", .{ i, item.cmd });
                // std.debug.print("PS: {s} :: HS: {s}\n", .{ self.pattern_space.items, self.hold_space.items });
                // defer std.debug.print("APS: {s} :: AHS: {s}\n", .{ self.pattern_space.items, self.hold_space.items });
                i += 1;

                if (!(item.found_first or try self.matchAddress(item.address1))) {
                    item.found_first = false;
                    // skip groups
                    switch (item.cmd) {
                        .group => |g| i += g,
                        else => {},
                    }
                    continue;
                }

                if (item.address2) |a2| {
                    // Spec says "if the second address is a number less than or equal to the line number first selected, only one line shall be selected."
                    const line_early = switch (a2.kind) {
                        .Line => |l| l <= self.in_stream.line() and !a2.invert,
                        else => false,
                    };
                    if (line_early or (item.found_first and try self.matchAddress(a2))) {
                        item.found_first = false;
                    } else {
                        item.found_first = true;
                    }
                }

                switch (item.cmd) {
                    .quit => return error.QuitCommand,
                    .print => |p| {
                        if (p) {
                            try self.printPatternSpace();
                        } else {
                            self.out_stream.print("{s}\n", .{mem.sliceTo(self.pattern_space.items, '\n')}) catch return error.IOError;
                        }
                    },
                    .delete => |d| {
                        if (d) {
                            return error.DeleteCommand;
                        } else {
                            if (mem.indexOfScalar(u8, self.pattern_space.items, '\n')) |newline| {
                                try self.pattern_space.replaceRange(0, newline, &.{});
                                // would be more fun to tail but https://github.com/ziglang/zig/issues/5692
                                i = 0;
                                continue;
                            } else {
                                return error.DeleteCommand;
                            }
                        }
                    },
                    .insert => |insert| {
                        const text = insert.contents;
                        if (insert.before) {
                            try self.pattern_space.insert(0, '\n');
                            try self.pattern_space.insertSlice(0, text);
                        } else {
                            try self.pattern_space.appendSlice(text);
                        }
                    },
                    .to_hold => |h| {
                        if (h) {
                            self.hold_space.clearRetainingCapacity();
                        } else {
                            try self.hold_space.append('\n');
                        }
                        try self.hold_space.appendSlice(self.pattern_space.items);
                    },
                    .to_pattern => |g| {
                        if (g) {
                            self.pattern_space.clearRetainingCapacity();
                        } else {
                            try self.pattern_space.append('\n');
                        }
                        try self.pattern_space.appendSlice(self.hold_space.items);
                    },
                    .exchange => {
                        mem.swap(std.ArrayList(u8), &self.pattern_space, &self.hold_space);
                    },
                    .next_line => |n| {
                        if (!self.in_stream.isLastLine()) {
                            if (n) {
                                if (self.opts.print) try self.printPatternSpace();
                                self.pattern_space.clearRetainingCapacity();
                            } else {
                                try self.pattern_space.append('\n');
                            }
                            const l = (self.in_stream.next() catch return error.IOError) orelse unreachable;
                            defer self.alloc.free(l);
                            try self.pattern_space.appendSlice(l);
                        } else {
                            if (n and self.opts.print) {
                                try self.printPatternSpace();
                            }
                            // http://sed.sourceforge.net/sedfaq6.html#s6.7.5
                            if (n or self.opts.posix_quit) {
                                return error.QuitCommand;
                            }
                        }
                    },
                    .substitute => |s| {
                        var match_data = try Pcre.MatchData.init(s.regex);
                        defer match_data.deinit();
                        if (try Pcre.match(s.regex, self.pattern_space.items, 0, &match_data, .{})) {
                            var match_it = Pcre.MatchIterator.init(s.regex, match_data, self.pattern_space.items);
                            // Query for matches.
                            const has_match = b: {
                                var match_index: usize = 0;
                                const expect = s.flags.nth orelse 0;
                                while (try match_it.next()) : (match_index += 1) {
                                    if (match_index == expect) break :b true;
                                }
                                break :b false;
                            };
                            if (has_match) {
                                const subject = self.pattern_space.toOwnedSlice();
                                defer self.alloc.free(subject);
                                try self.pattern_space.ensureUnusedCapacity(buf_size);
                                self.pattern_space.expandToCapacity();
                                // do the replacement in the basement
                                const offset = match_it.ovector.?[0];
                                // std.debug.print("STARTING {d} at {s}\n", .{ offset, subject });
                                const the_slice = try Pcre.replace(s.regex, subject, offset, s.repl, self.pattern_space.items, .{
                                    .bits = if (s.flags.global) Pcre.pcre2.PCRE2_SUBSTITUTE_GLOBAL else 0,
                                    .data_opt = match_data,
                                });
                                // hide the uninitialized 640k
                                self.pattern_space.shrinkRetainingCapacity(the_slice.len);
                                // p option
                                if (s.flags.print) {
                                    try self.printPatternSpace();
                                }
                            }
                        }
                    },
                    .translate => |y| {
                        const subject = self.pattern_space.toOwnedSlice();
                        defer self.alloc.free(subject);
                        var pat_utf8 = try UnicodeView.init(subject);
                        var pat_it = pat_utf8.iterator();
                        while (pat_it.nextCodepointSlice()) |cp_s| {
                            const cp = std.unicode.utf8Decode(cp_s) catch unreachable;
                            for (y.from) |from, index| {
                                if (cp == from) {
                                    var utf8_buf: [4]u8 = undefined;
                                    const utf8_len = try std.unicode.utf8Encode(y.to[std.math.min(index, y.to.len - 1)], &utf8_buf);
                                    try self.pattern_space.appendSlice(utf8_buf[0..utf8_len]);
                                    break;
                                }
                            } else {
                                try self.pattern_space.appendSlice(cp_s);
                            }
                        }
                    },
                    .group => {},
                    .jump => |b| {
                        i = b;
                        continue;
                    },
                    .tmp_jump, .nil => {
                        std.log.err("Uninitialized instruction, file a bug", .{});
                        std.os.exit(1);
                    },
                    else => return error.NotImplemented,
                }
            }
        }

        pub fn run(self: *Self) !void {
            while (try self.in_stream.next() catch error.IOError) |l| {
                defer self.alloc.free(l);
                self.pattern_space.clearRetainingCapacity();
                try self.pattern_space.appendSlice(l);
                self.runInstructions(self.input) catch |e| switch (e) {
                    error.QuitCommand => return,
                    error.DeleteCommand => continue,
                    else => return e,
                };
                if (self.opts.print)
                    try self.printPatternSpace();
            }
        }
    };
}

pub fn runner(comptime Source: type, cleanup: fn (Source) void, out: anytype, a: std.mem.Allocator, i: []SedInstruction, opts: RunnerOptions) Runner(Source, @TypeOf(out)) {
    return Runner(Source, @TypeOf(out)).init(cleanup, out, a, i, opts);
}
