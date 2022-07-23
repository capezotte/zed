const std = @import("std");
const Pcre = @import("pcre2zig");
const Regex = Pcre.CompiledCode;
const mem = std.mem;

pub const Command = enum { substitute, print, insert, quit, translate, group, label, jump, sentinel };

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
        insert: []const u8, // itext
        substitute: struct {
            regex: Regex,
            repl: []const u8,
        }, // s/a/b/
        print: void,
        sentinel: void,
        quit: void,
        group: []SedInstruction,
        translate: struct {
            from: []u21,
            to: []u21,
        },
        jump: void,
        label: void,
    },
};

pub fn LineFeeder(comptime Source: type) type {
    return struct {
        fifo: InFifoType,
        current: ?Source = null,
        alloc: mem.Allocator,
        lookahead: ?[]const u8 = null,
        cleanup: fn (Source) void,
        last: bool = false,

        const InFifoType = std.fifo.LinearFifo(Source, .Dynamic);
        const Self = @This();

        pub fn init(a: mem.Allocator, cleanup: fn (Source) void) Self {
            return .{
                .cleanup = cleanup,
                .fifo = InFifoType.init(a),
                .alloc = a,
            };
        }

        pub fn deinit(self: *Self) void {
            self.fifo.deinit();
        }

        pub fn isLastLine(self: *Self) bool {
            return self.last;
        }

        pub fn remainingSources(self: *Self) usize {
            return self.fifo.readableLength() + @as(usize, if (self.current == null) 0 else 1);
        }

        pub fn addSource(self: *Self, i: Source) !void {
            try self.fifo.writeItem(i);
            if (self.current == null) {
                self.current = self.fifo.readItem().?;
                try self.lookAhead();
            }
        }

        fn lookAhead(self: *Self) !void {
            if (self.current == null) {
                return;
            } else while (true) {
                self.lookahead = try self.current.?.reader().readUntilDelimiterOrEofAlloc(self.alloc, '\n', 2048);
                if (self.lookahead == null) {
                    self.cleanup(self.current.?);
                    self.current = self.fifo.readItem();
                    if (self.current == null) break;
                } else break;
            }
            self.last = self.current == null;
        }

        pub fn next(self: *Self) !?[]const u8 {
            if (self.lookahead) |l| {
                try self.lookAhead();
                return l;
            } else {
                return null;
            }
        }
    };
}

pub const GeneratorError = error{ Unterminated, OutOfMemory, UnknownCommand, ExpectedAddress, UnmatchedBraces } || Pcre.PcreError;

pub const Generator = struct {
    buf: []const u8,
    index: usize = 0,
    group_level: usize = 0,
    alloc: std.mem.Allocator,

    const Self = @This();

    pub fn init(alloc: mem.Allocator, script: []const u8) Self {
        return .{ .alloc = alloc, .buf = script };
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
            ' ', '\t', '\n' => self.index += 1,
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
        var seen_backslash = false;
        var list = std.ArrayList(u8).init(self.alloc);
        while (self.nextChar()) |b| {
            if (b == delim) {
                if (seen_backslash) {
                    // pop the backslash and go on
                    _ = list.pop();
                } else {
                    // return slice
                    return list.toOwnedSlice();
                }
            }
            if (b == '\n') {
                if (seen_backslash) {
                    // literal newline escaped :D
                    _ = list.pop();
                } else {
                    // unterminated argument lists, in my sed?
                    return error.Unterminated;
                }
            }
            try list.append(b);
            seen_backslash = b == '\\';
        } else {
            return error.Unterminated;
        }
    }

    fn nextText(self: *Self) ![]const u8 {
        var seen_bl = false;
        var list = std.ArrayList(u8).init(self.alloc);
        while (true) {
            const b = self.nextChar() orelse '\n';
            if (b == '\n') {
                if (seen_bl) {
                    _ = list.pop();
                } else {
                    return list.toOwnedSlice();
                }
            }
            try list.append(b);
            seen_bl = b == '\\';
        }
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
            self.index -= 1;
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

    pub fn makeCommand(self: *Self) GeneratorError!?SedInstruction {
        var ret = SedInstruction{ .cmd = .sentinel };

        self.skipWhitespace();
        while (self.peekChar() == @as(u8, '#')) {
            self.comment();
            self.skipWhitespace();
        }

        var inline_for_break = false;
        inline for (.{ "address1", "address2" }) |addr_field| {
            if (!inline_for_break) {
                if (try self.nextNumber()) |line_addr| {
                    @field(ret, addr_field) = .{ .kind = .{ .Line = line_addr }, .invert = try self.maybeInvert() };
                } else switch (self.nextChar() orelse return null) {
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
                        if (inline_for_break) {
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

        const cmd = self.nextChar() orelse return null;
        switch (cmd) {
            '{' => {
                const my_level = self.group_level;
                self.group_level += 1;
                var commands = std.ArrayList(SedInstruction).init(self.alloc);
                while (self.group_level > my_level) {
                    const made_cmd = try self.makeCommand();
                    try commands.append(made_cmd orelse return error.UnmatchedBraces);
                }
                ret.cmd = .{ .group = commands.toOwnedSlice() };
                return ret; // no semicolons needed
            },
            '}' => return error.UnmatchedBraces,
            'p' => ret.cmd = .print,
            'q' => ret.cmd = .quit,
            'i' => {
                ret.cmd = .{ .insert = try self.nextText() };
            },
            's' => {
                const delim = try self.getDelim();
                const regex = try Pcre.compile(try self.nextArg(delim), .{});
                const repl = try self.nextArg(delim);
                ret.cmd = .{ .substitute = .{
                    .regex = regex,
                    .repl = repl,
                } };
            },
            '\n', ';' => {},
            else => return error.UnknownCommand,
        }

        self.skipWhitespace();

        // look for a closure
        switch (self.nextChar() orelse return ret) {
            '\n', ';' => return ret,
            '}' => {
                if (self.group_level == 0) {
                    return error.UnmatchedBraces;
                }
                self.group_level -= 1;
                return ret;
            },
            else => return error.Unterminated,
        }
    }

    pub fn work(self: *Self) ![]SedInstruction {
        var program = std.ArrayList(SedInstruction).init(self.alloc);
        while (try self.makeCommand()) |cmd| {
            try program.append(cmd);
        } else {
            return if (self.group_level == 0) program.toOwnedSlice() else error.UnmatchedBraces;
        }
    }
};

pub fn Runner(comptime Source: type, comptime Writer: type) type {
    return struct {
        in_stream: LineFeeder(Source),
        out_stream: Writer,
        print: bool = true,
        alloc: std.mem.Allocator,
        input: []SedInstruction,
        pattern_space: std.ArrayList(u8),
        next_line: std.ArrayList(u8),
        hold_space: std.ArrayList(u8),
        last_line: bool = false,
        index: usize = 0,

        const Self = @This();
        const buf_size = 1 << 16; //max size for stream operations

        pub fn init(cleanup: fn (Source) void, out_stream: Writer, alloc: std.mem.Allocator, instructions: []SedInstruction, print: bool) Self {
            return .{
                .input = instructions,
                .in_stream = LineFeeder(Source).init(alloc, cleanup),
                .out_stream = out_stream,
                .pattern_space = std.ArrayList(u8).init(alloc),
                .next_line = std.ArrayList(u8).init(alloc),
                .hold_space = std.ArrayList(u8).init(alloc),
                .alloc = alloc,
                .print = print,
            };
        }

        pub fn deinit(self: *Self) void {
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

        fn performReplacement(self: *Self, r: Regex, repl: []const u8, global: bool) !void {
            _ = global;
            const subject = self.pattern_space.toOwnedSlice();
            defer self.alloc.free(subject);
            try self.pattern_space.ensureUnusedCapacity(1 << 16);
            self.pattern_space.expandToCapacity();
            const the_slice = try Pcre.replace(r, subject, 0, repl, self.pattern_space.items, .{});
            self.pattern_space.shrinkRetainingCapacity(the_slice.len);
        }

        fn matchAddress(self: *Self, a: ?Address) !bool {
            if (a == null) return true;
            const ret = switch (a.?.kind) {
                .Line => |l| self.index == l,
                .LastLine => self.in_stream.isLastLine(),
                .Regex => |r| {
                    var data = try Pcre.MatchData.init(r);
                    defer data.deinit();
                    return Pcre.match(r, self.pattern_space.items, 0, &data, .{}) catch false;
                },
            };
            if (a.?.invert) return !ret else return ret;
        }

        const RunError = Pcre.PcreError || error{ OutOfMemory, QuitCommand, NotImplemented, IOError };
        fn runInstructions(self: *Self, input: []SedInstruction) RunError!void {
            var i: usize = 0;
            while (i < input.len) : (i += 1) {
                var item = &input[i];

                if (!(item.found_first or try self.matchAddress(item.address1))) {
                    item.found_first = false;
                    continue;
                }

                if (item.address2) |a2| {
                    // Spec says if it matches a previous line, we must consider only the first pattern.
                    const line_early = switch (a2.kind) {
                        .Line => |l| l <= self.index and !a2.invert,
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
                    .print => try self.printPatternSpace(),
                    .insert => |text| {
                        try self.pattern_space.insert(0, '\n');
                        try self.pattern_space.insertSlice(0, text);
                    },
                    .substitute => |s| self.performReplacement(s.regex, s.repl, false) catch continue,
                    .group => |g| try self.runInstructions(g),
                    .sentinel => {
                        std.log.err("Invalid instruction, file a bug", .{});
                        std.os.exit(1);
                    },
                    else => return error.NotImplemented,
                }
            }
        }

        pub fn run(self: *Self) !void {
            while (try self.in_stream.next() catch error.IOError) |l| {
                self.index += 1;
                defer self.alloc.free(l);
                self.pattern_space.clearRetainingCapacity();
                try self.pattern_space.appendSlice(l);
                self.runInstructions(self.input) catch |e| switch (e) {
                    error.QuitCommand => {},
                    else => return e,
                };
                if (self.print)
                    try self.printPatternSpace();
            }
        }
    };
}

pub fn runner(comptime Source: type, cleanup: fn (Source) void, out: anytype, a: std.mem.Allocator, i: []SedInstruction, print: bool) Runner(Source, @TypeOf(out)) {
    return Runner(Source, @TypeOf(out)).init(cleanup, out, a, i, print);
}
