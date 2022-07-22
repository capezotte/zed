const std = @import("std");
const Pcre = @import("pcre2zig");
const Regex = Pcre.CompiledCode;
const mem = std.mem;

pub const Command = enum { substitute, print, insert, quit, translate, group, label, sentinel };

pub const AddressKind = union(enum) {
    Regex: Regex,
    Line: usize,
    LastLine: void,
};

pub const Address = struct { invert: bool, kind: AddressKind };

pub const Token = union(enum) {
    Address: Address,
    Command: Command,
    Argument: []const u8,
    Option: u8,
    Comma,
    Not,
    BeginGroup,
    EndGroup,
    Semicolon,
};

pub const TokenFifo = std.fifo.LinearFifo(Token, .Dynamic);

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

pub fn Scanner(comptime Reader: type) type {
    return struct {
        stream: Reader,
        list: std.ArrayList(u8),
        alloc: std.mem.Allocator,
        output: *TokenFifo,
        char_lookahead: ?u8 = null,

        const Self = @This();

        pub fn init(the_stream: Reader, alloc: mem.Allocator, out: *TokenFifo) Self {
            return .{ .stream = the_stream, .alloc = alloc, .list = std.ArrayList(u8).init(alloc), .output = out };
        }

        pub fn deinit(self: *Self) void {
            self.list.deinit();
        }

        fn nextChar(self: *Self) !?u8 {
            if (self.char_lookahead) |c| {
                self.char_lookahead = null;
                return c;
            } else {
                return self.stream.readByte() catch |err| switch (err) {
                    error.EndOfStream => return null,
                    else => return error.IOError,
                };
            }
        }

        fn nextCharU(self: *Self) !u8 {
            return if (try self.nextChar()) |c| c else error.Unterminated;
        }

        fn getDelim(self: *Self) !u8 {
            const ret = try self.nextCharU();
            if (ret == '\n') {
                return error.Unterminated;
            }
            return ret;
        }

        fn nextArg(self: *Self, delim: u8) ![]const u8 {
            var seen_backslash = false;
            while (true) {
                const b = try self.nextCharU();
                if (b == delim) {
                    if (seen_backslash) {
                        // pop the backslash and go on
                        _ = self.list.pop();
                    } else {
                        // return slice
                        return self.list.toOwnedSlice();
                    }
                }
                if (b == '\n') {
                    if (seen_backslash) {
                        // literal newline escaped :D
                        _ = self.list.pop();
                    } else {
                        // unterminated argument lists, in my sed?
                        return error.Unterminated;
                    }
                }
                try self.list.append(b);
                seen_backslash = b == '\\';
            }
        }

        fn nextText(self: *Self) ![]const u8 {
            var seen_bl = false;
            while (true) {
                const b = (try self.nextChar()) orelse '\n';
                if (b == '\n') {
                    if (seen_bl) {
                        _ = self.list.pop();
                    } else {
                        self.char_lookahead = '\n';
                        return self.list.toOwnedSlice();
                    }
                }
                try self.list.append(b);
                seen_bl = b == '\\';
            }
        }

        fn nextOption(self: *Self) !?u8 {
            const b = (try self.nextChar()) orelse return null;
            if (b == null or std.ascii.isAlpha(b)) {
                return b;
            } else {
                self.char_lookahead = b;
            }
        }

        fn maybeInvert(self: *Self) !bool {
            const b = (try self.nextChar()) orelse return false;
            if (b == '!') {
                return true;
            }
            self.char_lookahead = b;
            return false;
        }

        fn tokenOut(self: *Self, t: Token) !void {
            try self.output.writeItem(t);
        }

        pub fn work(self: *Self) !void {
            while (try self.nextChar()) |b| switch (b) {
                '#' => {
                    // comment: eat until newline
                    while (((try self.nextChar()) orelse '\n') == '\n') {}
                    try self.tokenOut(.{ .Semicolon = {} });
                },
                ' ', '\t' => {},
                '/' => {
                    const r = try self.nextArg('/');
                    const invert = try self.maybeInvert();
                    try self.tokenOut(.{ .Address = Address{ .kind = .{ .Regex = try Pcre.compile(r, .{}) }, .invert = invert } });
                },
                '\\' => {
                    const r = try self.nextArg(try self.getDelim());
                    const invert = try self.maybeInvert();
                    try self.tokenOut(.{ .Address = Address{ .kind = .{ .Regex = try Pcre.compile(r, .{}) }, .invert = invert } });
                },
                '0'...'9' => {
                    try self.list.append(b);
                    while (true) {
                        const new_b = (try self.nextChar()) orelse break;
                        if (std.ascii.isDigit(new_b)) {
                            try self.list.append(b);
                        } else {
                            self.char_lookahead = new_b;
                            break;
                        }
                    }
                    const ret = std.fmt.parseUnsigned(usize, self.list.items, 10) catch unreachable;
                    self.list.clearAndFree();
                    try self.tokenOut(.{ .Address = Address{ .kind = .{ .Line = ret }, .invert = try self.maybeInvert() } });
                },
                '$' => try self.tokenOut(.{ .Address = Address{ .kind = .{ .LastLine = {} }, .invert = try self.maybeInvert() } }),
                '{' => try self.tokenOut(.{ .BeginGroup = {} }),
                '}' => try self.tokenOut(.{ .EndGroup = {} }),
                ',' => try self.tokenOut(.{ .Comma = {} }),
                'p' => try self.tokenOut(.{ .Command = .print }),
                'q' => try self.tokenOut(.{ .Command = .quit }),
                'i' => {
                    try self.tokenOut(.{ .Command = .insert });
                    try self.tokenOut(.{ .Argument = try self.nextText() });
                },
                's' => {
                    const delim = try self.getDelim();
                    try self.tokenOut(Token{ .Command = .substitute });
                    try self.tokenOut(Token{ .Argument = try self.nextArg(delim) });
                    try self.tokenOut(Token{ .Argument = try self.nextArg(delim) });
                },
                'y' => {
                    const delim = try self.getDelim();
                    try self.tokenOut(Token{ .Command = .substitute });
                    try self.tokenOut(Token{ .Argument = try self.nextArg(delim) });
                    try self.tokenOut(Token{ .Argument = try self.nextArg(delim) });
                },
                ';', '\n' => try self.tokenOut(.{ .Semicolon = {} }),
                else => return error.UnknownCommand,
            };
            try self.tokenOut(.{ .Semicolon = {} });
        }
    };
}

pub fn scanner(reader: anytype, a: std.mem.Allocator, f: *TokenFifo) Scanner(@TypeOf(reader)) {
    return Scanner(@TypeOf(reader)).init(reader, a, f);
}

// TODO: refactor with unions instead of cursed optionals
pub const SedInstruction = struct {
    address1: ?Address = null,
    address2: ?Address = null,
    cmd: Command,
    group: ?[]SedInstruction = null,
    regex: ?Regex = null,
    found_first: bool = false,
    args: [5:null]?[]const u8 = [5:null]?[]const u8{ null, null, null, null, null },
};

pub const Generator = struct {
    input: *TokenFifo,
    group_level: u16 = 0,
    alloc: std.mem.Allocator,
    output: std.ArrayList(SedInstruction),

    pub fn init(alloc: std.mem.Allocator, inn: *TokenFifo) Generator {
        return Generator{ .alloc = alloc, .input = inn, .output = std.ArrayList(SedInstruction).init(alloc) };
    }

    pub fn finish(self: *Generator) []SedInstruction {
        return self.output.toOwnedSlice();
    }

    fn next(self: *Generator) !Token {
        if (self.input.readItem()) |i| {
            return i;
        } else {
            return error.EndOfFile;
        }
    }

    fn argRun(self: *Generator, count: usize, out: *[5:null]?[]const u8) !void {
        // assert(out < 5)
        var i: usize = 0;
        while (i < count) : (i += 1) {
            switch (try self.next()) {
                .Argument => |a| out[i] = a,
                else => {
                    std.log.err("Expected argument for command", .{});
                    return error.UnexpectedToken;
                },
            }
        }
    }

    fn groupLevel(self: *Generator, increase: bool) !void {
        if (!increase) {
            if (self.group_level == 0) {
                return error.UnmatchedBraces;
            }
            self.group_level -= 1;
        } else {
            self.group_level += 1;
        }
    }

    const AddressOrCommandRunError = error{ OutOfMemory, UnmatchedBraces, UnexpectedToken, IOError, EndOfFile, ExtraAddress } || Pcre.PcreError;

    fn commandRun(self: *Generator, initial: ?Token) AddressOrCommandRunError!SedInstruction {
        var ret = SedInstruction{ .cmd = .sentinel };
        switch (initial orelse try self.next()) {
            .Command => |c| {
                ret.cmd = c;
                switch (c) {
                    .print, .quit => {},
                    .substitute => {
                        try self.argRun(2, &ret.args);
                        ret.regex = try Pcre.compile(ret.args[0].?, .{});
                    },
                    .insert => try self.argRun(1, &ret.args),
                    else => {
                        std.log.err("Command not implemented", .{});
                        return error.UnexpectedToken;
                    },
                }
            },
            .BeginGroup => {
                ret.cmd = .group;
                try self.groupLevel(true);
                var command_buf = std.ArrayList(SedInstruction).init(self.alloc);
                errdefer command_buf.deinit();
                while (true) {
                    const next_t = self.next() catch return error.UnmatchedBraces;
                    switch (next_t) {
                        .EndGroup => {
                            try self.groupLevel(false);
                            break;
                        },
                        .Address, .Command, .BeginGroup => {
                            try command_buf.append((try self.addressOrCommandRun(next_t)) orelse unreachable);
                        },
                        .Semicolon => continue,
                        else => return error.UnexpectedToken,
                    }
                }
                ret.group = command_buf.toOwnedSlice();
            },
            .EndGroup => return error.UnmatchedBraces, // should only be reachable from the case above.
            else => {
                std.log.err("Expected command or {{", .{});
                return error.UnexpectedToken;
            },
        }

        // Look for a closure
        const t = self.next() catch |e| switch (e) {
            error.EndOfFile => return ret, // ok, equivalent to semicolon
            else => return e,
        };
        switch (t) {
            .Semicolon => {},
            .EndGroup => {
                // push back, see if who's calling us will handle it
                // if not, it's an unmatched brace
                try self.input.unget(&.{t});
            },
            else => {
                std.log.err("Expected newline, semicolon", .{});
                return error.UnexpectedToken;
            },
        }
        return ret;
    }

    fn addressOrCommandRun(self: *Generator, initial: ?Token) AddressOrCommandRunError!?SedInstruction {
        const t = initial orelse self.next() catch {
            if (self.group_level == 0) {
                return null;
            } else {
                return error.UnmatchedBraces;
            }
        };

        switch (t) {
            .Address => |a1| {
                const t_a1 = try self.next();
                switch (t_a1) {
                    .Comma => {
                        switch (try self.next()) {
                            .Address => |a2| {
                                var ret = try self.commandRun(null);
                                ret.address1 = a1;
                                ret.address2 = a2;
                                switch (ret.cmd) {
                                    // commands that don't accept two adresses
                                    .label, .quit => return error.ExtraAddress,
                                    else => {},
                                }
                                return ret;
                            },
                            else => {
                                std.log.err("Expected address after comma", .{});
                                return error.UnexpectedToken;
                            },
                        }
                    },
                    .Command, .BeginGroup => {
                        var ret = try self.commandRun(t_a1);
                        ret.address1 = a1;
                        switch (ret.cmd) {
                            .label => return error.ExtraAddress,
                            else => {},
                        }
                        return ret;
                    },
                    else => {
                        std.log.err("Expected comma or command after address", .{});
                        return error.UnexpectedToken;
                    },
                }
            },
            .Command, .BeginGroup => return try self.commandRun(t),
            .Semicolon => return try self.addressOrCommandRun(null), // basically skip
            else => {
                std.log.err("Expected address, command or semicolon", .{});
                return error.UnexpectedToken;
            },
        }
    }

    pub fn work(self: *Generator) !void {
        while (try self.addressOrCommandRun(null)) |i| {
            try self.output.append(i);
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
                    .insert => {
                        try self.pattern_space.insert(0, '\n');
                        try self.pattern_space.insertSlice(0, item.args[0].?);
                    },
                    .substitute => self.performReplacement(item.regex.?, item.args[1].?, false) catch continue,
                    .group => try self.runInstructions(item.group.?),
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
