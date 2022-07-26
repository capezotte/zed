const std = @import("std");
const mem = std.mem;

pub fn LineFeeder(comptime Source: type) type {
    return struct {
        fifo: InFifoType,
        current: ?Source = null,
        alloc: mem.Allocator,
        lookahead: ?[]const u8 = null,
        cleanup: fn (Source) void,
        _line: usize = 0,
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

        pub fn line(self: *Self) usize {
            return self._line;
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
                self._line += 1;
                return l;
            } else {
                return null;
            }
        }
    };
}
