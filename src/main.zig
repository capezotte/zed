const sed = @import("./parse.zig");
const std = @import("std");
const clap = @import("clap");
const fs = std.fs;
const io = std.io;
const mem = std.mem;

fn fileClose(f: fs.File) void {
    f.close();
}

pub fn main() !void {
    const params = comptime clap.parseParamsComptime(
        \\-n, --quiet  Whether to automatically print the transformed lines.
        \\-e, --expression <script>...  Command line argument that is evalued as a sed script. Can be specified multiple times.
        \\-f, --file <file>...          Files where scripts are sourced from.
        \\--posix                       Make N and n quit when no new lines are available.
        \\<file>...
        \\
    );

    var gpa = std.heap.GeneralPurposeAllocator(.{ .stack_trace_frames = 10 }){};
    var alloc = gpa.allocator();
    defer _ = gpa.deinit(); // stupid dog you make me look bad
    const parsers = comptime .{
        .script = clap.parsers.string,
        .file = clap.parsers.string,
    };
    var res = try clap.parse(clap.Help, &params, parsers, .{});
    defer res.deinit();
    var files = std.fifo.LinearFifo(fs.File, .Dynamic).init(alloc);
    defer files.deinit();
    // Script Get
    var script_from_pos = false;
    var script = b: {
        var script = std.ArrayList(u8).init(alloc);
        for (res.args.expression) |expr| {
            try script.appendSlice(expr);
            try script.append('\n');
        }
        for (res.args.file) |f| {
            var script_file = try fs.cwd().openFile(f, .{});
            defer script_file.close();
            try script_file.reader().readAllArrayList(&script, 1 << 16);
            try script.append('\n');
        }
        if (script.items.len == 0 and res.positionals.len > 0) {
            try script.appendSlice(res.positionals[0]);
            try script.append('\n');
            script_from_pos = true;
        }
        if (script.items.len == 0) {
            try clap.help(std.io.getStdErr().writer(), clap.Help, &params, .{});
            return;
        }
        break :b script.toOwnedSlice();
    };
    defer alloc.free(script);

    const positionals = if (script_from_pos) res.positionals[1..] else res.positionals;

    var stdout = io.getStdOut().writer();
    var gen = b: {
        var error_location: usize = undefined;
        var gen = sed.Generator.init(alloc, script, &error_location) catch |e| {
            var line: usize = 0;
            var line_start: usize = 0;
            for (script[0..error_location]) |c, i| {
                if (c == '\n') {
                    line += 1;
                    line_start = i + 1;
                }
            }
            const line_offset = error_location - line_start;
            std.log.err("{} at line {}, pos {}.", .{ e, line, line_offset });
            std.log.err("{s}", .{mem.sliceTo(script[line_start..], '\n')});
            std.log.err("{[empty]s: >[len]}^", .{ .empty = "", .len = line_offset });
            return e;
        };
        break :b gen;
    };
    defer gen.deinit();

    var r = sed.runner(fs.File, fileClose, stdout, alloc, gen.program(), .{ .print = !res.args.quiet, .posix_quit = res.args.posix });
    defer r.deinit();

    for (positionals) |p| {
        try r.addSource(if (std.mem.eql(u8, "-", p)) io.getStdIn() else try fs.cwd().openFile(p, .{}));
    }
    if (r.remainingSources() == 0) {
        try r.addSource(io.getStdIn());
    }

    try r.run();
}
