const sed = @import("./parse.zig");
const SedError = sed.ScanError || sed.GenerationError;
const std = @import("std");
const clap = @import("clap");
const fs = std.fs;
const io = std.io;
const mem = std.mem;

fn fileClose(f: fs.File) void {
    f.close();
}

fn scriptFromFixedBuffer(a: mem.Allocator, f: *sed.TokenFifo, src: []const u8) !void {
    var script = io.fixedBufferStream(src).reader();
    var scan = sed.scanner(script, a, f);
    defer scan.deinit();
    return scan.work();
}

pub fn main() !void {
    const params = comptime clap.parseParamsComptime(
        \\-n, --quiet  Whether to automatically print the transformed lines.
        \\-e, --expression <script>...  Command line argument that is evalued as a sed script. Can be specified multiple times.
        \\-f, --file <file>...        Files where scripts are sourced from.
        \\<file>...
        \\
    );

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var alloc = gpa.allocator();
    // defer _ = alloc.deinit(); stupid dog you make me look bad
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

    const positionals = if (script_from_pos) res.positionals[1..] else res.positionals;

    var stdout = io.getStdOut().writer();
    var gen = sed.Generator.init(alloc, script);
    const instr = gen.work() catch |e| {
        var line: usize = 0;
        var line_start: usize = 0;
        for (gen.buf[0..gen.index]) |c, i| {
            if (c == '\n') {
                line += 1;
                line_start = i + 1;
            }
        }
        std.log.err("{} at line {}, pos {}.", .{ e, line, gen.index - line_start });
        return e;
    };

    var r = sed.runner(fs.File, fileClose, stdout, alloc, instr, !res.args.quiet);
    defer r.deinit();

    for (positionals) |p| {
        try r.addSource(if (std.mem.eql(u8, "-", p)) io.getStdIn() else try fs.cwd().openFile(p, .{}));
    }
    if (r.remainingSources() == 0) {
        try r.addSource(io.getStdIn());
    }

    try r.run();
}
