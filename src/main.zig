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

    var alloc = std.heap.GeneralPurposeAllocator(.{}){};
    // defer _ = alloc.deinit(); stupid dog you make me look bad
    var fifo = sed.TokenFifo.init(alloc.allocator());
    defer fifo.deinit();
    const parsers = comptime .{
        .script = clap.parsers.string,
        .file = clap.parsers.string,
    };
    var res = try clap.parse(clap.Help, &params, parsers, .{});
    var files = std.fifo.LinearFifo(fs.File, .Dynamic).init(alloc.allocator());
    defer files.deinit();
    // Script Get
    var got_script = true;
    defer res.deinit();
    for (res.args.expression) |expr, i| {
        scriptFromFixedBuffer(alloc.allocator(), &fifo, expr) catch |e| {
            std.log.err("error in expression #{d} ({s})", .{ i + 1, expr });
            return e;
        };
    } else {
        got_script = false;
    }
    for (res.args.file) |f, i| {
        var scriptFile = try fs.cwd().openFile(f, .{});
        defer scriptFile.close();
        var r = scriptFile.reader();
        var scan = sed.scanner(r, alloc.allocator(), &fifo);
        defer scan.deinit();
        scan.work() catch |e| {
            std.log.err("error in file #{}", .{i + 1});
            return e;
        };
    }

    const positionals = if (!got_script) b: {
        if (res.positionals.len > 0) {
            scriptFromFixedBuffer(alloc.allocator(), &fifo, res.positionals[0]) catch |e| {
                std.log.err("error in first argument", .{});
                return e;
            };
            got_script = true;
            break :b res.positionals[1..];
        } else {
            return clap.help(std.io.getStdErr().writer(), clap.Help, &params, .{});
        }
    } else res.positionals;

    var gen = sed.Generator.init(alloc.allocator(), &fifo);
    try gen.work();

    var stdout = io.getStdOut().writer();
    const instr = gen.finish();
    defer alloc.allocator().free(instr);

    var r = sed.runner(fs.File, fileClose, stdout, alloc.allocator(), instr, !res.args.quiet);
    defer r.deinit();

    for (positionals) |p| {
        try r.addSource(if (std.mem.eql(u8, "-", p)) io.getStdIn() else try fs.cwd().openFile(p, .{}));
    }
    if (r.remainingSources() == 0) {
        try r.addSource(io.getStdIn());
    }

    try r.run();
}
