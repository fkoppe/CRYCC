const std = @import("std");

pub const Color = enum {
    reset,
    bold,
    dim,
    red,
    green,
    yellow,
    blue,
    magenta,
    cyan,
    gray,
};

pub fn code(c: Color) []const u8 {
    return switch (c) {
        .reset => "\x1b[0m",
        .bold => "\x1b[1m",
        .dim => "\x1b[2m",
        .red => "\x1b[31m",
        .green => "\x1b[32m",
        .yellow => "\x1b[33m",
        .blue => "\x1b[34m",
        .magenta => "\x1b[35m",
        .cyan => "\x1b[36m",
        .gray => "\x1b[90m",
    };
}

pub fn print(color: Color, comptime fmt: []const u8, args: anytype) void {
    std.debug.print("{s}", .{code(color)});
    std.debug.print(fmt, args);
    std.debug.print("{s}", .{code(.reset)});
}
