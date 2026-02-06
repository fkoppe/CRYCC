const std = @import("std");
const Io = std.Io;

pub const front = @import("frontend/frontend.zig");
pub const Lexer = front.Lexer;
pub const color = @import("util/color.zig");
pub const tinyc_hir = @import("hir/tiny_c/hir.zig");
pub const tinyc_hir_lower = @import("hir/tiny_c/from_ast.zig");
pub const tinyc_mir = @import("mir/mir.zig");
pub const tinyc_mir_lower = @import("mir/from_hir.zig");
pub const mir_interpreter = @import("backend/interpreter/interpreter.zig");

/// This is a documentation comment to explain the `printAnotherMessage` function below.
///
/// Accepting an `Io.Writer` instance is a handy way to write reusable code.
pub fn printAnotherMessage(writer: *Io.Writer) Io.Writer.Error!void {
    try writer.print("Run `zig build test` to run the tests.\n", .{});
}

pub fn add(a: i32, b: i32) i32 {
    return a + b;
}

test "basic add functionality" {
    try std.testing.expect(add(3, 7) == 10);
}
