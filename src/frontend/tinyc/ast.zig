const std = @import("std");
const color = @import("../../util/color.zig");
const core = @import("../core/core.zig");

pub const ASTNode = union(enum) {
    empty: Empty,
    program: Program,
    block: Block,
    stmt_list: StmtList,
    if_: If,
    if_else: IfElse,
    while_: While,
    do_while: DoWhile,
    expr_stmt: ExprStatement,
    assign: Assign,
    less_than: LessThan,
    add: Add,
    sub: Sub,
    ident: Ident,
    integer: Integer,

    pub fn print(self: ASTNode) void {
        self.printIndented(0);
    }

    pub fn printIndented(self: ASTNode, depth: usize) void {
        switch (self) {
            inline else => |node| node.printIndented(depth),
        }
    }
};

fn printIndent(depth: usize) void {
    var i: usize = 0;
    while (i < depth) : (i += 1) {
        std.debug.print("  ", .{});
    }
}

pub const Program = struct {
    children: []const *ASTNode,

    pub fn printIndented(self: Program, depth: usize) void {
        printIndent(depth);
        std.debug.print("Program\n", .{});
        for (self.children) |child| {
            child.printIndented(depth + 1);
        }
    }
};

pub const StmtList = struct {
    children: []const *ASTNode,

    pub fn printIndented(self: StmtList, depth: usize) void {
        printIndent(depth);
        std.debug.print("StmtList\n", .{});
        for (self.children) |child| {
            child.printIndented(depth + 1);
        }
    }
};

pub const Empty = struct {
    pub fn printIndented(self: Empty, depth: usize) void {
        _ = self;
        printIndent(depth);
        std.debug.print("Empty\n", .{});
    }
};

pub const Block = struct {
    children: []const *ASTNode,

    pub fn printIndented(self: Block, depth: usize) void {
        printIndent(depth);
        std.debug.print("Block\n", .{});
        for (self.children) |child| {
            child.printIndented(depth + 1);
        }
    }
};

pub const If = struct {
    cond: *ASTNode,
    then: *ASTNode,

    pub fn printIndented(self: If, depth: usize) void {
        printIndent(depth);
        color.print(.cyan, "If\n", .{});
        printIndent(depth + 1);
        std.debug.print("cond\n", .{});
        self.cond.printIndented(depth + 2);
        printIndent(depth + 1);
        std.debug.print("then\n", .{});
        self.then.printIndented(depth + 2);
    }
};

pub const IfElse = struct {
    cond: *ASTNode,
    then: *ASTNode,
    else_: *ASTNode,

    pub fn printIndented(self: IfElse, depth: usize) void {
        printIndent(depth);
        color.print(.cyan, "IfElse\n", .{});
        printIndent(depth + 1);
        std.debug.print("cond\n", .{});
        self.cond.printIndented(depth + 2);
        printIndent(depth + 1);
        std.debug.print("then\n", .{});
        self.then.printIndented(depth + 2);
        printIndent(depth + 1);
        std.debug.print("else\n", .{});
        self.else_.printIndented(depth + 2);
    }
};

pub const While = struct {
    cond: *ASTNode,
    body: *ASTNode,

    pub fn printIndented(self: While, depth: usize) void {
        printIndent(depth);
        color.print(.cyan, "While\n", .{});
        printIndent(depth + 1);
        std.debug.print("cond\n", .{});
        self.cond.printIndented(depth + 2);
        printIndent(depth + 1);
        std.debug.print("body\n", .{});
        self.body.printIndented(depth + 2);
    }
};

pub const DoWhile = struct {
    body: *ASTNode,
    cond: *ASTNode,

    pub fn printIndented(self: DoWhile, depth: usize) void {
        printIndent(depth);
        color.print(.cyan, "DoWhile\n", .{});
        printIndent(depth + 1);
        std.debug.print("body\n", .{});
        self.body.printIndented(depth + 2);
        printIndent(depth + 1);
        std.debug.print("cond\n", .{});
        self.cond.printIndented(depth + 2);
    }
};

pub const ExprStatement = struct {
    expression: *ASTNode,

    pub fn printIndented(self: ExprStatement, depth: usize) void {
        printIndent(depth);
        std.debug.print("ExprStatement\n", .{});
        self.expression.printIndented(depth + 1);
    }
};

pub const Assign = struct {
    left: *ASTNode,
    value: *ASTNode,

    pub fn printIndented(self: Assign, depth: usize) void {
        printIndent(depth);
        std.debug.print("Assign\n", .{});
        printIndent(depth + 1);
        std.debug.print("left\n", .{});
        self.left.printIndented(depth + 2);
        printIndent(depth + 1);
        std.debug.print("value\n", .{});
        self.value.printIndented(depth + 2);
    }
};

pub const LessThan = struct {
    left: *ASTNode,
    right: *ASTNode,

    pub fn printIndented(self: LessThan, depth: usize) void {
        printIndent(depth);
        std.debug.print("LessThan\n", .{});
        self.left.printIndented(depth + 1);
        self.right.printIndented(depth + 1);
    }
};

pub const Add = struct {
    left: *ASTNode,
    right: *ASTNode,

    pub fn printIndented(self: Add, depth: usize) void {
        printIndent(depth);
        std.debug.print("Add\n", .{});
        self.left.printIndented(depth + 1);
        self.right.printIndented(depth + 1);
    }
};

pub const Sub = struct {
    left: *ASTNode,
    right: *ASTNode,

    pub fn printIndented(self: Sub, depth: usize) void {
        printIndent(depth);
        std.debug.print("Sub\n", .{});
        self.left.printIndented(depth + 1);
        self.right.printIndented(depth + 1);
    }
};

pub const Ident = struct {
    identifier: core.Identifier,

    pub fn printIndented(self: Ident, depth: usize) void {
        printIndent(depth);
        std.debug.print("Ident {d}\n", .{self.identifier});
    }
};

pub const Integer = struct {
    value: i64,

    pub fn printIndented(self: Integer, depth: usize) void {
        printIndent(depth);
        std.debug.print("Integer {d}\n", .{self.value});
    }
};
