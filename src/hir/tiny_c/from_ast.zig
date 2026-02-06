// English comments only.

const std = @import("std");

const ast = @import("../../frontend/tinyc/ast.zig");
const hir = @import("hir.zig").tinyc_hir;

pub const Error = error{
    UnexpectedNode,
} || std.mem.Allocator.Error;

pub fn lowerProgram(allocator: std.mem.Allocator, root: *ast.ASTNode) !hir.Module {
    var ctx = LowerCtx.init(allocator);
    defer ctx.deinit();

    switch (root.*) {
        .program => |p| {
            var stmt_ids = std.ArrayListUnmanaged(hir.StmtId){};
            defer stmt_ids.deinit(allocator);

            for (p.children) |stmt_node| {
                try stmt_ids.append(allocator, try ctx.lowerStmt(stmt_node));
            }

            const entry_block: hir.BlockId = @intCast(ctx.block_store.items.len);
            try ctx.block_store.append(allocator, .{
                .id = entry_block,
                .span = .{ .start = 0, .end = 0 },
                .stmts = try stmt_ids.toOwnedSlice(allocator),
            });

            const main_fn: hir.FuncId = @intCast(ctx.func_store.items.len);
            try ctx.func_store.append(allocator, .{
                .id = main_fn,
                .name = 0,
                .span = .{ .start = 0, .end = 0 },
                .body = entry_block,
            });
            try ctx.funcs.append(allocator, main_fn);
        },
        else => return error.UnexpectedNode,
    }

    return ctx.finish();
}

const LowerCtx = struct {
    allocator: std.mem.Allocator,

    funcs: std.ArrayListUnmanaged(hir.FuncId) = .{},
    func_store: std.ArrayListUnmanaged(hir.Func) = .{},

    block_store: std.ArrayListUnmanaged(hir.Block) = .{},
    stmt_store: std.ArrayListUnmanaged(hir.Stmt) = .{},
    expr_store: std.ArrayListUnmanaged(hir.Expr) = .{},
    local_store: std.ArrayListUnmanaged(hir.Local) = .{},

    fn init(allocator: std.mem.Allocator) LowerCtx {
        return .{ .allocator = allocator };
    }

    fn deinit(self: *LowerCtx) void {
        self.funcs.deinit(self.allocator);
        self.func_store.deinit(self.allocator);
        self.block_store.deinit(self.allocator);
        self.stmt_store.deinit(self.allocator);
        self.expr_store.deinit(self.allocator);
        self.local_store.deinit(self.allocator);
    }

    fn finish(self: *LowerCtx) !hir.Module {
        return .{
            .funcs = try self.funcs.toOwnedSlice(self.allocator),
            .func_store = try self.func_store.toOwnedSlice(self.allocator),
            .block_store = try self.block_store.toOwnedSlice(self.allocator),
            .stmt_store = try self.stmt_store.toOwnedSlice(self.allocator),
            .expr_store = try self.expr_store.toOwnedSlice(self.allocator),
            .local_store = try self.local_store.toOwnedSlice(self.allocator),
        };
    }

    fn lowerStmt(self: *LowerCtx, node: *ast.ASTNode) Error!hir.StmtId {
        const span = hir.Span{ .start = 0, .end = 0 };

        const kind = switch (node.*) {
            .empty => hir.Kind{ .empty = {} },
            .expr_stmt => |s| hir.Kind{ .expr = try self.lowerExpr(s.expression) },
            .block => |b| hir.Kind{ .block = try self.lowerBlock(b) },
            .if_ => |n| hir.Kind{ .if_else = .{
                .cond = try self.lowerExpr(n.cond),
                .then_stmt = try self.lowerStmt(n.then),
                .else_stmt = null,
            } },
            .if_else => |n| hir.Kind{ .if_else = .{
                .cond = try self.lowerExpr(n.cond),
                .then_stmt = try self.lowerStmt(n.then),
                .else_stmt = try self.lowerStmt(n.else_),
            } },
            .while_ => |n| hir.Kind{ .while_ = .{
                .cond = try self.lowerExpr(n.cond),
                .body_stmt = try self.lowerStmt(n.body),
            } },
            .do_while => |n| hir.Kind{ .do_while = .{
                .body_stmt = try self.lowerStmt(n.body),
                .cond = try self.lowerExpr(n.cond),
            } },
            else => return error.UnexpectedNode,
        };

        const id: hir.StmtId = @intCast(self.stmt_store.items.len);
        try self.stmt_store.append(self.allocator, .{ .id = id, .span = span, .kind = kind });
        return id;
    }

    fn lowerBlock(self: *LowerCtx, b: ast.Block) Error!hir.BlockId {
        var stmt_ids = std.ArrayListUnmanaged(hir.StmtId){};
        defer stmt_ids.deinit(self.allocator);

        for (b.children) |stmt_node| {
            try stmt_ids.append(self.allocator, try self.lowerStmt(stmt_node));
        }

        const id: hir.BlockId = @intCast(self.block_store.items.len);
        try self.block_store.append(self.allocator, .{
            .id = id,
            .span = .{ .start = 0, .end = 0 },
            .stmts = try stmt_ids.toOwnedSlice(self.allocator),
        });
        return id;
    }

    fn lowerExpr(self: *LowerCtx, node: *ast.ASTNode) Error!hir.ExprId {
        const span = hir.Span{ .start = 0, .end = 0 };

        const kind = switch (node.*) {
            .ident => |n| hir.ExprKind{ .ident = n.identifier },
            .integer => |n| hir.ExprKind{ .int_lit = n.value },
            .add => |n| hir.ExprKind{ .binary = .{
                .op = .add,
                .lhs = try self.lowerExpr(n.left),
                .rhs = try self.lowerExpr(n.right),
            } },
            .sub => |n| hir.ExprKind{ .binary = .{
                .op = .sub,
                .lhs = try self.lowerExpr(n.left),
                .rhs = try self.lowerExpr(n.right),
            } },
            .less_than => |n| hir.ExprKind{ .binary = .{
                .op = .lt,
                .lhs = try self.lowerExpr(n.left),
                .rhs = try self.lowerExpr(n.right),
            } },
            .assign => |n| hir.ExprKind{ .assign = .{
                .lhs = try self.lowerExpr(n.left),
                .rhs = try self.lowerExpr(n.value),
            } },
            else => return error.UnexpectedNode,
        };

        const id: hir.ExprId = @intCast(self.expr_store.items.len);
        try self.expr_store.append(self.allocator, .{
            .id = id,
            .span = span,
            .kind = kind,
            .ty = .unknown,
            .effects = .{},
            .is_lvalue = node.* == .ident,
        });
        return id;
    }
};
