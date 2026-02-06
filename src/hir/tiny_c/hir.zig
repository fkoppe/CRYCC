// English comments only.

const std = @import("std");
const color = @import("../../util/color.zig");

pub const tinyc_hir = struct {
    pub const Span = struct { start: u32, end: u32 };

    pub const SymbolId = u32;
    pub const FuncId = u32;
    pub const BlockId = u32;
    pub const StmtId = u32;
    pub const ExprId = u32;

    pub const LocalId = u32;
    const EnvId = u32;

    const Environment = struct {
        id: EnvId,
        parent: ?EnvId,
        bindings: std.AutoHashMap(SymbolId, LocalId),
    };

    // ===== module / function =====

    pub const Local = struct {
        id: LocalId,
        name: SymbolId,
        span: Span,
    };

    pub const Module = struct {
        funcs: []const FuncId,
        func_store: []const Func,

        block_store: []const Block,
        stmt_store: []const Stmt,
        expr_store: []const Expr,
        local_store: []const Local,
    };

    pub const Func = struct {
        id: FuncId,
        name: SymbolId,
        span: Span,
        body: BlockId,
    };

    // ===== blocks =====

    pub const Block = struct {
        id: BlockId,
        span: Span,
        stmts: []const StmtId,
    };

    // ===== statements =====

    pub const Stmt = struct {
        id: StmtId,
        span: Span,
        kind: Kind,
    };

    pub const Kind = union(enum) {
        // Empty statement: ";"
        empty: void,

        // Expression statement: "<expr> ;"
        expr: ExprId,

        // Block statement: "{ ... }"
        block: BlockId,

        // if (cond) then_stmt else else_stmt
        // Note: both branches are statements (TinyC grammar).
        if_else: IfElse,

        // while (cond) body_stmt
        while_: While,

        // do body_stmt while (cond) ;
        do_while: DoWhile,
    };

    pub const IfElse = struct {
        cond: ExprId,
        then_stmt: StmtId,
        else_stmt: ?StmtId,
    };

    pub const While = struct {
        cond: ExprId,
        body_stmt: StmtId,
    };

    pub const DoWhile = struct {
        body_stmt: StmtId,
        cond: ExprId,
    };

    // ===== expressions =====

    pub const Expr = struct {
        id: ExprId,
        span: Span,
        kind: ExprKind,

        // Optional: filled after later passes
        ty: TypeRef = .unknown,
        effects: EffectSummary = .{},
        is_lvalue: bool = false, // place-ness (identifier is true)
    };

    pub const ExprKind = union(enum) {
        // Identifier and int literal
        ident: SymbolId,
        int_lit: i64,

        // Parentheses disappear in HIR; keep span only.

        // Binary operators
        binary: Binary,

        // Assignment: "lhs = rhs"
        // In TinyC, lhs should be an identifier (by your grammar).
        assign: Assign,
    };

    pub const BinOp = enum {
        lt, // <
        add, // +
        sub, // -
    };

    pub const Binary = struct {
        op: BinOp,
        lhs: ExprId,
        rhs: ExprId,
    };

    pub const Assign = struct {
        lhs: ExprId,
        rhs: ExprId,
    };

    // ===== minimal types / effects (optional for TinyC) =====

    pub const TypeRef = union(enum) {
        unknown,
        error_ty,
        int,
        bool, // result of comparisons if you want it
        unit, // for statements
    };

    pub const EffectSummary = packed struct {
        reads_memory: bool = false,
        writes_memory: bool = false,
        has_call: bool = false, // TinyC currently has no calls, but keep extensible
        may_panic: bool = false,
        diverges: bool = false,
    };

    pub fn printModule(m: Module) void {
        color.print(.bold, "HIR Module\n", .{});
        for (m.funcs) |fid| {
            const f = m.func_store[@intCast(fid)];
            printFunc(m, f, 1);
        }
    }

    fn printFunc(m: Module, f: Func, depth: usize) void {
        printIndent(depth);
        color.print(.bold, "Func {d}\n", .{f.id});
        const b = m.block_store[@intCast(f.body)];
        printBlock(m, b, depth + 1);
    }

    fn printBlock(m: Module, b: Block, depth: usize) void {
        printIndent(depth);
        std.debug.print("Block {d}\n", .{b.id});
        for (b.stmts) |sid| {
            const s = m.stmt_store[@intCast(sid)];
            printStmt(m, s, depth + 1);
        }
    }

    fn printStmt(m: Module, s: Stmt, depth: usize) void {
        printIndent(depth);
        switch (s.kind) {
            .empty => std.debug.print("Stmt empty\n", .{}),
            .expr => |eid| blk: {
                std.debug.print("Stmt expr\n", .{});
                const e = m.expr_store[@intCast(eid)];
                printExpr(m, e, depth + 1);
                break :blk;
            },
            .block => |bid| blk: {
                color.print(.cyan, "Stmt block\n", .{});
                const b = m.block_store[@intCast(bid)];
                printBlock(m, b, depth + 1);
                break :blk;
            },
            .if_else => |n| blk: {
                color.print(.cyan, "Stmt if_else\n", .{});
                printIndent(depth + 1);
                std.debug.print("cond\n", .{});
                printExpr(m, m.expr_store[@intCast(n.cond)], depth + 2);
                printIndent(depth + 1);
                std.debug.print("then\n", .{});
                printStmt(m, m.stmt_store[@intCast(n.then_stmt)], depth + 2);
                if (n.else_stmt) |es| {
                    printIndent(depth + 1);
                    std.debug.print("else\n", .{});
                    printStmt(m, m.stmt_store[@intCast(es)], depth + 2);
                }
                break :blk;
            },
            .while_ => |n| blk: {
                color.print(.cyan, "Stmt while\n", .{});
                printIndent(depth + 1);
                std.debug.print("cond\n", .{});
                printExpr(m, m.expr_store[@intCast(n.cond)], depth + 2);
                printIndent(depth + 1);
                std.debug.print("body\n", .{});
                printStmt(m, m.stmt_store[@intCast(n.body_stmt)], depth + 2);
                break :blk;
            },
            .do_while => |n| blk: {
                color.print(.cyan, "Stmt do_while\n", .{});
                printIndent(depth + 1);
                std.debug.print("body\n", .{});
                printStmt(m, m.stmt_store[@intCast(n.body_stmt)], depth + 2);
                printIndent(depth + 1);
                std.debug.print("cond\n", .{});
                printExpr(m, m.expr_store[@intCast(n.cond)], depth + 2);
                break :blk;
            },
        }
    }

    fn printExpr(m: Module, e: Expr, depth: usize) void {
        printIndent(depth);
        switch (e.kind) {
            .ident => |id| std.debug.print("Expr ident {d}\n", .{id}),
            .int_lit => |v| std.debug.print("Expr int {d}\n", .{v}),
            .binary => |b| blk: {
                std.debug.print("Expr binary {s}\n", .{binOpText(b.op)});
                printExpr(m, m.expr_store[@intCast(b.lhs)], depth + 1);
                printExpr(m, m.expr_store[@intCast(b.rhs)], depth + 1);
                break :blk;
            },
            .assign => |a| blk: {
                std.debug.print("Expr assign\n", .{});
                printIndent(depth + 1);
                std.debug.print("lhs\n", .{});
                printExpr(m, m.expr_store[@intCast(a.lhs)], depth + 2);
                printIndent(depth + 1);
                std.debug.print("rhs\n", .{});
                printExpr(m, m.expr_store[@intCast(a.rhs)], depth + 2);
                break :blk;
            },
        }
    }

    fn binOpText(op: BinOp) []const u8 {
        return switch (op) {
            .lt => "<",
            .add => "+",
            .sub => "-",
        };
    }

    fn printIndent(depth: usize) void {
        var i: usize = 0;
        while (i < depth) : (i += 1) {
            std.debug.print("  ", .{});
        }
    }
};
