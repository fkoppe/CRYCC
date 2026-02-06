// English comments only.

const std = @import("std");

const hir = @import("../hir/tiny_c/hir.zig").tinyc_hir;
const mir = @import("mir.zig").tinyc_mir;

pub const Error = error{
    UnexpectedNode,
    InvalidAssignTarget,
} || std.mem.Allocator.Error;

pub fn lowerModule(allocator: std.mem.Allocator, hmod: hir.Module) !mir.Module {
    var ctx = LowerCtx.init(allocator);
    defer ctx.deinit();

    for (hmod.funcs) |fid| {
        const f = hmod.func_store[@intCast(fid)];
        const mir_fid = try ctx.lowerFunc(hmod, f);
        try ctx.funcs.append(allocator, mir_fid);
    }

    return ctx.finish();
}

const BlockBuilder = struct {
    id: mir.BlockId,
    span: mir.Span,
    insts: std.ArrayListUnmanaged(mir.InstId) = .{},
    term: ?mir.Terminator = null,
};

const LowerCtx = struct {
    allocator: std.mem.Allocator,

    funcs: std.ArrayListUnmanaged(mir.FuncId) = .{},
    func_store: std.ArrayListUnmanaged(mir.Func) = .{},
    block_store: std.ArrayListUnmanaged(BlockBuilder) = .{},
    inst_store: std.ArrayListUnmanaged(mir.Inst) = .{},
    local_store: std.ArrayListUnmanaged(mir.Local) = .{},

    temp_count: mir.TempId = 0,

    fn init(allocator: std.mem.Allocator) LowerCtx {
        return .{ .allocator = allocator };
    }

    fn deinit(self: *LowerCtx) void {
        for (self.block_store.items) |*b| b.insts.deinit(self.allocator);
        self.funcs.deinit(self.allocator);
        self.func_store.deinit(self.allocator);
        self.block_store.deinit(self.allocator);
        self.inst_store.deinit(self.allocator);
        self.local_store.deinit(self.allocator);
    }

    fn finish(self: *LowerCtx) !mir.Module {
        var blocks = try self.allocator.alloc(mir.BasicBlock, self.block_store.items.len);
        for (self.block_store.items, 0..) |*b, i| {
            blocks[i] = .{
                .id = b.id,
                .span = b.span,
                .insts = try b.insts.toOwnedSlice(self.allocator),
                .term = b.term orelse .{ .ret = .{ .value = null } },
                .preds = null,
            };
        }

        return .{
            .funcs = try self.funcs.toOwnedSlice(self.allocator),
            .func_store = try self.func_store.toOwnedSlice(self.allocator),
            .block_store = blocks,
            .inst_store = try self.inst_store.toOwnedSlice(self.allocator),
            .local_store = try self.local_store.toOwnedSlice(self.allocator),
            .temp_debug = null,
        };
    }

    fn lowerFunc(self: *LowerCtx, hmod: hir.Module, f: hir.Func) Error!mir.FuncId {
        const entry = try self.createBlock(f.span);
        var func_ctx = FuncCtx.init(self.allocator, entry);
        defer func_ctx.deinit();

        const body_block = hmod.block_store[@intCast(f.body)];
        try self.lowerBlock(hmod, &func_ctx, body_block);

        // Ensure function ends in a terminator.
        if (!self.blockHasTerm(func_ctx.current_block)) {
            self.setTerm(func_ctx.current_block, .{ .ret = .{ .value = null } });
        }

        const id: mir.FuncId = @intCast(self.func_store.items.len);
        try self.func_store.append(self.allocator, .{
            .id = id,
            .span = .{ .start = f.span.start, .end = f.span.end },
            .name = f.name,
            .params = &.{},
            .ret_ty = .unit,
            .entry = entry,
            .blocks = try self.collectBlocks(),
        });
        return id;
    }

    fn collectBlocks(self: *LowerCtx) Error![]const mir.BlockId {
        const buf = try self.allocator.alloc(mir.BlockId, self.block_store.items.len);
        for (self.block_store.items, 0..) |b, i| buf[i] = b.id;
        return buf;
    }

    fn createBlock(self: *LowerCtx, span: hir.Span) Error!mir.BlockId {
        const id: mir.BlockId = @intCast(self.block_store.items.len);
        try self.block_store.append(self.allocator, .{
            .id = id,
            .span = .{ .start = span.start, .end = span.end },
        });
        return id;
    }

    fn blockHasTerm(self: *LowerCtx, bid: mir.BlockId) bool {
        return self.block_store.items[@intCast(bid)].term != null;
    }

    fn setTerm(self: *LowerCtx, bid: mir.BlockId, term: mir.Terminator) void {
        self.block_store.items[@intCast(bid)].term = term;
    }

    fn emitInst(self: *LowerCtx, bid: mir.BlockId, kind: mir.InstKind, span: hir.Span) Error!mir.InstId {
        const id: mir.InstId = @intCast(self.inst_store.items.len);
        try self.inst_store.append(self.allocator, .{
            .id = id,
            .span = .{ .start = span.start, .end = span.end },
            .kind = kind,
        });
        try self.block_store.items[@intCast(bid)].insts.append(self.allocator, id);
        return id;
    }

    fn newTemp(self: *LowerCtx) mir.TempId {
        const t = self.temp_count;
        self.temp_count += 1;
        return t;
    }

    fn getOrCreateLocal(self: *LowerCtx, func_ctx: *FuncCtx, sym: hir.SymbolId, span: hir.Span) Error!mir.LocalId {
        if (func_ctx.locals.get(sym)) |lid| return lid;

        const id: mir.LocalId = @intCast(self.local_store.items.len);
        try self.local_store.append(self.allocator, .{
            .id = id,
            .span = .{ .start = span.start, .end = span.end },
            .name = sym,
            .ty = .i64,
            .storage = .auto,
        });
        try func_ctx.locals.put(sym, id);
        return id;
    }

    fn lowerBlock(self: *LowerCtx, hmod: hir.Module, func_ctx: *FuncCtx, b: hir.Block) Error!void {
        for (b.stmts) |sid| {
            const s = hmod.stmt_store[@intCast(sid)];
            try self.lowerStmt(hmod, func_ctx, s);
            if (self.blockHasTerm(func_ctx.current_block)) return;
        }
    }

    fn lowerStmt(self: *LowerCtx, hmod: hir.Module, func_ctx: *FuncCtx, s: hir.Stmt) Error!void {
        switch (s.kind) {
            .empty => {},
            .expr => |eid| {
                _ = try self.lowerExpr(hmod, func_ctx, hmod.expr_store[@intCast(eid)]);
            },
            .block => |bid| {
                const b = hmod.block_store[@intCast(bid)];
                try self.lowerBlock(hmod, func_ctx, b);
            },
            .if_else => |n| {
                const then_block = try self.createBlock(s.span);
                const else_block = try self.createBlock(s.span);
                const cont_block = try self.createBlock(s.span);

                const cond = hmod.expr_store[@intCast(n.cond)];
                const cond_val = try self.lowerExpr(hmod, func_ctx, cond);

                self.setTerm(func_ctx.current_block, .{
                    .cbr = .{
                        .cond = cond_val.value,
                        .then_target = then_block,
                        .else_target = else_block,
                    },
                });

                func_ctx.current_block = then_block;
                try self.lowerStmt(hmod, func_ctx, hmod.stmt_store[@intCast(n.then_stmt)]);
                if (!self.blockHasTerm(func_ctx.current_block)) {
                    self.setTerm(func_ctx.current_block, .{ .br = .{ .target = cont_block } });
                }

                func_ctx.current_block = else_block;
                if (n.else_stmt) |es| {
                    try self.lowerStmt(hmod, func_ctx, hmod.stmt_store[@intCast(es)]);
                }
                if (!self.blockHasTerm(func_ctx.current_block)) {
                    self.setTerm(func_ctx.current_block, .{ .br = .{ .target = cont_block } });
                }

                func_ctx.current_block = cont_block;
            },
            .while_ => |n| {
                const cond_block = try self.createBlock(s.span);
                const body_block = try self.createBlock(s.span);
                const cont_block = try self.createBlock(s.span);

                self.setTerm(func_ctx.current_block, .{ .br = .{ .target = cond_block } });

                func_ctx.current_block = cond_block;
                const cond = hmod.expr_store[@intCast(n.cond)];
                const cond_val = try self.lowerExpr(hmod, func_ctx, cond);
                self.setTerm(func_ctx.current_block, .{
                    .cbr = .{
                        .cond = cond_val.value,
                        .then_target = body_block,
                        .else_target = cont_block,
                    },
                });

                func_ctx.current_block = body_block;
                try self.lowerStmt(hmod, func_ctx, hmod.stmt_store[@intCast(n.body_stmt)]);
                if (!self.blockHasTerm(func_ctx.current_block)) {
                    self.setTerm(func_ctx.current_block, .{ .br = .{ .target = cond_block } });
                }

                func_ctx.current_block = cont_block;
            },
            .do_while => |n| {
                const body_block = try self.createBlock(s.span);
                const cond_block = try self.createBlock(s.span);
                const cont_block = try self.createBlock(s.span);

                self.setTerm(func_ctx.current_block, .{ .br = .{ .target = body_block } });

                func_ctx.current_block = body_block;
                try self.lowerStmt(hmod, func_ctx, hmod.stmt_store[@intCast(n.body_stmt)]);
                if (!self.blockHasTerm(func_ctx.current_block)) {
                    self.setTerm(func_ctx.current_block, .{ .br = .{ .target = cond_block } });
                }

                func_ctx.current_block = cond_block;
                const cond = hmod.expr_store[@intCast(n.cond)];
                const cond_val = try self.lowerExpr(hmod, func_ctx, cond);
                self.setTerm(func_ctx.current_block, .{
                    .cbr = .{
                        .cond = cond_val.value,
                        .then_target = body_block,
                        .else_target = cont_block,
                    },
                });

                func_ctx.current_block = cont_block;
            },
        }
    }

    fn lowerExpr(self: *LowerCtx, hmod: hir.Module, func_ctx: *FuncCtx, e: hir.Expr) Error!ValueWithType {
        const span = e.span;
        switch (e.kind) {
            .ident => |id| {
                const lid = try self.getOrCreateLocal(func_ctx, id, span);
                const t = self.newTemp();
                _ = try self.emitInst(func_ctx.current_block, .{
                    .load = .{
                        .dst = t,
                        .src = .{ .base = .{ .local = lid } },
                        .ty = .i64,
                    },
                }, span);
                return .{ .value = .{ .temp = t }, .ty = .i64 };
            },
            .int_lit => |v| {
                return .{ .value = .{ .imm_int = v }, .ty = .i64 };
            },
            .binary => |b| {
                const lhs = try self.lowerExpr(hmod, func_ctx, hmod.expr_store[@intCast(b.lhs)]);
                const rhs = try self.lowerExpr(hmod, func_ctx, hmod.expr_store[@intCast(b.rhs)]);
                const t = self.newTemp();
                const op: mir.BinOp = switch (b.op) {
                    .add => .add,
                    .sub => .sub,
                    .lt => .lt,
                };
                const ty: mir.TypeRef = switch (b.op) {
                    .lt => .bool,
                    else => .i64,
                };
                _ = try self.emitInst(func_ctx.current_block, .{
                    .bin = .{
                        .dst = t,
                        .op = op,
                        .lhs = lhs.value,
                        .rhs = rhs.value,
                        .ty = ty,
                    },
                }, span);
                return .{ .value = .{ .temp = t }, .ty = ty };
            },
            .assign => |a| {
                const lhs = hmod.expr_store[@intCast(a.lhs)];
                if (lhs.kind != .ident) return error.InvalidAssignTarget;

                const rhs = try self.lowerExpr(hmod, func_ctx, hmod.expr_store[@intCast(a.rhs)]);
                const lid = try self.getOrCreateLocal(func_ctx, lhs.kind.ident, span);
                _ = try self.emitInst(func_ctx.current_block, .{
                    .store = .{
                        .dst = .{ .base = .{ .local = lid } },
                        .value = rhs.value,
                        .ty = rhs.ty,
                    },
                }, span);
                return rhs;
            },
        }
    }
};

const FuncCtx = struct {
    allocator: std.mem.Allocator,
    current_block: mir.BlockId,
    locals: std.AutoHashMap(hir.SymbolId, mir.LocalId),

    fn init(allocator: std.mem.Allocator, entry: mir.BlockId) FuncCtx {
        return .{
            .allocator = allocator,
            .current_block = entry,
            .locals = std.AutoHashMap(hir.SymbolId, mir.LocalId).init(allocator),
        };
    }

    fn deinit(self: *FuncCtx) void {
        self.locals.deinit();
    }
};

const ValueWithType = struct {
    value: mir.Value,
    ty: mir.TypeRef,
};
