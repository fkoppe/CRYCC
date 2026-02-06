// English comments only.

const std = @import("std");
const color = @import("../util/color.zig");

pub const tinyc_mir = struct {
    // ===== ids =====

    pub const Span = struct { start: u32, end: u32 };

    pub const FuncId = u32;
    pub const BlockId = u32;
    pub const InstId = u32;

    pub const LocalId = u32; // HIR locals lowered to MIR locals
    pub const TempId = u32;  // SSA-ish temporaries

    pub const SymbolId = u32; // optional (for debug names only)

    // ===== module / function =====

    pub const Module = struct {
        funcs: []const FuncId,
        func_store: []const Func,

        block_store: []const BasicBlock,
        inst_store: []const Inst,
        local_store: []const Local,

        // Optional debug tables:
        temp_debug: ?[]const TempDebug = null,
    };

    pub const Func = struct {
        id: FuncId,
        span: Span,
        name: SymbolId,

        params: []const LocalId,
        ret_ty: TypeRef,

        entry: BlockId,
        blocks: []const BlockId,
    };

    pub const Local = struct {
        id: LocalId,
        span: Span,
        name: SymbolId, // optional/debug

        ty: TypeRef,
        // Storage class stays target-neutral: stack/global decided later or by passes.
        storage: StorageClass = .auto,
    };

    pub const StorageClass = enum {
        auto,   // compiler decides
        stack,  // force stack slot (useful for address-taken)
        global, // for globals if you add them
    };

    pub const TempDebug = struct {
        id: TempId,
        span: Span,
        ty: TypeRef,
        // Optional: originating HIR ExprId etc.
    };

    // ===== CFG =====

    pub const BasicBlock = struct {
        id: BlockId,
        span: Span,

        insts: []const InstId,
        term: Terminator,

        // Optional: predecessor list for analyses (can be computed)
        preds: ?[]const BlockId = null,
    };

    pub const Terminator = union(enum) {
        // Fallthrough is explicit in MIR: you always end blocks with a terminator.
        br: Br,
        cbr: Cbr,
        ret: Ret,
        unreachable_: void,
    };

    pub const Br = struct { target: BlockId };

    pub const Cbr = struct {
        cond: Value,       // bool-like
        then_target: BlockId,
        else_target: BlockId,
    };

    pub const Ret = struct { value: ?Value };

    // ===== values / places =====

    // Value is something you can compute with and pass around.
    pub const Value = union(enum) {
        temp: TempId,
        imm_int: i64,
        imm_bool: bool,
        // Extend: float, null/unit, function refs
    };

    // Place is an assignable location (lvalue). Keep it generic.
    pub const Place = struct {
        base: PlaceBase,
        proj: []const Projection = &.{},
    };

    pub const PlaceBase = union(enum) {
        local: LocalId,
        temp_addr: TempId, // address in a temp (after addr_of lowering)
        // Extend: global id
    };

    pub const Projection = union(enum) {
        // Extend as needed: field index, array index, deref, etc.
        deref,
        index: Value,
    };

    // ===== instructions =====

    pub const Inst = struct {
        id: InstId,
        span: Span,
        kind: InstKind,
    };

    pub const InstKind = union(enum) {
        // t = op(a, b)
        bin: Bin,

        // t = op(a)
        un: Un,

        // t = load place
        load: Load,

        // store place, value
        store: Store,

        // t = addr_of place
        addr_of: AddrOf,

        // t = call callee(args...)
        call: Call,

        // t = phi(preds...)
        // Optional but strongly recommended for SSA-based optimizations.
        phi: Phi,

        // No-op marker for debug
        nop: void,
    };

    pub const BinOp = enum {
        add,
        sub,
        lt,  // produces bool
        // Extend: mul/div, comparisons, bitops
    };

    pub const UnOp = enum {
        neg,
        not,
        // Extend
    };

    pub const Bin = struct {
        dst: TempId,
        op: BinOp,
        lhs: Value,
        rhs: Value,
        ty: TypeRef,
    };

    pub const Un = struct {
        dst: TempId,
        op: UnOp,
        src: Value,
        ty: TypeRef,
    };

    pub const Load = struct {
        dst: TempId,
        src: Place,
        ty: TypeRef,
        // Volatile/atomic flags can be added later.
    };

    pub const Store = struct {
        dst: Place,
        value: Value,
        ty: TypeRef,
    };

    pub const AddrOf = struct {
        dst: TempId,
        src: Place,
        ty: TypeRef, // pointer type
        mut: bool,
    };

    pub const Call = struct {
        dst: ?TempId,     // null for void/unit
        callee: Value,    // function pointer or symbol lowered earlier
        args: []const Value,
        ret_ty: TypeRef,
        // Calling convention remains target-neutral here; ABI resolved in LIR.
        may_throw: bool = false, // placeholder if you later add exceptions
    };

    pub const Phi = struct {
        dst: TempId,
        ty: TypeRef,
        incomings: []const Incoming,
    };

    pub const Incoming = struct {
        pred: BlockId,
        value: Value,
    };

    // ===== types (target-neutral) =====

    pub const TypeRef = union(enum) {
        unknown,
        error_ty,
        unit,
        bool,
        i32,
        i64,
        ptr: Ptr,
        // Extend: aggregates
    };

    pub const Ptr = struct {
        child: *const TypeRef,
        mut: bool,
    };

    pub fn printModule(m: Module) void {
        color.print(.bold, "MIR Module\n", .{});
        for (m.funcs) |fid| {
            const f = m.func_store[@intCast(fid)];
            printFunc(m, f, 1);
        }
    }

    fn printFunc(m: Module, f: Func, depth: usize) void {
        printIndent(depth);
        color.print(.bold, "Func {d}\n", .{f.id});
        for (f.blocks) |bid| {
            const b = m.block_store[@intCast(bid)];
            printBlock(m, b, depth + 1);
        }
    }

    fn printBlock(m: Module, b: BasicBlock, depth: usize) void {
        printIndent(depth);
        std.debug.print("Block {d}\n", .{b.id});
        for (b.insts) |iid| {
            const i = m.inst_store[@intCast(iid)];
            printInst(m, i, depth + 1);
        }
        printTerm(b.term, depth + 1);
    }

    fn printInst(m: Module, i: Inst, depth: usize) void {
        _ = m;
        printIndent(depth);
        switch (i.kind) {
            .bin => |b| blk: {
                std.debug.print("bin t{d} = ", .{b.dst});
                printValue(b.lhs);
                std.debug.print(" {s} ", .{binOpText(b.op)});
                printValue(b.rhs);
                std.debug.print("\n", .{});
                break :blk;
            },
            .un => |u| blk: {
                std.debug.print("un t{d} = {s} ", .{ u.dst, unOpText(u.op) });
                printValue(u.src);
                std.debug.print("\n", .{});
                break :blk;
            },
            .load => |l| blk: {
                std.debug.print("load t{d} <- ", .{l.dst});
                printPlace(l.src);
                std.debug.print("\n", .{});
                break :blk;
            },
            .store => |s| blk: {
                std.debug.print("store ", .{});
                printPlace(s.dst);
                std.debug.print(" <- ", .{});
                printValue(s.value);
                std.debug.print("\n", .{});
                break :blk;
            },
            .addr_of => |a| blk: {
                std.debug.print("addr_of t{d} <- ", .{a.dst});
                printPlace(a.src);
                std.debug.print("\n", .{});
                break :blk;
            },
            .call => |c| blk: {
                std.debug.print("call ", .{});
                printValue(c.callee);
                std.debug.print(" (args={d})\n", .{c.args.len});
                break :blk;
            },
            .phi => |p| std.debug.print("phi t{d} (n={d})\n", .{ p.dst, p.incomings.len }),
            .nop => std.debug.print("nop\n", .{}),
        }
    }

    fn printTerm(t: Terminator, depth: usize) void {
        printIndent(depth);
        switch (t) {
            .br => |b| blk: {
                color.print(.cyan, "br", .{});
                std.debug.print(" B{d}\n", .{b.target});
                break :blk;
            },
            .cbr => |c| blk: {
                color.print(.cyan, "cbr", .{});
                std.debug.print(" ", .{});
                printValue(c.cond);
                std.debug.print(" ? B{d} : B{d}\n", .{ c.then_target, c.else_target });
                break :blk;
            },
            .ret => |r| blk: {
                color.print(.cyan, "ret", .{});
                if (r.value) |v| {
                    std.debug.print(" ", .{});
                    printValue(v);
                }
                std.debug.print("\n", .{});
                break :blk;
            },
            .unreachable_ => std.debug.print("unreachable\n", .{}),
        }
    }

    fn printValue(v: Value) void {
        switch (v) {
            .temp => |t| std.debug.print("t{d}", .{t}),
            .imm_int => |i| std.debug.print("{d}", .{i}),
            .imm_bool => |b| std.debug.print("{s}", .{if (b) "true" else "false"}),
        }
    }

    fn printPlace(p: Place) void {
        switch (p.base) {
            .local => |l| std.debug.print("l{d}", .{l}),
            .temp_addr => |t| std.debug.print("t{d}_addr", .{t}),
        }
    }

    fn binOpText(op: BinOp) []const u8 {
        return switch (op) {
            .add => "+",
            .sub => "-",
            .lt => "<",
        };
    }

    fn unOpText(op: UnOp) []const u8 {
        return switch (op) {
            .neg => "-",
            .not => "!",
        };
    }

    fn printIndent(depth: usize) void {
        var i: usize = 0;
        while (i < depth) : (i += 1) {
            std.debug.print("  ", .{});
        }
    }
};
