// English comments only.

const std = @import("std");

const mir = @import("../../mir/mir.zig").tinyc_mir;

pub const Error = error{
    UnsupportedInstruction,
    UnsupportedCall,
    UnsupportedValue,
    UnsupportedPlace,
    MissingTerminator,
    InvalidBlock,
    InvalidFunc,
    StepLimit,
} || std.mem.Allocator.Error;

pub const Result = struct {
    value: ?i64,
};

pub fn runModule(
    allocator: std.mem.Allocator,
    module: mir.Module,
    func_id: mir.FuncId,
) Error!Result {
    if (func_id >= module.func_store.len) return error.InvalidFunc;
    const func = module.func_store[@intCast(func_id)];

    var state = try ExecState.init(allocator, module);
    defer state.deinit(allocator);

    const ret_val = try execFunc(&state, module, func);
    return .{ .value = ret_val };
}

const ExecState = struct {
    allocator: std.mem.Allocator,
    temps: []i64,
    locals: []i64,
    stack: std.ArrayListUnmanaged(i64) = .{},

    fn init(allocator: std.mem.Allocator, module: mir.Module) !ExecState {
        const temp_cap = maxTemp(module);
        const temps = try allocator.alloc(i64, temp_cap);
        const locals = try allocator.alloc(i64, module.local_store.len);
        @memset(temps, 0);
        @memset(locals, 0);
        return .{ .allocator = allocator, .temps = temps, .locals = locals };
    }

    fn deinit(self: *ExecState, allocator: std.mem.Allocator) void {
        self.stack.deinit(allocator);
        allocator.free(self.temps);
        allocator.free(self.locals);
    }
};

fn maxTemp(module: mir.Module) usize {
    var max_id: usize = 0;
    for (module.inst_store) |inst| {
        switch (inst.kind) {
            .bin => |b| max_id = @max(max_id, @as(usize, b.dst) + 1),
            .un => |u| max_id = @max(max_id, @as(usize, u.dst) + 1),
            .load => |l| max_id = @max(max_id, @as(usize, l.dst) + 1),
            .addr_of => |a| max_id = @max(max_id, @as(usize, a.dst) + 1),
            .phi => |p| max_id = @max(max_id, @as(usize, p.dst) + 1),
            .store, .call, .nop => {},
        }
    }
    return max_id;
}

fn execFunc(state: *ExecState, module: mir.Module, func: mir.Func) Error!?i64 {
    const max_steps: u64 = 1_000_000;
    var steps: u64 = 0;

    var current: mir.BlockId = func.entry;
    while (true) {
        steps += 1;
        if (steps > max_steps) return error.StepLimit;

        if (current >= module.block_store.len) return error.InvalidBlock;
        const block = module.block_store[@intCast(current)];

        for (block.insts) |iid| {
            steps += 1;
            if (steps > max_steps) return error.StepLimit;
            const inst = module.inst_store[@intCast(iid)];
            try execInst(state, module, inst);
        }

        switch (block.term) {
            .br => |b| {
                current = b.target;
            },
            .cbr => |c| {
                try pushValue(state, c.cond);
                const cond = try popValue(state);
                current = if (cond != 0) c.then_target else c.else_target;
            },
            .ret => |r| {
                // TinyC debug: print a..z locals once on termination.
                printAZ(state);
                if (r.value) |v| {
                    try pushValue(state, v);
                    return try popValue(state);
                }
                return null;
            },
            .unreachable_ => return null,
        }
    }
}

fn printAZ(state: *ExecState) void {
    const max = @min(@as(usize, 26), state.locals.len);
    var i: usize = 0;
    while (i < max) : (i += 1) {
        const ch: u8 = @intCast('a' + i);
        if (i == 0) {
            std.debug.print("{c}={d}", .{ ch, state.locals[i] });
        } else {
            std.debug.print(" {c}={d}", .{ ch, state.locals[i] });
        }
    }
    if (max > 0) {
        std.debug.print("\n", .{});
    }
}

fn execInst(state: *ExecState, module: mir.Module, inst: mir.Inst) Error!void {
    _ = module;
    switch (inst.kind) {
        .bin => |b| {
            try pushValue(state, b.lhs);
            try pushValue(state, b.rhs);
            const rhs = try popValue(state);
            const lhs = try popValue(state);
            const res: i64 = switch (b.op) {
                .add => lhs + rhs,
                .sub => lhs - rhs,
                .lt => if (lhs < rhs) 1 else 0,
            };
            state.temps[@intCast(b.dst)] = res;
        },
        .un => return error.UnsupportedInstruction,
        .load => |l| {
            const val = try loadPlace(state, l.src);
            try state.stack.append(state.allocator, val);
            const out = try popValue(state);
            state.temps[@intCast(l.dst)] = out;
        },
        .store => |s| {
            try pushValue(state, s.value);
            const val = try popValue(state);
            try storePlace(state, s.dst, val);
        },
        .addr_of => return error.UnsupportedInstruction,
        .call => |c| {
            // Builtin: callee imm_int 0 == print(arg0)
            if (c.args.len != 1) return error.UnsupportedCall;
            if (c.callee != .imm_int or c.callee.imm_int != 0) {
                return error.UnsupportedCall;
            }
            const arg0 = try readValue(state, c.args[0]);
            std.debug.print("{d}\n", .{arg0});
            if (c.dst) |dst| {
                state.temps[@intCast(dst)] = 0;
            }
        },
        .phi => return error.UnsupportedInstruction,
        .nop => {},
    }
}

fn pushValue(state: *ExecState, v: mir.Value) Error!void {
    const val: i64 = try readValue(state, v);
    try state.stack.append(state.allocator, val);
}

fn popValue(state: *ExecState) Error!i64 {
    if (state.stack.items.len == 0) return error.UnsupportedValue;
    return state.stack.pop().?;
}

fn readValue(state: *ExecState, v: mir.Value) Error!i64 {
    return switch (v) {
        .temp => |t| state.temps[@intCast(t)],
        .imm_int => |i| i,
        .imm_bool => |b| if (b) 1 else 0,
    };
}

fn loadPlace(state: *ExecState, p: mir.Place) Error!i64 {
    return switch (p.base) {
        .local => |l| state.locals[@intCast(l)],
        .temp_addr => return error.UnsupportedPlace,
    };
}

fn storePlace(state: *ExecState, p: mir.Place, value: i64) Error!void {
    switch (p.base) {
        .local => |l| state.locals[@intCast(l)] = value,
        .temp_addr => return error.UnsupportedPlace,
    }
}
