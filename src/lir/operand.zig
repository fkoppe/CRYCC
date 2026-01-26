pub const Reg = enum {
    v0,
    v1,
    v2, // virtual registers
};

pub const Operand = union(enum) {
    Reg: Reg,
    Imm: i64,
};

pub const Mem = struct {
    base: Reg,
    offset: i32,
};
