pub const Instr = union(enum) {
    Mov: struct { dst: Reg, src: Operand },
    Add: struct { dst: Reg, lhs: Reg, rhs: Operand },
    Load: struct { dst: Reg, addr: Mem },
    Store: struct { src: Reg, addr: Mem },
    Jmp: Label,
    Jcc: struct { cond: Cond, target: Label },
    Call: Label,
    Ret,
};
