const EffectSet = @import("effects.zig").EffectSet;

pub const BlockId = u32;

pub const Terminator = union(enum) {
    Return,
    Branch: struct {
        cond: BlockId,
        then_block: BlockId,
        else_block: BlockId,
    },
    Jump: BlockId,
};

pub const Block = struct {
    id: BlockId,
    effects: EffectSet,
    terminator: Terminator,
};
