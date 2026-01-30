pub const Effect = enum {
    Pure,      // no observable effects
    Read,      // reads memory
    Write,     // writes memory
    IO,        // external effects
};

pub const EffectSet = struct {
    read: bool = false,
    write: bool = false,
    io: bool = false,

    pub fn pure() EffectSet {
        return .{};
    }

    pub fn hasSideEffects(self: EffectSet) bool {
        return self.write or self.io;
    }
};
