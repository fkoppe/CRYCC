const Type = @import("types.zig").Type;

pub const Value = union(enum) {
    Bool: bool,
    Int: i64,
    Null,
};

pub const TypedValue = struct {
    type: *Type,
    value: Value,
};
