pub const IntWidth = enum {
    i8, i16, i32, i64,
};

pub const Type = union(enum) {
    Void,
    Bool,
    Int: IntWidth,
    Ptr: *Type,
};
