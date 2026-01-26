pub const Backend = union(enum) {
    c,
    llvm,
    native_x86_64,
    native_aarch64,
};