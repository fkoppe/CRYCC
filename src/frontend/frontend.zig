pub const crylang = @import("crylang/crylang.zig");
pub const tinyc = @import("tinyc/tinyc.zig");

const core = @import("core/core.zig");

pub const Interner = @import("core/interner.zig").Interner;
pub const Lexer = @import("core/lexer/lexer.zig").Lexer;
pub const LLKParser = @import("core/parser/llk.zig").LLKParser;

//const Languages = enum {
//    tinyc,
//    own_language,
//};
//
//const ASTNode = union(Languages) {
//
//};
