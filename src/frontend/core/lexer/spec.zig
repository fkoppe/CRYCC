const std = @import("std");

pub fn validate(comptime Lexicon: type) void {
    requireDecl(Lexicon, "Token");
    requireDecl(Lexicon, "keywords");
    requireDecl(Lexicon, "symbols");
    requireDecl(Lexicon, "Keyword");
    requireDecl(Lexicon, "Symbol");

    requireFn(Lexicon, "isIdentStart", fn (u8) bool);
    requireFn(Lexicon, "isIdentContinue", fn (u8) bool);
    requireFn(Lexicon, "isWhitespace", fn (u8) bool);
}

fn requireDecl(comptime Lexicon: type, comptime name: []const u8) void {
    if (!@hasDecl(Lexicon, name)) {
        @compileError("Lexer spec error: missing declaration `" ++ name ++ "`");
    }
}

fn requireFn(comptime Lexicon: type, comptime name: []const u8, comptime T: type) void {
    if (!@hasDecl(Lexicon, name))
        @compileError("Lexer spec error: missing function `" ++ name ++ "`");

    if (@TypeOf(@field(Lexicon, name)) != T)
        @compileError("Lexer spec error: `" ++ name ++ "` must have type `" ++
            @typeName(T) ++ "`");
}
