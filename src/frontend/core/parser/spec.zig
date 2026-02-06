const std = @import("std");

pub fn validate(comptime Grammar: type) void {
    if (Grammar.lookahead_k != 1) {
        @compileError("LLKParser currently implements LL(1) only (Grammar.lookahead_k must be 1).");
    }
    
    if (!@hasDecl(Grammar, "tokenToTerminal")) @compileError("Grammar.tokenToTerminal(tok) is required");
    if (!@hasDecl(Grammar, "eofTerminal")) @compileError("Grammar.eofTerminal() is required");
    if (!@hasDecl(Grammar, "onShift")) @compileError("Grammar.onShift(...) is required");
    if (!@hasDecl(Grammar, "onReduce")) @compileError("Grammar.onReduce(...) is required");
    if (!@hasDecl(Grammar, "Id")) @compileError("Grammar.Id (production id enum) is required");
    if (!@hasDecl(Grammar, "productions")) @compileError("Grammar.productions is required");
    if (!@hasDecl(Grammar, "start_symbol")) @compileError("Grammar.start_symbol is required");
}
