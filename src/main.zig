const std = @import("std");
const Io = std.Io;

const crycc = @import("CRYCC");

const Interner = crycc.front.Interner;
const Lexer = crycc.front.Lexer(crycc.front.tinyc.Lexicon);
const Parser = crycc.front.LLKParser(crycc.front.tinyc.Grammar);
const tinyc_hir = crycc.tinyc_hir.tinyc_hir;
const tinyc_lower = crycc.tinyc_hir_lower;
const tinyc_mir = crycc.tinyc_mir.tinyc_mir;
const tinyc_mir_lower = crycc.tinyc_mir_lower;
const mir_interpreter = crycc.mir_interpreter;

pub fn main(init: std.process.Init) !void {
    const arena: std.mem.Allocator = init.arena.allocator();

    var interner = Interner.init(arena);
    defer interner.deinit();

    const input =
        \\a = 0;
        \\if (1 < 2) { ; }
        \\while (a < 3) { a = a + 1; b = 1000; }
        \\// comment
    ;

    var lexer = Lexer.init(arena, input, &interner);

    var tokens_buf = std.ArrayListUnmanaged(Lexer.SpannedToken){};
    defer tokens_buf.deinit(arena);

    while (true) {
        const token = lexer.next() catch |err| switch (err) {
            Lexer.Error.EndOfStream => break,
            else => {
                const e = lexer.last_error.?;
                std.debug.print(
                    "Lexer Error: {s} at #{} {}-{}\n",
                    .{ e.message, e.span.source_id, e.span.start, e.span.end },
                );
                return err;
            },
        };
        try tokens_buf.append(arena, token);
        token.value.print(&interner);
    }

    std.debug.print("\n", .{});

    var parser = try Parser.init(arena, tokens_buf.items);
    defer parser.deinit();

    const ast = parser.parse() catch |err| {
        if (parser.ast_error) |e| {
            std.debug.print(
                "Parser error: {s} at {} {}-{}\n",
                .{ e.message, e.span.source_id, e.span.start, e.span.end },
            );
        } else {
            std.debug.print("Parser error: {}\n", .{err});
        }
        return err;
    };

    ast.print();

    const hir_mod = try tinyc_lower.lowerProgram(arena, ast);
    tinyc_hir.printModule(hir_mod);

    const mir_mod = try tinyc_mir_lower.lowerModule(arena, hir_mod);
    tinyc_mir.printModule(mir_mod);

    const result = mir_interpreter.runModule(arena, mir_mod, 0) catch |err| {
        std.debug.print("MIR interpreter error: {}\n", .{err});
        return err;
    };
    if (result.value) |v| {
        std.debug.print("MIR Result: {d}\n", .{v});
    } else {
        std.debug.print("MIR Result: void\n", .{});
    }
}
