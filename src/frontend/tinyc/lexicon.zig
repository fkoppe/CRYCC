const std = @import("std");
const core = @import("../core/core.zig");
const Interner = @import("../core/interner.zig").Interner;

pub const Lexicon = struct {

    // ############################## TOKEN ##############################
    pub const Token = union(enum) {
        symbol: Symbol,
        literal: Literal,
        keyword: Keyword,
        identifier: core.Identifier,

        pub fn print(self: Token, interner: *Interner) void {
            switch (self) {
                .keyword => |k| std.debug.print("{s} ", .{k.text()}),
                .symbol => |s| std.debug.print("{s} ", .{s.text()}),
                .identifier => |id| {
                    std.debug.print("#{s} ", .{interner.get(id) orelse @panic("unknown identifier id")});
                },
                .literal => |lit| {
                    std.debug.print("{s} ", .{lit.lexeme});
                },
            }
        }
    };

    // ############################## KEYWORD ##############################

    pub const Keyword = enum {
        do_,
        while_,
        if_,
        else_,

        fn text(self: Keyword) []const u8 {
            for (keywords) |k| {
                if (k.tag == self) return k.text;
            }
            unreachable;
        }
    };

    pub const keywords = [_]struct {
        text: []const u8,
        tag: Keyword,
    }{
        .{ .text = "do", .tag = .do_ },
        .{ .text = "while", .tag = .while_ },
        .{ .text = "if", .tag = .if_ },
        .{ .text = "else", .tag = .else_ },
    };

    // ############################## SYMBOL ##############################
    pub const Symbol = enum {
        l_brace,
        r_brace,
        l_paren,
        r_paren,
        plus,
        minus,
        less_than,
        semicolon,
        equal,

        fn text(self: Symbol) []const u8 {
            for (symbols) |s| {
                if (s.tag == self) return s.text;
            }
            unreachable;
        }
    };

    pub const symbols = [_]struct {
        text: []const u8,
        tag: Symbol,
    }{
        .{ .text = "{", .tag = .l_brace },
        .{ .text = "}", .tag = .r_brace },
        .{ .text = "(", .tag = .l_paren },
        .{ .text = ")", .tag = .r_paren },
        .{ .text = "+", .tag = .plus },
        .{ .text = "-", .tag = .minus },
        .{ .text = "<", .tag = .less_than },
        .{ .text = ";", .tag = .semicolon },
        .{ .text = "=", .tag = .equal },
    };

    pub fn isIdentStart(c: u8) bool {
        return std.ascii.isAlphabetic(c) or c == '_';
    }

    pub fn isIdentContinue(c: u8) bool {
        return std.ascii.isAlphanumeric(c) or c == '_';
    }

    pub fn isWhitespace(c: u8) bool {
        return switch (c) {
            ' ', '\t', '\n', '\r' => true,
            else => false,
        };
    }

    // ############################## COMMENT ##############################
    pub const comment = core.CommentSpec{
        .line = "//",
    };

    // ############################## LITERAL ##############################
    pub const LiteralKind = enum {
        int,
        float,
        string,
        char,
        bool,
        null,

        fn text(self: LiteralKind) []const u8 {
            for (literals) |l| {
                if (l.kind == self) return l.text;
            }
            unreachable;
        }
    };

    pub const Literal = struct {
        kind: LiteralKind,
        lexeme: []const u8,
    };

    pub const literals = [_]core.LiteralSpec(LiteralKind){
        .{
            .text = "bool",
            .kind = .bool,
            .start = .{ .any_of = &.{ "true", "false" } },
            .body = .{ .exact = {} },
        },

        .{
            .text = "decimal integer",
            .kind = .int,
            .body = .{
                .number = .{ .base = 10 },
            },
        },

        .{
            .text = "hex integer",
            .kind = .int,
            .start = .{ .any_of = &.{ "0x", "0X" } },
            .body = .{
                .number = .{ .base = 16 },
            },
        },

        .{
            .text = "binary integer",
            .kind = .int,
            .start = .{ .any_of = &.{ "0b", "0B" } },
            .body = .{
                .number = .{ .base = 2 },
            },
        },

        .{
            .text = "string",
            .kind = .string,
            .start = .{ .any_of = &.{"\""} },
            .body = .{
                .delimited = .{
                    .end = .same_as_start,
                    .escape = .backslash,
                },
            },
        },

        .{
            .text = "char",
            .kind = .char,
            .start = .{ .any_of = &.{"'"} },
            .body = .{
                .delimited = .{
                    .end = .same_as_start,
                    .escape = .backslash,
                },
            },
        },
    };
};
