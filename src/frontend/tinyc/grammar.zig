const std = @import("std");
const Lexicon = @import("lexicon.zig").Lexicon;
const ast = @import("ast.zig");
const core = @import("../core/core.zig");
const span = @import("../core/span.zig");

pub const Grammar = struct {
    pub const lookahead_k = 1;

    pub const Token = Lexicon.Token;
    pub const ASTNode = ast.ASTNode;

    pub const Nonterm = enum {
        program,

        statement,
        stmt_list,

        else_opt,

        paren_expr,

        expr,
        expr_tail,

        test_,
        test_tail,

        sum,
        sum_tail,

        term,
    };

    pub const Symbol = union(enum) {
        keyword: Lexicon.Keyword,
        symbol: Lexicon.Symbol,
        literal: Lexicon.LiteralKind,
        identifier,
        nonterm: Nonterm,
        eof,
    };

    pub fn eofTerminal() Symbol {
        return .eof;
    }

    pub fn tokenToTerminal(tok: Token) ?Symbol {
        return switch (tok) {
            .keyword => |k| .{ .keyword = k },
            .symbol => |s| .{ .symbol = s },
            .literal => |l| .{ .literal = l.kind },
            .identifier => .identifier,
        };
    }

    pub const Id = enum(u16) {
        program_stmt,

        statement_if,
        statement_while,
        statement_do,
        statement_block,
        statement_expr,
        statement_empty,

        else_opt_else,
        else_opt_empty,

        stmt_list_cons,
        stmt_list_empty,

        paren_expr,

        expr_test,
        expr_tail_assign,
        expr_tail_empty,

        test_sum,

        test_tail_lt,
        test_tail_empty,

        sum_term,

        sum_tail_plus,
        sum_tail_minus,
        sum_tail_empty,

        term_ident,
        term_int,
        term_paren,
    };

    pub const Production = struct {
        id: Id,
        left: Nonterm,
        right: []const Symbol,
    };

    fn keyword(k: Lexicon.Keyword) Symbol {
        return .{ .keyword = k };
    }

    fn symbol(s: Lexicon.Symbol) Symbol {
        return .{ .symbol = s };
    }

    fn literal(k: Lexicon.LiteralKind) Symbol {
        return .{ .literal = k };
    }

    fn identifier() Symbol {
        return .{ .identifier = {} };
    }

    fn nonTerminal(n: Nonterm) Symbol {
        return .{ .nonterm = n };
    }

    pub const start_symbol: Nonterm = .program;

    pub const productions = [_]Production{
        // <program> ::= <stmt_list>
        .{ .id = Id.program_stmt, .left = .program, .right = &.{nonTerminal(.stmt_list)} },

        // <statement> ::= "if" <paren_expr> "{" <stmt_list> "}" <else_opt>
        //               | "while" <paren_expr> <statement>
        //               | "do" <statement> "while" <paren_expr> ";"
        //               | "{" <stmt_list> "}"
        //               | <expr> ";"
        //               | ";"
        .{ .id = Id.statement_if, .left = .statement, .right = &.{ keyword(.if_), nonTerminal(.paren_expr), symbol(.l_brace), nonTerminal(.stmt_list), symbol(.r_brace), nonTerminal(.else_opt) } },
        .{ .id = Id.statement_while, .left = .statement, .right = &.{ keyword(.while_), nonTerminal(.paren_expr), nonTerminal(.statement) } },
        .{ .id = Id.statement_do, .left = .statement, .right = &.{ keyword(.do_), nonTerminal(.statement), keyword(.while_), nonTerminal(.paren_expr), symbol(.semicolon) } },
        .{ .id = Id.statement_block, .left = .statement, .right = &.{ symbol(.l_brace), nonTerminal(.stmt_list), symbol(.r_brace) } },
        .{ .id = Id.statement_expr, .left = .statement, .right = &.{ nonTerminal(.expr), symbol(.semicolon) } },
        .{ .id = Id.statement_empty, .left = .statement, .right = &.{symbol(.semicolon)} },

        // <else_opt> ::= "else" "{" <stmt_list> "}"
        //              | ε
        .{ .id = Id.else_opt_else, .left = .else_opt, .right = &.{ keyword(.else_), symbol(.l_brace), nonTerminal(.stmt_list), symbol(.r_brace) } },
        .{ .id = Id.else_opt_empty, .left = .else_opt, .right = &.{} },

        // <stmt_list> ::= <statement> <stmt_list>
        //               | ε
        .{ .id = Id.stmt_list_cons, .left = .stmt_list, .right = &.{ nonTerminal(.statement), nonTerminal(.stmt_list) } },
        .{ .id = Id.stmt_list_empty, .left = .stmt_list, .right = &.{} },

        //<paren_expr> ::= "(" <expr> ")"
        .{ .id = Id.paren_expr, .left = .paren_expr, .right = &.{ symbol(.l_paren), nonTerminal(.expr), symbol(.r_paren) } },

        // <expr> ::= <test> <expr_tail>
        .{ .id = Id.expr_test, .left = .expr, .right = &.{ nonTerminal(.test_), nonTerminal(.expr_tail) } },

        // <expr_tail> ::= "=" <expr>
        //               | ε
        .{ .id = Id.expr_tail_assign, .left = .expr_tail, .right = &.{ symbol(.equal), nonTerminal(.expr) } },
        .{ .id = Id.expr_tail_empty, .left = .expr_tail, .right = &.{} },

        //<test> ::= <sum> <test_tail>
        .{ .id = Id.test_sum, .left = .test_, .right = &.{ nonTerminal(.sum), nonTerminal(.test_tail) } },

        //<test_tail> ::= "<" <sum>
        //              | ε
        .{ .id = Id.test_tail_lt, .left = .test_tail, .right = &.{ symbol(.less_than), nonTerminal(.sum) } },
        .{ .id = Id.test_tail_empty, .left = .test_tail, .right = &.{} },

        //<sum> ::= <term> <sum_tail>
        .{ .id = Id.sum_term, .left = .sum, .right = &.{ nonTerminal(.term), nonTerminal(.sum_tail) } },

        //<sum_tail> ::= "+" <term> <sum_tail>
        //             | "-" <term> <sum_tail>
        //             | ε
        .{ .id = Id.sum_tail_plus, .left = .sum_tail, .right = &.{ symbol(.plus), nonTerminal(.term), nonTerminal(.sum_tail) } },
        .{ .id = Id.sum_tail_minus, .left = .sum_tail, .right = &.{ symbol(.minus), nonTerminal(.term), nonTerminal(.sum_tail) } },
        .{ .id = Id.sum_tail_empty, .left = .sum_tail, .right = &.{} },

        //<term> ::= <id>
        //         | <int>
        //         | <paren_expr>
        .{ .id = Id.term_ident, .left = .term, .right = &.{identifier()} },
        .{ .id = Id.term_int, .left = .term, .right = &.{literal(.int)} },
        .{ .id = Id.term_paren, .left = .term, .right = &.{nonTerminal(.paren_expr)} },
    };

    pub fn onShift(
        allocator: std.mem.Allocator,
        term: Symbol,
        token: span.Spanned(Token),
    ) !?*ASTNode {
        switch (term) {
            .identifier => {
                const n = try allocator.create(ASTNode);
                n.* = .{ .ident = .{ .identifier = token.value.identifier } };
                return n;
            },
            .literal => |k| {
                if (k != .int) return error.UnexpectedToken;

                const value = try std.fmt.parseInt(i64, token.value.literal.lexeme, 10);
                const n = try allocator.create(ASTNode);
                n.* = .{ .integer = .{ .value = value } };
                return n;
            },
            else => {
                // keywords, symbols, parentheses, semicolon → no AST node
                return @as(?*ASTNode, null);
            },
        }
    }

    pub fn symbolProducesNode(sym: Symbol) bool {
        return switch (sym) {
            .identifier => true,
            .literal => |k| k == .int,
            .nonterm => true,
            else => false,
        };
    }

    pub fn onReduce(
        allocator: std.mem.Allocator,
        production_id: Id,
        children: []const *ASTNode,
    ) !?*ASTNode {
        const blockFromStmtList = struct {
            fn apply(allocator_: std.mem.Allocator, list_node: *ASTNode) !*ASTNode {
                if (list_node.* != .stmt_list) return error.UnexpectedToken;
                const list = list_node.stmt_list.children;
                const buf = try allocator_.alloc(*ASTNode, list.len);
                std.mem.copyForwards(*ASTNode, buf, list);

                const n = try allocator_.create(ASTNode);
                n.* = .{ .block = .{ .children = buf } };
                return n;
            }
        }.apply;

        const isEmpty = struct {
            fn check(n: *ASTNode) bool {
                return switch (n.*) {
                    .empty => true,
                    else => false,
                };
            }
        }.check;

        const foldSum = struct {
            fn apply(allocator_: std.mem.Allocator, left: *ASTNode, tail: *ASTNode) !*ASTNode {
                var acc = left;
                var cur = tail;
                while (true) {
                    switch (cur.*) {
                        .empty => return acc,
                        .add => |node| {
                            const n = try allocator_.create(ASTNode);
                            n.* = .{ .add = .{ .left = acc, .right = node.left } };
                            acc = n;
                            cur = node.right;
                        },
                        .sub => |node| {
                            const n = try allocator_.create(ASTNode);
                            n.* = .{ .sub = .{ .left = acc, .right = node.left } };
                            acc = n;
                            cur = node.right;
                        },
                        else => return error.UnexpectedToken,
                    }
                }
            }
        }.apply;

        switch (production_id) {
            .program_stmt => {
                if (children[0].* != .stmt_list) return error.UnexpectedToken;
                const list = children[0].stmt_list.children;

                const buf = try allocator.alloc(*ASTNode, list.len);
                std.mem.copyForwards(*ASTNode, buf, list);

                const n = try allocator.create(ASTNode);
                n.* = .{ .program = .{ .children = buf } };
                return n;
            },

            // ===== statements =====

            .statement_if => {
                const then_block = try blockFromStmtList(allocator, children[1]);

                const n = try allocator.create(ASTNode);
                n.* = .{
                    .if_else = .{
                        .cond = children[0],
                        .then = then_block,
                        .else_ = children[2], // ?*ASTNode
                    },
                };
                return n;
            },

            .statement_while => {
                const n = try allocator.create(ASTNode);
                n.* = .{
                    .while_ = .{
                        .cond = children[0],
                        .body = children[1],
                    },
                };
                return n;
            },

            .statement_do => {
                const n = try allocator.create(ASTNode);
                n.* = .{
                    .do_while = .{
                        .body = children[0],
                        .cond = children[1],
                    },
                };
                return n;
            },

            .statement_block => {
                return blockFromStmtList(allocator, children[0]);
            },

            .statement_expr => {
                const n = try allocator.create(ASTNode);
                n.* = .{ .expr_stmt = .{ .expression = children[0] } };
                return n;
            },

            .statement_empty => {
                const n = try allocator.create(ASTNode);
                n.* = .{ .empty = .{} };
                return n;
            },

            // ===== else =====

            .else_opt_else => {
                return blockFromStmtList(allocator, children[0]);
            },

            .else_opt_empty => {
                const n = try allocator.create(ASTNode);
                n.* = .{ .empty = .{} };
                return n;
            },

            // ===== stmt_list =====

            .stmt_list_cons => {
                if (children[1].* != .stmt_list) return error.UnexpectedToken;
                const tail = children[1].stmt_list.children;
                const buf = try allocator.alloc(*ASTNode, tail.len + 1);
                buf[0] = children[0];
                std.mem.copyForwards(*ASTNode, buf[1..], tail);

                const n = try allocator.create(ASTNode);
                n.* = .{ .stmt_list = .{ .children = buf } };
                return n;
            },

            .stmt_list_empty => {
                const n = try allocator.create(ASTNode);
                n.* = .{ .stmt_list = .{ .children = &.{} } };
                return n;
            },

            // ===== expressions =====

            .expr_tail_empty,
            .test_tail_empty,
            .sum_tail_empty,
            => {
                const n = try allocator.create(ASTNode);
                n.* = .{ .empty = .{} };
                return n;
            },

            .expr_test => {
                if (isEmpty(children[1])) return children[0];

                if (children[0].* != .ident) {
                    return error.UnexpectedToken;
                }

                const n = try allocator.create(ASTNode);
                n.* = .{
                    .assign = .{
                        .left = children[0],
                        .value = children[1],
                    },
                };
                return n;
            },

            .expr_tail_assign => {
                return children[0];
            },

            .test_sum => {
                if (isEmpty(children[1])) return children[0];

                const n = try allocator.create(ASTNode);
                n.* = .{
                    .less_than = .{
                        .left = children[0],
                        .right = children[1],
                    },
                };
                return n;
            },

            .test_tail_lt => {
                return children[0];
            },

            .sum_term => {
                if (isEmpty(children[1])) return children[0];
                return foldSum(allocator, children[0], children[1]);
            },

            .sum_tail_plus => {
                const n = try allocator.create(ASTNode);
                n.* = .{
                    .add = .{
                        .left = children[0],
                        .right = children[1],
                    },
                };
                return n;
            },

            .sum_tail_minus => {
                const n = try allocator.create(ASTNode);
                n.* = .{
                    .sub = .{
                        .left = children[0],
                        .right = children[1],
                    },
                };
                return n;
            },

            // ===== pure forwarding =====

            .paren_expr,
            .term_paren,
            .term_ident,
            .term_int,
            => return children[0],

            // All cases handled.
        }
    }
};
