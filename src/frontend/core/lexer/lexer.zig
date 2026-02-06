const std = @import("std");

const spec = @import("spec.zig");
const span = @import("../span.zig");

const util = @import("../../../util/helper.zig");

const Interner = @import("../interner.zig").Interner;

fn initStaticStringMap(comptime V: type, comptime table: anytype) std.StaticStringMap(V) {
    var kvs: [table.len]struct { []const u8, V } = undefined;
    for (table, 0..) |entry, i| {
        kvs[i] = .{ entry.text, entry.tag };
    }
    return std.StaticStringMap(V).initComptime(kvs[0..]);
}

pub fn Lexer(comptime Lexicon: type) type {
    comptime {
        spec.validate(Lexicon);
    }

    const keyword_map = initStaticStringMap(Lexicon.Keyword, Lexicon.keywords);

    return struct {
        const Self = @This();

        allocator: std.mem.Allocator,
        input: []const u8,
        interner: *Interner,
        pos: usize = 0,
        last_error: ?struct { span: span.Span, message: []const u8 } = null,

        pub const SpannedToken = span.Spanned(Lexicon.Token);

        pub const Error = error{
            EndOfStream,
            InvalidCharacter,
            UnterminatedLiteral,
        } || std.mem.Allocator.Error;

        pub fn init(allocator: std.mem.Allocator, input: []const u8, interner: *Interner) Self {
            return .{ .allocator = allocator, .input = input, .interner = interner };
        }

        pub fn next(self: *Self) !SpannedToken {
            self.skipWhitespaceAndComments();

            if (self.reached_eof()) return Error.EndOfStream;

            const start = self.pos;

            if (try self.matchSymbol(start)) |tok| return tok;
            if (try self.matchIdentifierOrKeyword(start)) |tok| return tok;
            if (try self.matchLiteral(start)) |tok| return tok;

            self.last_error = .{
                .span = .{ .source_id = 0, .start = start, .end = self.pos + 1 },
                .message = "invalid character",
            };
            return Error.InvalidCharacter;
        }

        fn advance(self: *Self, n: usize) void {
            self.pos += n;
        }

        fn reached_eof(self: *Self) bool {
            return self.pos >= self.input.len;
        }

        fn skipWhitespaceAndComments(self: *Self) void {
            while (true) {
                if (self.reached_eof()) break;
                const c = self.input[self.pos];
                if (Lexicon.isWhitespace(c)) {
                    self.pos += 1;
                    continue;
                }

                if (@hasDecl(Lexicon, "comment")) {
                    if (Lexicon.comment.line) |line_prefix| {
                        if (self.startsWith(line_prefix)) {
                            self.advance(line_prefix.len);
                            while (!self.reached_eof()) : (self.pos += 1) {
                                const cc = self.input[self.pos];
                                if (cc == '\n' or cc == '\r') break;
                            }
                            continue;
                        }
                    }
                }

                break;
            }
        }

        fn matchSymbol(self: *Self, start_pos: usize) !?SpannedToken {
            if (self.reached_eof()) return null;

            const remaining = self.input.len - self.pos;
            var best_len: usize = 0;
            var best_sym: ?Lexicon.Symbol = null;

            for (Lexicon.symbols) |entry| {
                const text = entry.text;
                if (text.len == 0) continue;
                if (text.len > remaining) continue;
                if (text.len < best_len) continue;
                if (self.startsWith(text)) {
                    best_len = text.len;
                    best_sym = entry.tag;
                }
            }

            if (best_sym) |sym| {
                self.advance(best_len);
                return SpannedToken.init(start_pos, self.pos, Lexicon.Token{ .symbol = sym });
            }
            return null;
        }

        fn matchIdentifierOrKeyword(self: *Self, start_pos: usize) !?SpannedToken {
            if (self.reached_eof()) return null;
            const c = self.input[self.pos];
            if (!Lexicon.isIdentStart(c)) return null;

            const start = self.pos;
            self.pos += 1;
            while (!self.reached_eof()) {
                const cc = self.input[self.pos];
                if (!Lexicon.isIdentContinue(cc)) break;
                self.pos += 1;
            }
            const word = self.input[start..self.pos];

            if (keyword_map.get(word)) |kw| {
                return SpannedToken.init(start_pos, self.pos, Lexicon.Token{ .keyword = kw });
            }
            const id = try self.interner.intern(word);
            return SpannedToken.init(start_pos, self.pos, Lexicon.Token{ .identifier = id });
        }

        fn matchLiteral(self: *Self, start_pos: usize) !?SpannedToken {
            var best_rule_index: ?usize = null;
            var best_prefix_len: usize = 0;

            for (Lexicon.literals, 0..) |lit, i| {
                const starts = switch (lit.start) {
                    .any_of => |s| s,
                };
                for (starts) |pref| {
                    if (pref.len == 0) continue;
                    if (pref.len < best_prefix_len) continue;
                    if (self.startsWith(pref)) {
                        best_prefix_len = pref.len;
                        best_rule_index = i;
                    }
                }
            }

            // Handle empty-prefix number rules last so they don't shadow everything.
            if (best_rule_index == null) {
                for (Lexicon.literals, 0..) |lit, i| {
                    const starts = switch (lit.start) {
                        .any_of => |s| s,
                    };
                    for (starts) |pref| {
                        if (pref.len != 0) continue;
                        best_rule_index = i;
                        best_prefix_len = 0;
                        break;
                    }
                    if (best_rule_index != null) break;
                }
            }

            const rule_index = best_rule_index orelse return null;
            const rule = Lexicon.literals[rule_index];

            switch (rule.body) {
                .number => |num| {
                    // Require first char to be digit for empty-prefix numbers.
                    if (best_prefix_len == 0) {
                        if (self.reached_eof() or !util.isDigitBase(self.input[self.pos], num.base)) {
                            self.pos = start_pos;
                            return null;
                        }
                    }

                    self.advance(best_prefix_len);
                    var seen_digit = false;
                    while (!self.reached_eof()) {
                        const c = self.input[self.pos];
                        if (num.digit_sep) |sep| {
                            if (c == sep) {
                                self.pos += 1;
                                continue;
                            }
                        }
                        if (util.isDigitBase(c, num.base)) {
                            seen_digit = true;
                            self.pos += 1;
                            continue;
                        }
                        break;
                    }
                    if (!seen_digit) {
                        self.pos = start_pos;
                        return null;
                    }

                    const lexeme = self.input[start_pos..self.pos];
                    return SpannedToken.init(start_pos, self.pos, Lexicon.Token{ .literal = .{ .kind = rule.kind, .lexeme = lexeme } });
                },
                .exact => {
                    const lexeme = self.input[start_pos .. start_pos + best_prefix_len];
                    self.pos = start_pos + best_prefix_len;
                    return SpannedToken.init(start_pos, self.pos, Lexicon.Token{ .literal = .{ .kind = rule.kind, .lexeme = lexeme } });
                },
                .delimited => |delim| {
                    if (best_prefix_len == 0) return null;

                    self.advance(best_prefix_len);
                    const end_seq = switch (delim.end) {
                        .same_as_start => self.input[start_pos .. start_pos + best_prefix_len],
                        .fixed => |f| f,
                    };

                    while (!self.reached_eof()) {
                        const c = self.input[self.pos];

                        if (delim.escape == .backslash and c == '\\') {
                            self.pos += 1;
                            if (!self.reached_eof()) self.pos += 1;
                            continue;
                        }

                        if (delim.escape == .doubled_end and end_seq.len == 1 and c == end_seq[0]) {
                            if (self.pos + 1 < self.input.len and self.input[self.pos + 1] == end_seq[0]) {
                                self.pos += 2;
                                continue;
                            }
                        }

                        if (self.startsWith(end_seq)) {
                            self.pos += end_seq.len;
                            const lexeme = self.input[start_pos..self.pos];
                            return SpannedToken.init(start_pos, self.pos, Lexicon.Token{ .literal = .{ .kind = rule.kind, .lexeme = lexeme } });
                        }

                        self.pos += 1;
                    }

                    self.last_error = .{
                        .span = .{ .source_id = 0, .start = start_pos, .end = self.pos },
                        .message = "unterminated literal",
                    };
                    return Error.UnterminatedLiteral;
                },
                else => {
                    return null;
                },
            }
        }

        fn startsWith(self: *Self, needle: []const u8) bool {
            if (needle.len == 0) return true;
            if (self.pos + needle.len > self.input.len) return false;
            return std.mem.eql(u8, self.input[self.pos .. self.pos + needle.len], needle);
        }
    };
}
