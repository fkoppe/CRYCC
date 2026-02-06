const std = @import("std");

const spec = @import("spec.zig");
const span = @import("../span.zig");

pub fn LLKParser(comptime Grammar: type) type {
    comptime {
        spec.validate(Grammar);
    }

    const StackItem = union(enum) {
        symbol: Grammar.Symbol,
        reduce: struct {
            production_id: Grammar.Id,
            arity: usize,
        },
    };

    // ---------- Symbol hashing for hash maps ----------
    const SymbolCtx = struct {
        const Self = @This();

        pub fn hash(_: Self, s: Grammar.Symbol) u64 {
            // Hash by bytes; OK if Symbol has no pointers with unstable content identity.
            var h: u64 = 1469598103934665603;
            const b = std.mem.asBytes(&s);
            for (b) |x| {
                h ^= x;
                h *%= 1099511628211;
            }
            return h;
        }
        pub fn eql(_: Self, a: Grammar.Symbol, b: Grammar.Symbol) bool {
            return std.meta.eql(a, b);
        }
    };

    const TermSet = std.HashMap(Grammar.Symbol, void, SymbolCtx, std.hash_map.default_max_load_percentage);

    const FirstSet = struct {
        eps: bool = false,
        terms: TermSet,

        const Self = @This();

        fn init(allocator: std.mem.Allocator) Self {
            return .{ .terms = TermSet.init(allocator) };
        }
        fn deinit(self: *Self) void {
            self.terms.deinit();
        }
        fn addTerm(self: *Self, t: Grammar.Symbol) !bool {
            const gop = try self.terms.getOrPut(t);
            if (gop.found_existing) return false;
            gop.value_ptr.* = {};
            return true;
        }
    };

    const FollowSet = TermSet;

    const Prod = Grammar.Production;

    // Parse table row: terminal -> production id
    const Row = std.HashMap(Grammar.Symbol, Grammar.Id, SymbolCtx, std.hash_map.default_max_load_percentage);

    return struct {
        const Self = @This();

        allocator: std.mem.Allocator,
        tokens: []const span.Spanned(Grammar.Token),
        pos: usize = 0,

        // prod_by_id[id] = production index
        prod_index_by_id: []usize,

        // table[nonterm_index] : map(terminal -> prod_id)
        table: []Row,

        ast_error: ?struct { message: []const u8, span: span.Span } = null,

        pub const Error = error{
            UnexpectedEof,
            UnexpectedToken,
            NoProduction,
            GrammarNotLL1,
        } || std.mem.Allocator.Error;

        pub fn init(
            allocator: std.mem.Allocator,
            tokens: []const span.Spanned(Grammar.Token),
        ) !Self {
            var self: Self = .{
                .allocator = allocator,
                .tokens = tokens,
                .prod_index_by_id = undefined,
                .table = undefined,
            };

            try self.buildProdIndexById();
            try self.buildParseTableLL1();
            return self;
        }

        pub fn deinit(self: *Self) void {
            for (self.table) |*r| r.deinit();
            self.allocator.free(self.table);
            self.allocator.free(self.prod_index_by_id);
        }

        // ----------------- parse -----------------

        pub fn parse(self: *Self) !*Grammar.ASTNode {
            var stack = std.array_list.Managed(StackItem).init(self.allocator);
            defer stack.deinit();

            var node_stack = std.array_list.Managed(*Grammar.ASTNode).init(self.allocator);
            defer node_stack.deinit();

            // LL(1) typically uses EOF sentinel to force completion.
            try stack.append(.{ .symbol = Grammar.eofTerminal() });
            try stack.append(.{ .symbol = .{ .nonterm = Grammar.start_symbol } });

            while (stack.items.len > 0) {
                const item = stack.pop() orelse break;

                switch (item) {
                    .reduce => |r| {
                        // Pop r.arity nodes, preserve order
                        var tmp = try self.allocator.alloc(*Grammar.ASTNode, r.arity);
                        defer self.allocator.free(tmp);

                        var i: usize = r.arity;
                        while (i > 0) {
                            i -= 1;
                            if (node_stack.items.len == 0) return error.UnexpectedToken;
                            tmp[i] = node_stack.pop() orelse break;
                        }

                        const p = self.productionById(r.production_id);
                        const n = try Grammar.onReduce(
                            self.allocator,
                            p.id,
                            tmp,
                        ) orelse continue; //TODO should this be an error?
                        // Grammar.Node is expected to be nullable-capable if needed
                        try node_stack.append(n);
                    },

                    .symbol => |sym| {
                        switch (sym) {
                            .nonterm => |A| {
                                const la = self.peekTerminal();
                                const row = &self.table[@intFromEnum(A)];
                                const pid = row.get(la) orelse {
                                    self.ast_error = .{
                                        .message = "no production",
                                        .span = self.nextTerminalSpan(),
                                    };
                                    return error.NoProduction;
                                };

                                const p = self.productionById(pid);

                                // push reduce frame + RHS reversed (ignore ε where right.len==0)
                                try stack.append(.{
                                    .reduce = .{ .production_id = p.id, .arity = self.rhsNodeArity(p.right) },
                                });

                                var j: usize = p.right.len;
                                while (j > 0) {
                                    j -= 1;
                                    try stack.append(.{ .symbol = p.right[j] });
                                }
                            },
                            .eof => {
                                if (!std.meta.eql(self.peekTerminal(), Grammar.eofTerminal())) {
                                    self.ast_error = .{
                                        .message = "unexpected token",
                                        .span = self.nextTerminalSpan(),
                                    };
                                    return error.UnexpectedToken;
                                }
                                self.pos = self.tokens.len;
                            },
                            else => {
                                // terminal: must match next terminal token
                                const st = try self.consumeMatching(sym);
                                const leaf = try Grammar.onShift(self.allocator, sym, st);
                                if (leaf) |n| {
                                    try node_stack.append(n);
                                }
                            },
                        }
                    },
                }
            }

            if (node_stack.items.len != 1) {
                self.ast_error = .{
                    .message = "unexpected token",
                    .span = self.endOfInputSpan(),
                };
                return error.UnexpectedToken;
            }
            return node_stack.items[0];
        }

        // ----------------- token stream -----------------

        fn peekTerminal(self: *Self) Grammar.Symbol {
            var i = self.pos;
            while (i < self.tokens.len) : (i += 1) {
                const st = self.tokens[i];
                if (Grammar.tokenToTerminal(st.value)) |sym| {
                    return sym;
                }
            }
            return Grammar.eofTerminal();
        }

        fn endOfInputSpan(self: *Self) span.Span {
            if (self.tokens.len == 0) {
                return .{
                    .source_id = 0,
                    .start = 0,
                    .end = 0,
                };
            }

            const last = self.tokens[self.tokens.len - 1].span;
            return .{
                .source_id = last.source_id,
                .start = last.end,
                .end = last.end,
            };
        }

        fn nextTerminalSpan(self: *Self) span.Span {
            var i = self.pos;
            while (i < self.tokens.len) : (i += 1) {
                const st = self.tokens[i];
                if (Grammar.tokenToTerminal(st.value)) |_| {
                    return st.span;
                }
            }
            return self.endOfInputSpan();
        }

        fn consumeMatching(
            self: *Self,
            expected: Grammar.Symbol,
        ) !span.Spanned(Grammar.Token) {
            var i = self.pos;

            while (i < self.tokens.len) : (i += 1) {
                const st = self.tokens[i];
                if (Grammar.tokenToTerminal(st.value)) |sym| {
                    if (!std.meta.eql(sym, expected)) {
                        self.ast_error = .{
                            .message = "unexpected token",
                            .span = st.span,
                        };
                        return error.UnexpectedToken;
                    }

                    self.pos = i + 1;
                    return st;
                }
            }

            // No token left, but a terminal was expected → error
            self.ast_error = .{
                .message = "unexpected eof",
                .span = self.endOfInputSpan(),
            };
            return error.UnexpectedEof;
        }

        // Count how many RHS symbols will produce nodes at runtime.
        // Convention used here: count all nonterminals, and optionally count terminals
        // that the grammar declares as node-producing via symbolProducesNode().
        fn rhsNodeArity(self: *Self, rhs: []const Grammar.Symbol) usize {
            _ = self;
            var c: usize = 0;
            for (rhs) |s| {
                switch (s) {
                    .nonterm => c += 1,
                    else => {
                        if (@hasDecl(Grammar, "symbolProducesNode") and Grammar.symbolProducesNode(s)) {
                            c += 1;
                        }
                    },
                }
            }
            return c;
        }

        // ----------------- productions by id -----------------

        fn buildProdIndexById(self: *Self) !void {
            const id_count = @typeInfo(Grammar.Id).@"enum".fields.len;
            self.prod_index_by_id = try self.allocator.alloc(usize, id_count);

            // init with invalid marker
            for (self.prod_index_by_id) |*x| x.* = std.math.maxInt(usize);

            for (Grammar.productions, 0..) |p, idx| {
                const id_i = @intFromEnum(p.id);
                if (self.prod_index_by_id[id_i] != std.math.maxInt(usize)) {
                    return error.GrammarNotLL1; // duplicate id
                }
                self.prod_index_by_id[id_i] = idx;
            }

            // ensure all ids exist exactly once
            for (self.prod_index_by_id) |x| {
                if (x == std.math.maxInt(usize)) return error.GrammarNotLL1;
            }
        }

        fn productionById(self: *Self, id: Grammar.Id) Prod {
            return Grammar.productions[self.prod_index_by_id[@intFromEnum(id)]];
        }

        // ----------------- FIRST/FOLLOW + parse table (LL1) -----------------

        fn buildParseTableLL1(self: *Self) !void {
            const nt_count = @typeInfo(Grammar.Nonterm).@"enum".fields.len;

            // FIRST and FOLLOW
            var first = try self.allocator.alloc(FirstSet, nt_count);
            defer {
                for (first) |*fs| fs.deinit();
                self.allocator.free(first);
            }
            for (first) |*fs| fs.* = FirstSet.init(self.allocator);

            var follow = try self.allocator.alloc(FollowSet, nt_count);
            defer {
                for (follow) |*fs| fs.deinit();
                self.allocator.free(follow);
            }
            for (follow) |*fs| fs.* = FollowSet.init(self.allocator);

            // FOLLOW(start) includes EOF
            {
                const start_i = @intFromEnum(Grammar.start_symbol);
                _ = try follow[start_i].getOrPut(Grammar.eofTerminal());
            }

            // ---- compute FIRST (fixpoint) ----
            var changed = true;
            while (changed) {
                changed = false;
                for (Grammar.productions) |p| {
                    const A = @intFromEnum(p.left);

                    var seq_first = try self.firstOfSequenceLL1(&first, p.right);
                    defer seq_first.terms.deinit();

                    // merge terms
                    var it = seq_first.terms.iterator();
                    while (it.next()) |e| {
                        const t = e.key_ptr.*;
                        if (try first[A].addTerm(t)) changed = true;
                    }
                    if (seq_first.eps and !first[A].eps) {
                        first[A].eps = true;
                        changed = true;
                    }
                }
            }

            // ---- compute FOLLOW (fixpoint, LL1) ----
            changed = true;
            while (changed) {
                changed = false;
                for (Grammar.productions) |p| {
                    const A_i = @intFromEnum(p.left);

                    // scan RHS for nonterminals B
                    var i: usize = 0;
                    while (i < p.right.len) : (i += 1) {
                        const sym = p.right[i];
                        if (sym != .nonterm) continue;

                        const B = @intFromEnum(sym.nonterm);

                        // beta = rhs[i+1..]
                        const beta = p.right[i + 1 ..];

                        var first_beta = try self.firstOfSequenceLL1(&first, beta);
                        defer first_beta.terms.deinit();

                        // add FIRST(beta) \ {ε} to FOLLOW(B)
                        var itb = first_beta.terms.iterator();
                        while (itb.next()) |e| {
                            const t = e.key_ptr.*;
                            const gop = try follow[B].getOrPut(t);
                            if (!gop.found_existing) changed = true;
                        }

                        // if beta nullable, add FOLLOW(A) to FOLLOW(B)
                        if (first_beta.eps) {
                            var ita = follow[A_i].iterator();
                            while (ita.next()) |e| {
                                const t = e.key_ptr.*;
                                const gop = try follow[B].getOrPut(t);
                                if (!gop.found_existing) changed = true;
                            }
                        }
                    }
                }
            }

            // ---- build parse table ----
            self.table = try self.allocator.alloc(Row, nt_count);
            for (self.table) |*r| r.* = Row.init(self.allocator);

            for (Grammar.productions) |p| {
                const A_i = @intFromEnum(p.left);

                var first_alpha = try self.firstOfSequenceLL1(&first, p.right);
                defer first_alpha.terms.deinit();

                // for each terminal in FIRST(alpha): table[A, t] = p.id
                var it = first_alpha.terms.iterator();
                while (it.next()) |e| {
                    const t = e.key_ptr.*;
                    try self.putTableEntry(A_i, t, p.id);
                }

                // if ε in FIRST(alpha): for each t in FOLLOW(A): table[A, t] = p.id
                if (first_alpha.eps) {
                    var itf = follow[A_i].iterator();
                    while (itf.next()) |e| {
                        const t = e.key_ptr.*;
                        try self.putTableEntry(A_i, t, p.id);
                    }
                }
            }
        }

        fn putTableEntry(self: *Self, A_i: usize, t: Grammar.Symbol, id: Grammar.Id) !void {
            const row = &self.table[A_i];
            const gop = try row.getOrPut(t);
            if (gop.found_existing) {
                // LL(1) conflict
                return error.GrammarNotLL1;
            }
            gop.value_ptr.* = id;
        }

        fn firstOfSequenceLL1(self: *Self, first: *[]FirstSet, seq: []const Grammar.Symbol) !FirstSet {
            var out = FirstSet.init(self.allocator);

            if (seq.len == 0) {
                out.eps = true;
                return out;
            }

            var nullable_prefix = true;

            for (seq) |s| {
                if (!nullable_prefix) break;

                var s_first = try self.firstOfSymbolLL1(first, s);
                defer s_first.terms.deinit();

                // add terminals
                var it = s_first.terms.iterator();
                while (it.next()) |e| {
                    const t = e.key_ptr.*;
                    _ = try out.addTerm(t);
                }

                // keep going only if ε in FIRST(s)
                nullable_prefix = s_first.eps;
            }

            out.eps = nullable_prefix;
            return out;
        }

        fn firstOfSymbolLL1(self: *Self, first: *[]FirstSet, s: Grammar.Symbol) !FirstSet {
            var out = FirstSet.init(self.allocator);

            switch (s) {
                .nonterm => |nt| {
                    const fs = &first.*[@intFromEnum(nt)];
                    out.eps = fs.eps;

                    var it = fs.terms.iterator();
                    while (it.next()) |e| {
                        const t = e.key_ptr.*;
                        _ = try out.addTerm(t);
                    }
                    return out;
                },
                else => {
                    // terminal
                    _ = try out.addTerm(s);
                    out.eps = false;
                    return out;
                },
            }
        }
    };
}
