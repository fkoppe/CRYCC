pub const Identifier = u32;

pub const CommentSpec = struct {
    line: ?[]const u8 = null,

    block: ?struct {
        start: []const u8,
        end: []const u8,
    } = null,

    doc_line: ?[]const u8 = null,

    doc_block: ?struct {
        start: []const u8,
        end: []const u8,
    } = null,
};

pub fn LiteralSpec(comptime Kind: type) type {
    return struct {
        text: []const u8,
        kind: Kind,
        start: Start = .{ .any_of = &.{""} },
        body: LiteralBody,
        suffixes: []const []const u8 = &.{},

        pub const Start = union(enum) {
            /// One of these sequences must match at the current position.
            /// Keep longest entries first if overlapping.
            any_of: []const []const u8,
        };
    };
}

pub const LiteralBody = union(enum) {
    pub const Escape = union(enum) {
        none,

        /// Traditional `\`-escaping.
        backslash,

        /// `""` inside `"`-strings (SQL-style).
        doubled_end,
    };

    pub const Exponent = struct {
        /// e.g. `eE` (base 10), or `pP` (hex floats)
        markers: []const u8,
        allow_sign: bool = true,
    };

    pub const Delimited = struct {
        pub const End = union(enum) {
            fixed: []const u8,
            same_as_start,
        };

        /// How the literal ends.
        end: End = .same_as_start,

        /// Escape rules while scanning.
        escape: Escape = .none,

        multiline: bool = false,
    };

    pub const Number = struct {
        base: u8, // 2, 8, 10, 16

        /// Optional digit separator (e.g. `_` in `1_000`).
        digit_sep: ?u8 = null,

        /// Optional fractional part and exponent.
        allow_fraction: bool = false,
        exponent: ?Exponent = null,
    };

    pub const TakeWhile = struct {
        pub const CharClass = union(enum) {
            any,
            ascii_alpha,
            ascii_alnum,
            ascii_digit,
            hex_digit,
            oct_digit,
            bin_digit,

            /// Set membership over raw bytes.
            byte_set: []const u8,

            /// Negated set membership over raw bytes.
            not_byte_set: []const u8,
        };

        class: CharClass,
        min: usize = 1,
        max: ?usize = null,
    };

    pub const UntilAny = struct {
        /// End markers (longest-first if overlapping).
        end_markers: []const []const u8,
        escape: Escape = .none,
        multiline: bool = false,
    };

    /// Literal is exactly one of the start spellings (no extra scanning).
    exact: void,

    number: Number,
    delimited: Delimited,
    take_while: TakeWhile,
    until_any: UntilAny,
};
