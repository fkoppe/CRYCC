pub const Span = struct {
    source_id: u32,
    start: usize,
    end: usize,
};

pub fn Spanned(comptime T: type) type {
    return struct {
        span: Span,
        value: T,

        const Self = @This();

        pub fn init(start: usize, end: usize, value: T) Self {
            return .{
                .span = .{ .source_id = 0, .start = start, .end = end },
                .value = value,
            };
        }
    };
}
