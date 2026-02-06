const std = @import("std");

const core = @import("../core/core.zig");

/// Identifier interning
pub const Interner = struct {
    gpa: std.mem.Allocator,

    map: std.StringHashMapUnmanaged(core.Identifier) = .{},
    strings: std.ArrayListUnmanaged([]const u8) = .{},

    pub fn init(gpa: std.mem.Allocator) Interner {
        return .{ .gpa = gpa };
    }

    pub fn deinit(self: *Interner) void {
        for (self.strings.items) |s| self.gpa.free(s);

        self.strings.deinit(self.gpa);
        self.map.deinit(self.gpa);
    }

    /// Intern `name` and return its ID.
    /// The returned ID is stable for the lifetime of this interner.
    pub fn intern(self: *Interner, name: []const u8) !core.Identifier {
        // already interned.
        if (self.map.get(name)) |id| return id;

        const owned = try self.gpa.dupe(u8, name);
        errdefer self.gpa.free(owned);

        const id: core.Identifier = @intCast(self.strings.items.len);
        if (id == std.math.maxInt(core.Identifier)) {
            return error.TooManyIdentifiers;
        }
        try self.strings.append(self.gpa, owned);

        errdefer {
            _ = self.strings.pop();
        }

        try self.map.put(self.gpa, owned, id);

        return id;
    }

    /// Look up an already-interned name. Returns null if not interned.
    pub fn lookup(self: *const Interner, name: []const u8) ?core.Identifier {
        return self.map.get(name);
    }

    /// Resolve an ID back to the canonical string.
    pub fn get(self: *const Interner, id: core.Identifier) ?[]const u8 {
        if (id >= self.strings.items.len) return null;

        return self.strings.items[@intCast(id)];
    }

    pub fn count(self: *const Interner) usize {
        return self.strings.items.len;
    }

    pub fn reserve(self: *Interner, n: usize) !void {
        try self.strings.ensureTotalCapacity(self.gpa, n);
        try self.map.ensureTotalCapacity(self.gpa, n);
    }
};
