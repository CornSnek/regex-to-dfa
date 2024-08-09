const std = @import("std");
const ESC = @import("main.zig").ESC;
const SortedList = @import("sorted_list.zig").SortedList;
const SortedIntList = @import("sorted_list.zig").SortedIntList;
const RingBufferQueue = @import("ring_buffer.zig").RingBufferQueue;
pub fn Range(comptime IntT: type) type {
    return struct {
        min: IntT,
        max: IntT,
        pub const RangeSplit = struct { i: Range(IntT), l: ?Range(IntT), u: ?Range(IntT) };
        /// Splits range into any intersections and any leftover partitions of this_r.
        pub fn intersect_split(this_r: Range(IntT), other_r: Range(IntT)) ?RangeSplit {
            const intersect: Range(IntT) = .{ .min = @max(other_r.min, this_r.min), .max = @min(other_r.max, this_r.max) };
            return if (intersect.min <= intersect.max) .{
                .i = intersect,
                //std.math.minInt(IntT)/std.math.maxInt(IntT) checked because of underflow/overflow
                .l = if (intersect.min != std.math.minInt(IntT) and this_r.min <= intersect.min - 1) .{ .min = this_r.min, .max = intersect.min - 1 } else null,
                .u = if (intersect.max != std.math.maxInt(IntT) and this_r.max >= intersect.max + 1) .{ .min = intersect.max + 1, .max = this_r.max } else null,
            } else null;
        }
        /// this_r U other_r but partitioned into 1 to 3 ranges
        pub fn union_split(this_r: Range(IntT), other_r: Range(IntT)) ?RangeSplit {
            const intersect: Range(IntT) = .{ .min = @max(other_r.min, this_r.min), .max = @min(other_r.max, this_r.max) };
            return if (intersect.min <= intersect.max) .{
                .i = intersect,
                .l = if (intersect.min != std.math.minInt(IntT) and @min(this_r.min, other_r.min) <= intersect.min - 1) .{ .min = @min(this_r.min, other_r.min), .max = intersect.min - 1 } else null,
                .u = if (intersect.max != std.math.maxInt(IntT) and @max(this_r.max, other_r.max) >= intersect.max + 1) .{ .min = intersect.max + 1, .max = @max(this_r.max, other_r.max) } else null,
            } else null;
        }
        /// this_r U other_r
        pub fn union_merge(this_r: Range(IntT), other_r: Range(IntT)) ?Range(IntT) {
            const intersect: Range(IntT) = .{ .min = @max(other_r.min, this_r.min), .max = @min(other_r.max, this_r.max) };
            return if (intersect.min <= intersect.max +| 1)
                .{ .min = @min(other_r.min, this_r.min), .max = @max(other_r.max, this_r.max) }
            else
                null;
        }
        pub fn contains_range(this_r: Range(IntT), other_r: Range(IntT)) bool {
            return this_r.min <= other_r.min and other_r.max <= this_r.max;
        }
        pub fn contains_point(self: Range(IntT), point: IntT) bool {
            return self.min <= point and point <= self.max;
        }
    };
}
test "Range(u16) intersect_split contined in middle" {
    const r1: Range(u16) = .{ .min = 2, .max = 10 };
    const r2: Range(u16) = .{ .min = 5, .max = 6 };
    try std.testing.expectEqual(@as(
        ?Range(u16).RangeSplit,
        .{ .i = .{ .min = 5, .max = 6 }, .l = .{ .min = 2, .max = 4 }, .u = .{ .min = 7, .max = 10 } },
    ), r1.intersect_split(r2));
}
test "Range(u16) union_split one contained in middle" {
    const r1: Range(u16) = .{ .min = 2, .max = 10 };
    const r2: Range(u16) = .{ .min = 5, .max = 6 };
    try std.testing.expectEqual(@as(
        ?Range(u16).RangeSplit,
        .{ .i = .{ .min = 5, .max = 6 }, .l = .{ .min = 2, .max = 4 }, .u = .{ .min = 7, .max = 10 } },
    ), r1.union_split(r2));
    try std.testing.expectEqual(r1.union_split(r2), r2.union_split(r1));
}
test "Range(u16) intersect_split union_split equal" {
    const r1: Range(u16) = .{ .min = 2, .max = 10 };
    const r2: Range(u16) = .{ .min = 2, .max = 10 };
    try std.testing.expectEqual(@as(
        ?Range(u16).RangeSplit,
        .{ .i = .{ .min = 2, .max = 10 }, .l = null, .u = null },
    ), r1.intersect_split(r2));
    try std.testing.expectEqual(r1.intersect_split(r2), r1.union_split(r2));
    try std.testing.expectEqual(r1.union_split(r2), r2.union_split(r1));
}
test "Range(u16) intersect_split whole range" {
    const r1: Range(u16) = .{ .min = 2, .max = 10 };
    const r2: Range(u16) = .{ .min = 0, .max = 12 };
    try std.testing.expectEqual(@as(
        ?Range(u16).RangeSplit,
        .{ .i = .{ .min = 2, .max = 10 }, .l = null, .u = null },
    ), r1.intersect_split(r2));
}
test "Range(u16) intersect_split contained left" {
    const r1: Range(u16) = .{ .min = 2, .max = 10 };
    const r2: Range(u16) = .{ .min = 0, .max = 3 };
    try std.testing.expectEqual(@as(
        ?Range(u16).RangeSplit,
        .{ .i = .{ .min = 2, .max = 3 }, .l = null, .u = .{ .min = 4, .max = 10 } },
    ), r1.intersect_split(r2));
}
test "Range(u16) union_split contained left" {
    const r1: Range(u16) = .{ .min = 2, .max = 10 };
    const r2: Range(u16) = .{ .min = 0, .max = 3 };
    try std.testing.expectEqual(@as(
        ?Range(u16).RangeSplit,
        .{ .i = .{ .min = 2, .max = 3 }, .l = .{ .min = 0, .max = 1 }, .u = .{ .min = 4, .max = 10 } },
    ), r1.union_split(r2));
    try std.testing.expectEqual(r1.union_split(r2), r2.union_split(r1));
}
test "Range(u16) intersect_split contained right" {
    const r1: Range(u16) = .{ .min = 2, .max = 10 };
    const r2: Range(u16) = .{ .min = 9, .max = 12 };
    try std.testing.expectEqual(@as(
        ?Range(u16).RangeSplit,
        .{ .i = .{ .min = 9, .max = 10 }, .l = .{ .min = 2, .max = 8 }, .u = null },
    ), r1.intersect_split(r2));
}
test "Range(u16) union_split contained right" {
    const r1: Range(u16) = .{ .min = 2, .max = 10 };
    const r2: Range(u16) = .{ .min = 9, .max = 12 };
    try std.testing.expectEqual(@as(
        ?Range(u16).RangeSplit,
        .{ .i = .{ .min = 9, .max = 10 }, .l = .{ .min = 2, .max = 8 }, .u = .{ .min = 11, .max = 12 } },
    ), r1.union_split(r2));
    try std.testing.expectEqual(r1.union_split(r2), r2.union_split(r1));
}
test "Range(u16) intersect_split union_split none at left side" {
    const r1: Range(u16) = .{ .min = 2, .max = 10 };
    const r2: Range(u16) = .{ .min = 0, .max = 1 };
    try std.testing.expectEqual(@as(
        ?Range(u16).RangeSplit,
        null,
    ), r1.intersect_split(r2));
    try std.testing.expectEqual(r1.intersect_split(r2), r1.union_split(r2));
    try std.testing.expectEqual(r1.union_split(r2), r2.union_split(r1));
}
test "Range(u16) intersect_split union_split none at right side" {
    const r1: Range(u16) = .{ .min = 2, .max = 10 };
    const r2 = Range(u16){ .min = 11, .max = 20 };
    try std.testing.expectEqual(@as(
        ?Range(u16).RangeSplit,
        null,
    ), r1.intersect_split(r2));
    try std.testing.expectEqual(r1.intersect_split(r2), r1.union_split(r2));
    try std.testing.expectEqual(r1.union_split(r2), r2.union_split(r1));
}
test "Range(u16) intersect_split one number" {
    const r1: Range(u16) = .{ .min = 2, .max = 10 };
    const r2: Range(u16) = .{ .min = 5, .max = 5 };
    try std.testing.expectEqual(@as(
        ?Range(u16).RangeSplit,
        .{ .i = .{ .min = 5, .max = 5 }, .l = .{ .min = 2, .max = 4 }, .u = .{ .min = 6, .max = 10 } },
    ), r1.intersect_split(r2));
    try std.testing.expectEqual(r1.intersect_split(r2), r1.union_split(r2));
    try std.testing.expectEqual(r1.union_split(r2), r2.union_split(r1));
}
test "Range(u16) intersect_split at minimum" {
    const r1: Range(u16) = .{ .min = 0, .max = 5 };
    const r2: Range(u16) = .{ .min = 0, .max = 10 };
    try std.testing.expectEqual(@as(
        ?Range(u16).RangeSplit,
        .{ .i = .{ .min = 0, .max = 5 }, .l = null, .u = null },
    ), r1.intersect_split(r2));
}
test "Range(u16) union_split at minimum" {
    const r1: Range(u16) = .{ .min = 0, .max = 5 };
    const r2: Range(u16) = .{ .min = 0, .max = 10 };
    try std.testing.expectEqual(@as(
        ?Range(u16).RangeSplit,
        .{ .i = .{ .min = 0, .max = 5 }, .l = null, .u = .{ .min = 6, .max = 10 } },
    ), r1.union_split(r2));
    try std.testing.expectEqual(r1.union_split(r2), r2.union_split(r1));
}
test "Range(u16) intersect_split at maximum" {
    const r1: Range(u16) = .{ .min = std.math.maxInt(u16) - 3, .max = std.math.maxInt(u16) };
    const r2: Range(u16) = .{ .min = std.math.maxInt(u16) - 10, .max = std.math.maxInt(u16) };
    try std.testing.expectEqual(@as(
        ?Range(u16).RangeSplit,
        .{ .i = .{ .min = std.math.maxInt(u16) - 3, .max = std.math.maxInt(u16) }, .l = null, .u = null },
    ), r1.intersect_split(r2));
}
test "Range(u16) union_split at maximum" {
    const r1: Range(u16) = .{ .min = std.math.maxInt(u16) - 3, .max = std.math.maxInt(u16) };
    const r2: Range(u16) = .{ .min = std.math.maxInt(u16) - 10, .max = std.math.maxInt(u16) };
    try std.testing.expectEqual(@as(
        ?Range(u16).RangeSplit,
        .{
            .i = .{ .min = std.math.maxInt(u16) - 3, .max = std.math.maxInt(u16) },
            .l = .{ .min = std.math.maxInt(u16) - 10, .max = std.math.maxInt(u16) - 4 },
            .u = null,
        },
    ), r1.union_split(r2));
    try std.testing.expectEqual(r1.union_split(r2), r2.union_split(r1));
}
test "Range(u16) union_merge" {
    const r1: Range(u16) = .{ .min = 1, .max = 2 };
    const r2: Range(u16) = .{ .min = 3, .max = 4 };
    const r3: Range(u16) = .{ .min = 2, .max = 3 };
    const r4: Range(u16) = .{ .min = 4, .max = 5 };
    try std.testing.expectEqual(@as(
        ?Range(u16),
        .{ .min = 1, .max = 4 },
    ), r1.union_merge(r2));
    try std.testing.expectEqual(r1.union_merge(r2), r2.union_merge(r1));
    try std.testing.expectEqual(@as(
        ?Range(u16),
        .{ .min = 1, .max = 3 },
    ), r1.union_merge(r3));
    try std.testing.expectEqual(r1.union_merge(r3), r3.union_merge(r1));
    try std.testing.expectEqual(@as(
        ?Range(u16),
        null,
    ), r1.union_merge(r4));
    try std.testing.expectEqual(r1.union_merge(r4), r4.union_merge(r1));
}
test "Range(u16) union_merge at maximum" {
    const r1: Range(u16) = .{ .min = std.math.maxInt(u16) - 1, .max = std.math.maxInt(u16) };
    const r2: Range(u16) = .{ .min = std.math.maxInt(u16) - 3, .max = std.math.maxInt(u16) - 2 };
    const r3: Range(u16) = .{ .min = std.math.maxInt(u16) - 2, .max = std.math.maxInt(u16) - 1 };
    const r4: Range(u16) = .{ .min = std.math.maxInt(u16) - 4, .max = std.math.maxInt(u16) - 3 };
    try std.testing.expectEqual(@as(
        ?Range(u16),
        .{ .min = std.math.maxInt(u16) - 3, .max = std.math.maxInt(u16) },
    ), r1.union_merge(r2));
    try std.testing.expectEqual(r1.union_merge(r2), r2.union_merge(r1));
    try std.testing.expectEqual(@as(
        ?Range(u16),
        .{ .min = std.math.maxInt(u16) - 2, .max = std.math.maxInt(u16) },
    ), r1.union_merge(r3));
    try std.testing.expectEqual(r1.union_merge(r3), r3.union_merge(r1));
    try std.testing.expectEqual(@as(
        ?Range(u16),
        null,
    ), r1.union_merge(r4));
    try std.testing.expectEqual(r1.union_merge(r4), r4.union_merge(r1));
}
const DataType = union(enum) {
    none: void,
    char: u8,
    unicode: u16,
    range: Range(u16),
    fn r_or_un(range: Range(u16)) DataType {
        return if (range.min != range.max) .{ .range = range } else .{ .unicode = range.min };
    }
    pub fn format(self: @This(), comptime _: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        try writer.writeAll("T[");
        switch (self) {
            .none => try std.fmt.formatBuf("(none)", options, writer),
            .char => |ch| {
                try writer.writeByte('\'');
                if (32 <= ch and ch <= 126) { //Not control characters
                    try std.fmt.formatAsciiChar(ch, options, writer);
                    try writer.writeByte('\'');
                    try writer.writeAll(" \\x");
                    try std.fmt.formatInt(ch, 16, .lower, .{ .fill = '0', .width = 2 }, writer);
                } else {
                    try writer.writeAll("\\x");
                    try std.fmt.formatInt(ch, 16, .lower, .{ .fill = '0', .width = 2 }, writer);
                    try writer.writeByte('\'');
                }
            },
            .unicode => |u| {
                try writer.writeByte('\'');
                if (32 <= u and u <= 126) {
                    try std.fmt.formatAsciiChar(@intCast(u), options, writer);
                    try writer.writeByte('\'');
                    try writer.writeAll(" \\u");
                    try std.fmt.formatInt(u, 16, .lower, .{ .fill = '0', .width = 4 }, writer);
                } else {
                    try writer.writeAll("'\\u");
                    try std.fmt.formatInt(u, 16, .lower, .{ .fill = '0', .width = 4 }, writer);
                    try writer.writeByte('\'');
                }
            },
            .range => |r| {
                try writer.writeByte('\'');
                if (32 <= r.min and r.min <= 126) {
                    try std.fmt.formatAsciiChar(@intCast(r.min), options, writer);
                    try writer.writeByte('\'');
                    try writer.writeAll(" \\u");
                    try std.fmt.formatInt(r.min, 16, .lower, .{ .fill = '0', .width = 4 }, writer);
                } else {
                    try writer.writeAll("\\u");
                    try std.fmt.formatInt(r.min, 16, .lower, .{ .fill = '0', .width = 4 }, writer);
                    try writer.writeByte('\'');
                }
                try writer.writeAll(" to '");
                if (32 <= r.max and r.max <= 126) {
                    try std.fmt.formatAsciiChar(@intCast(r.max), options, writer);
                    try writer.writeByte('\'');
                    try writer.writeAll(" \\u");
                    try std.fmt.formatInt(r.max, 16, .lower, .{ .fill = '0', .width = 4 }, writer);
                } else {
                    try writer.writeAll("\\u");
                    try std.fmt.formatInt(r.max, 16, .lower, .{ .fill = '0', .width = 4 }, writer);
                    try writer.writeByte('\'');
                }
            },
        }
        try writer.writeByte(']');
    }
    ///Sort by the minimum number where .none is always the lowest.
    pub fn lt(_: void, ldt: DataType, rdt: DataType) bool {
        const ldt_min: u16 = switch (ldt) {
            .char => |ch| ch,
            .unicode => |u| u,
            .range => |r| r.min,
            .none => return true,
        };
        const rdt_min: u16 = switch (rdt) {
            .char => |ch| ch,
            .unicode => |u| u,
            .range => |r| r.min,
            .none => return false,
        };
        return ldt_min < rdt_min;
    }
    pub fn eq(self: @This(), other: @This()) bool {
        switch (self) {
            .none => return switch (other) {
                .none => true,
                .char => false,
                .unicode => false,
                .range => false,
            },
            .char => |c1| return switch (other) {
                .none => false,
                .char => |c2| c1 == c2,
                .unicode => false,
                .range => false,
            },
            .unicode => |un1| return switch (other) {
                .none => false,
                .char => false,
                .unicode => |un2| un1 == un2,
                .range => false,
            },
            .range => |r1| return switch (other) {
                .none => false,
                .char => false,
                .unicode => false,
                .range => |r2| r1.min == r2.min and r1.max == r2.max,
            },
        }
    }
};
pub const Transition = struct {
    to: u32,
    dtype: DataType,
    pub fn format(self: Transition, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        try self.dtype.format(fmt, options, writer);
        try writer.writeAll(" => ");
        try std.fmt.formatInt(self.to, 10, .lower, options, writer);
    }
    pub fn lt(ctx: void, ltr: Transition, rtr: Transition) bool {
        if (ltr.to < rtr.to) return true;
        if (ltr.to > rtr.to) return false;
        return DataType.lt(ctx, ltr.dtype, rtr.dtype);
    }
    pub fn eq(self: Transition, other: Transition) bool {
        if (self.to != other.to) return false;
        return self.dtype.eq(other.dtype);
    }
    /// Redundant or connected points/ranges are combined
    pub fn union_merge(self: *[]Transition, allocator: std.mem.Allocator) !void {
        if (self.len == 0) return;
        std.sort.block(Transition, self.*, {}, Transition.lt);
        var t_ptr: usize = self.len - 1;
        var new_self_len: usize = self.len;
        var cmp_tr = self.*[t_ptr];
        var copy_len: usize = 0;
        var nonconnected: usize = 0;
        var merged_ranges: usize = 0;
        var dtype_links: usize = 0;
        while (t_ptr != 0) : (t_ptr -= 1) { //Merge connected ranges and points backwards
            const cmp_next_tr = self.*[t_ptr - 1];
            no_merged_range: {
                if (cmp_tr.to != cmp_next_tr.to) break :no_merged_range;
                const cmp_tr_r: Range(u16) = switch (cmp_tr.dtype) {
                    .none => unreachable,
                    .unicode, .char => |wc| .{ .min = wc, .max = wc },
                    .range => |r| r,
                };
                const cmp_next_tr_r: Range(u16) = switch (cmp_next_tr.dtype) {
                    .none => unreachable,
                    .unicode, .char => |wc| .{ .min = wc, .max = wc },
                    .range => |r| r,
                };
                if (cmp_tr_r.union_merge(cmp_next_tr_r)) |merged_range| {
                    cmp_tr.dtype = .{ .range = merged_range };
                    dtype_links += 1;
                    continue;
                }
            }
            if (dtype_links == 0) {
                copy_len += 1;
                nonconnected += 1;
                cmp_tr.dtype = cmp_next_tr.dtype;
                continue;
            }
            self.*[t_ptr].dtype = switch (cmp_tr.dtype) {
                .none => unreachable,
                .char, .unicode => cmp_tr.dtype,
                .range => |r| DataType.r_or_un(r),
            };
            std.mem.copyForwards(Transition, self.*[t_ptr + 1 ..], self.*[new_self_len - copy_len .. new_self_len]);
            merged_ranges += 1;
            copy_len = merged_ranges + nonconnected;
            new_self_len -= dtype_links;
            dtype_links = 0;
            cmp_tr.dtype = cmp_next_tr.dtype;
        }
        if (dtype_links != 0) {
            self.*[0].dtype = switch (cmp_tr.dtype) {
                .none => unreachable,
                .char, .unicode => cmp_tr.dtype,
                .range => |r| DataType.r_or_un(r),
            };
            std.mem.copyForwards(Transition, self.*[1..], self.*[new_self_len - copy_len .. new_self_len]);
            new_self_len -= dtype_links;
        }
        self.* = try allocator.realloc(self.*, new_self_len);
    }
};
test "Transition merge merge ranges together" {
    var transitions = try std.testing.allocator.dupe(Transition, &[_]Transition{
        .{ .to = 0, .dtype = .{ .char = 2 } },
        .{ .to = 0, .dtype = .{ .char = 3 } },
        .{ .to = 0, .dtype = .{ .char = 4 } },
        .{ .to = 0, .dtype = .{ .char = 6 } },
        .{ .to = 0, .dtype = .{ .char = 8 } },
        .{ .to = 0, .dtype = .{ .char = 11 } },
        .{ .to = 0, .dtype = .{ .char = 12 } },
        .{ .to = 0, .dtype = .{ .char = 13 } },
        .{ .to = 0, .dtype = .{ .char = 14 } },
        .{ .to = 0, .dtype = .{ .char = 15 } },
        .{ .to = 0, .dtype = .{ .char = 18 } },
        .{ .to = 0, .dtype = .{ .char = 21 } },
        .{ .to = 0, .dtype = .{ .char = 22 } },
        .{ .to = 0, .dtype = .{ .char = 23 } },
        .{ .to = 0, .dtype = .{ .char = 24 } },
        .{ .to = 0, .dtype = .{ .char = 25 } },
    });
    defer std.testing.allocator.free(transitions);
    try Transition.union_merge(&transitions, std.testing.allocator);
    try std.testing.expectEqualSlices(Transition, &.{
        .{ .to = 0, .dtype = .{ .range = .{ .min = 2, .max = 4 } } },
        .{ .to = 0, .dtype = .{ .char = 6 } },
        .{ .to = 0, .dtype = .{ .char = 8 } },
        .{ .to = 0, .dtype = .{ .range = .{ .min = 11, .max = 15 } } },
        .{ .to = 0, .dtype = .{ .char = 18 } },
        .{ .to = 0, .dtype = .{ .range = .{ .min = 21, .max = 25 } } },
    }, transitions);
}
test "Transition merge remove duplicates" {
    var transitions = try std.testing.allocator.dupe(Transition, &[_]Transition{
        .{ .to = 0, .dtype = .{ .unicode = 'a' } },
        .{ .to = 0, .dtype = .{ .unicode = 'b' } },
        .{ .to = 0, .dtype = .{ .unicode = 'c' } },
        .{ .to = 0, .dtype = .{ .unicode = 'd' } },
        .{ .to = 0, .dtype = .{ .unicode = 'A' } },
        .{ .to = 0, .dtype = .{ .unicode = 'B' } },
        .{ .to = 0, .dtype = .{ .unicode = 'C' } },
        .{ .to = 0, .dtype = .{ .unicode = 'D' } },
        .{ .to = 0, .dtype = .{ .unicode = 'b' } },
        .{ .to = 0, .dtype = .{ .unicode = 'c' } },
        .{ .to = 0, .dtype = .{ .unicode = 'd' } },
        .{ .to = 0, .dtype = .{ .unicode = 'e' } },
        .{ .to = 0, .dtype = .{ .unicode = 'C' } },
    });
    defer std.testing.allocator.free(transitions);
    try Transition.union_merge(&transitions, std.testing.allocator);
    try std.testing.expectEqualSlices(Transition, &.{
        .{ .to = 0, .dtype = .{ .range = .{ .min = 'A', .max = 'D' } } },
        .{ .to = 0, .dtype = .{ .range = .{ .min = 'a', .max = 'e' } } },
    }, transitions);
}
pub const RegexState = struct {
    pub const ErrorState: u32 = 0;
    id: u32,
    transitions: []Transition = &.{},
    accept: bool = false,
    fn to_state(self: *const RegexState, wc: u16) u32 {
        for (self.transitions) |tr| {
            switch (tr.dtype) {
                .none => continue,
                .char => |c| if (wc == c) return tr.to,
                .unicode => |u| if (wc == u) return tr.to,
                .range => |r| if (r.contains_point(wc)) return tr.to,
            }
        }
        return ErrorState;
    }
    fn to_state_ptr(self: *const RegexState, ctx: *const RegexStateContext, wc: u16) *const RegexState {
        return &ctx.array[self.to_state(wc)];
    }
    /// Returns a list of transitions that point to different transitions by ranges and/or points.
    fn to_states(self: *const RegexState, allocator: std.mem.Allocator, range: Range(u16)) ![]Transition {
        var tr_list: std.ArrayListUnmanaged(Transition) = .{};
        defer tr_list.deinit(allocator);
        var ranges_left: std.ArrayListUnmanaged(Range(u16)) = .{};
        defer ranges_left.deinit(allocator);
        try ranges_left.append(allocator, range);
        for (self.transitions) |tr| {
            switch (tr.dtype) {
                .none => continue,
                .char => |c| {
                    for (0..ranges_left.items.len) |i| {
                        const range2 = ranges_left.items[i];
                        if (range2.intersect_split(.{ .min = c, .max = c })) |range_split| {
                            try add_leftover_ranges(allocator, i, &ranges_left, range_split);
                            try tr_list.append(allocator, .{ .to = tr.to, .dtype = .{ .char = c } });
                            break;
                        }
                    }
                },
                .unicode => |un| {
                    for (0..ranges_left.items.len) |i| {
                        const range2 = ranges_left.items[i];
                        if (range2.intersect_split(.{ .min = un, .max = un })) |range_split| {
                            try add_leftover_ranges(allocator, i, &ranges_left, range_split);
                            try tr_list.append(allocator, .{ .to = tr.to, .dtype = .{ .unicode = un } });
                            break;
                        }
                    }
                },
                .range => |r| {
                    for (0..ranges_left.items.len) |i| {
                        const range2 = ranges_left.items[i];
                        if (range2.intersect_split(r)) |range_split| {
                            try add_leftover_ranges(allocator, i, &ranges_left, range_split);
                            try tr_list.append(allocator, .{ .to = tr.to, .dtype = .{ .range = r } });
                            break;
                        }
                    }
                },
            }
        }
        for (ranges_left.items) |range_left| { //Other ranges/points go to ErrorState
            if (range_left.min != range_left.max) {
                try tr_list.append(allocator, .{ .to = ErrorState, .dtype = .{ .range = range_left } });
            } else {
                if (range_left.min <= std.math.maxInt(u16)) {
                    try tr_list.append(allocator, .{ .to = ErrorState, .dtype = .{ .char = @intCast(range_left.min) } });
                } else {
                    try tr_list.append(allocator, .{ .to = ErrorState, .dtype = .{ .unicode = range_left.min } });
                }
            }
        }
        std.sort.block(Transition, tr_list.items, {}, Transition.lt);
        return tr_list.toOwnedSlice(allocator);
    }
    fn add_leftover_ranges(allocator: std.mem.Allocator, i: usize, ranges_left: *std.ArrayListUnmanaged(Range(u16)), range_split: Range(u16).RangeSplit) !void {
        if (range_split.l != null and range_split.u != null) { //Any non-null leftover ranges overwrites the old range at i.
            ranges_left.items[i] = range_split.u.?;
            try ranges_left.insert(allocator, i, range_split.l.?);
        } else if (range_split.l != null and range_split.u == null) {
            ranges_left.items[i] = range_split.l.?;
        } else if (range_split.l == null and range_split.u != null) {
            ranges_left.items[i] = range_split.u.?;
        } else _ = ranges_left.orderedRemove(i);
    }
    pub fn format(self: @This(), comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        try writer.writeAll("{ [ ");
        try std.fmt.formatInt(self.id, 10, .lower, options, writer);
        try writer.writeAll(", A: ");
        try std.fmt.formatType(self.accept, "", options, writer, std.fmt.default_max_depth);
        try writer.writeAll(", T: { ");
        for (self.transitions, 0..) |tr, i| {
            try tr.format(fmt, options, writer);
            if (i != self.transitions.len - 1) try writer.writeAll(", ");
        }
        try writer.writeAll(" } ] }");
    }
};
pub const RegexStateContext = struct {
    array: []const RegexState,
    ///TODO
    str: []const u8 = &.{},
};
pub const RegexFSM = struct {
    pub const SubStateMachine = struct {
        pub const Accept = union(enum) {
            NFA: u32,
            DFA: []u32,
        };
        init: u32,
        accept: Accept,
        pub fn format(self: @This(), comptime _: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
            try writer.writeAll("[Init: ");
            try std.fmt.formatInt(self.init, 10, .lower, options, writer);
            try writer.writeAll(", Accept(");
            try std.fmt.formatBuf(@tagName(self.accept), options, writer);
            try writer.writeAll("): ");
            if (self.accept == .NFA) {
                try std.fmt.formatInt(self.accept.NFA, 10, .lower, options, writer);
            } else try std.fmt.formatType(self.accept.DFA, "any", options, writer, std.fmt.default_max_depth);
            try writer.writeByte(']');
        }
        pub fn deinit(self: SubStateMachine, allocator: std.mem.Allocator) void {
            if (self.accept == .DFA) allocator.free(self.accept.DFA);
        }
    };
    allocator: std.mem.Allocator,
    states: std.ArrayListUnmanaged(RegexState) = .{},
    substate_machines: std.ArrayListUnmanaged(SubStateMachine) = .{},
    set_datatypes: std.ArrayListUnmanaged(DataType) = .{},
    pub fn init(allocator: std.mem.Allocator) !RegexFSM {
        var self: RegexFSM = .{ .allocator = allocator };
        try self.states.ensureUnusedCapacity(allocator, 2);
        self.states.appendAssumeCapacity(.{ .id = RegexState.ErrorState });
        return self;
    }
    pub fn empty(self: *RegexFSM) !void {
        const last_id: u32 = @intCast(self.states.items.len);
        try self.states.append(self.allocator, .{ .id = last_id, .accept = true }); //Empty NFA used to concatenate an "empty" expression.
        try self.substate_machines.append(self.allocator, .{ .init = last_id, .accept = .{ .NFA = last_id } });
    }
    pub fn add_datatype(self: *RegexFSM, dt: DataType) !void {
        std.debug.assert(dt != .none);
        const this_id: u32 = @intCast(self.states.items.len);
        try self.states.append(self.allocator, .{ .id = this_id, .transitions = try self.allocator.alloc(Transition, 1) });
        errdefer self.allocator.free(self.states.items[this_id].transitions);
        self.states.items[this_id].transitions[0] = .{ .to = this_id + 1, .dtype = dt };
        try self.states.append(self.allocator, .{ .id = this_id + 1, .accept = true });
        try self.substate_machines.append(self.allocator, .{ .init = this_id, .accept = .{ .NFA = this_id + 1 } });
    }
    pub fn clone_ssm(self: *RegexFSM) !void {
        std.debug.assert(self.substate_machines.items.len != 0);
        const ssm = self.substate_machines.getLast();
        std.debug.assert(ssm.accept == .NFA);
        const ssm_slice = try self.allocator.dupe(RegexState, self.states.items[ssm.init .. ssm.accept.NFA + 1]);
        defer self.allocator.free(ssm_slice);
        try self.states.ensureUnusedCapacity(self.allocator, ssm_slice.len);
        for (ssm_slice) |ssm_e| {
            const add_ptr = self.states.addOneAssumeCapacity();
            add_ptr.* = ssm_e;
            add_ptr.id += @intCast(ssm_slice.len);
            add_ptr.transitions = try self.allocator.dupe(Transition, ssm_e.transitions);
            for (add_ptr.transitions) |*tr| tr.to += @intCast(ssm_slice.len);
        }
        try self.substate_machines.append(self.allocator, .{
            .init = ssm.accept.NFA + 1,
            .accept = .{ .NFA = ssm.accept.NFA + @as(u32, @intCast(ssm_slice.len)) },
        });
    }
    /// Quantifier '?' NFA
    pub fn optional(self: *RegexFSM) !void {
        var ssm = self.substate_machines.pop();
        std.debug.assert(ssm.accept == .NFA);
        try self.shift_right(ssm.init, 1);
        self.states.items[ssm.init] = .{ .id = ssm.init, .transitions = try self.allocator.alloc(Transition, 2) };
        self.states.items[ssm.init].transitions[0] = .{ .to = ssm.init + 1, .dtype = .none };
        self.states.items[ssm.init].transitions[1] = .{ .to = ssm.accept.NFA + 2, .dtype = .none }; //Point to the old init state and the new accept state.
        self.states.items[ssm.accept.NFA + 1] = .{ .id = ssm.accept.NFA + 1, .transitions = try self.allocator.alloc(Transition, 1) };
        self.states.items[ssm.accept.NFA + 1].transitions[0] = .{ .to = ssm.accept.NFA + 2, .dtype = .none }; //Change old accept state to .none transition to the new accept state.
        try self.states.append(self.allocator, .{ .id = ssm.accept.NFA + 2, .accept = true });
        ssm.accept.NFA += 2;
        self.substate_machines.appendAssumeCapacity(ssm);
    }
    /// ssm1|ssm2 NFA
    pub fn alternation(self: *RegexFSM) !void {
        const ssm2 = self.substate_machines.pop();
        const ssm1 = self.substate_machines.pop();
        std.debug.assert(ssm1.accept == .NFA and ssm2.accept == .NFA);
        try self.shift_right(ssm1.init, 1);
        self.states.items[ssm1.init] = .{ .id = ssm1.init, .transitions = try self.allocator.alloc(Transition, 2) };
        self.states.items[ssm1.init].transitions[0] = .{ .to = ssm1.init + 1, .dtype = .none };
        self.states.items[ssm1.init].transitions[1] = .{ .to = ssm2.init + 1, .dtype = .none };
        self.states.items[ssm1.accept.NFA + 1] = .{ .id = ssm1.accept.NFA + 1, .transitions = try self.allocator.alloc(Transition, 1) };
        self.states.items[ssm1.accept.NFA + 1].transitions[0] = .{ .to = ssm2.accept.NFA + 2, .dtype = .none };
        self.states.items[ssm2.accept.NFA + 1] = .{ .id = ssm2.accept.NFA + 1, .transitions = try self.allocator.alloc(Transition, 1) };
        self.states.items[ssm2.accept.NFA + 1].transitions[0] = .{ .to = ssm2.accept.NFA + 2, .dtype = .none };
        try self.states.append(self.allocator, .{ .id = ssm2.accept.NFA + 2, .accept = true });
        self.substate_machines.appendAssumeCapacity(.{ .init = ssm1.init, .accept = .{ .NFA = ssm2.accept.NFA + 2 } });
    }
    pub fn concatenation(self: *RegexFSM) !void {
        const ssm2 = self.substate_machines.pop();
        const ssm1 = self.substate_machines.pop();
        std.debug.assert(ssm1.accept == .NFA and ssm2.accept == .NFA);
        self.states.items[ssm1.accept.NFA].accept = false;
        self.states.items[ssm1.accept.NFA].transitions = try self.allocator.alloc(Transition, 1);
        self.states.items[ssm1.accept.NFA].transitions[0] = .{ .to = ssm2.init, .dtype = .none };
        self.substate_machines.appendAssumeCapacity(.{ .init = ssm1.init, .accept = ssm2.accept });
    }
    pub fn kleene_star(self: *RegexFSM) !void {
        const ssm = self.substate_machines.pop();
        std.debug.assert(ssm.accept == .NFA);
        try self.shift_right(ssm.init, 1);
        self.states.items[ssm.init] = .{ .id = ssm.init, .transitions = try self.allocator.alloc(Transition, 2) };
        self.states.items[ssm.init].transitions[0] = .{ .to = ssm.init + 1, .dtype = .none };
        self.states.items[ssm.init].transitions[1] = .{ .to = ssm.accept.NFA + 2, .dtype = .none };
        self.states.items[ssm.accept.NFA + 1].accept = false;
        self.states.items[ssm.accept.NFA + 1].transitions = try self.allocator.alloc(Transition, 2);
        self.states.items[ssm.accept.NFA + 1].transitions[0] = .{ .to = ssm.init + 1, .dtype = .none };
        self.states.items[ssm.accept.NFA + 1].transitions[1] = .{ .to = ssm.accept.NFA + 2, .dtype = .none };
        try self.states.append(self.allocator, .{ .id = ssm.accept.NFA + 2, .accept = true });
        self.substate_machines.appendAssumeCapacity(.{ .init = ssm.init, .accept = .{ .NFA = ssm.accept.NFA + 2 } });
    }
    pub fn plus(self: *RegexFSM) !void {
        try self.repetition_gte(1);
    }
    /// As quantifier `{by}`
    pub fn repetition(self: *RegexFSM, by: u32) !void {
        std.debug.assert(by != 0);
        var ssm_repeat = self.substate_machines.pop();
        std.debug.assert(ssm_repeat.accept == .NFA);
        const repeat_slice = try self.allocator.dupe(RegexState, self.states.items[ssm_repeat.init .. ssm_repeat.accept.NFA + 1]);
        defer self.allocator.free(repeat_slice); //Dupe because replaceRange overwrites the slice.
        self.states.items[self.states.items.len - 1].accept = false;
        self.states.items[self.states.items.len - 1].transitions = try self.allocator.alloc(Transition, 1);
        self.states.items[self.states.items.len - 1].transitions[0] = .{ .to = @intCast(self.states.items.len), .dtype = .none };
        var id_inc: u32 = 1;
        while (id_inc < by) : (id_inc += 1) {
            try self.states.appendSlice(self.allocator, repeat_slice);
            const this_slice = self.states.items[self.states.items.len - repeat_slice.len .. self.states.items.len];
            for (this_slice) |*e| {
                e.id += id_inc * @as(u32, @intCast(repeat_slice.len));
                e.transitions = try self.allocator.dupe(Transition, e.transitions);
                for (e.transitions) |*tr|
                    tr.to += id_inc * @as(u32, @intCast(repeat_slice.len));
            }
            if (id_inc == by - 1) break;
            self.states.items[self.states.items.len - 1].accept = false;
            self.states.items[self.states.items.len - 1].transitions = try self.allocator.alloc(Transition, 1);
            self.states.items[self.states.items.len - 1].transitions[0] = .{ .to = @intCast(self.states.items.len), .dtype = .none };
        }
        ssm_repeat.accept.NFA += (by - 1) * @as(u32, @intCast(repeat_slice.len));
        self.substate_machines.appendAssumeCapacity(ssm_repeat);
    }
    /// As quantifier `{,by}`
    pub fn repetition_lte(self: *RegexFSM, by: u32) !void {
        std.debug.assert(by != 0);
        var ssm_repeat = self.substate_machines.pop();
        std.debug.assert(ssm_repeat.accept == .NFA);
        const repeat_slice = try self.allocator.dupe(RegexState, self.states.items[ssm_repeat.init .. ssm_repeat.accept.NFA + 1]);
        defer self.allocator.free(repeat_slice); //Dupe because replaceRange overwrites the slice.
        const accept_state_i: u32 = ssm_repeat.accept.NFA + by * @as(u32, @intCast(repeat_slice.len)) + 3;
        try self.shift_right(ssm_repeat.init, 1);
        self.states.items[ssm_repeat.init] = .{ .id = ssm_repeat.init, .transitions = try self.allocator.alloc(Transition, 2) };
        self.states.items[ssm_repeat.init].transitions[0] = .{ .to = ssm_repeat.init + 1, .dtype = .none };
        self.states.items[ssm_repeat.init].transitions[1] = .{ .to = accept_state_i, .dtype = .none };
        self.states.items[self.states.items.len - 1].accept = false;
        self.states.items[self.states.items.len - 1].transitions = try self.allocator.alloc(Transition, 2);
        self.states.items[self.states.items.len - 1].transitions[0] = .{ .to = @intCast(self.states.items.len), .dtype = .none };
        self.states.items[self.states.items.len - 1].transitions[1] = .{ .to = accept_state_i, .dtype = .none };
        var id_inc: u32 = 1;
        while (id_inc < by + 1) : (id_inc += 1) {
            try self.states.appendSlice(self.allocator, repeat_slice);
            const this_slice = self.states.items[self.states.items.len - repeat_slice.len .. self.states.items.len];
            for (this_slice) |*e| {
                e.id += id_inc * @as(u32, @intCast(repeat_slice.len)) + 1;
                e.transitions = try self.allocator.dupe(Transition, e.transitions);
                for (e.transitions) |*tr|
                    tr.to += id_inc * @as(u32, @intCast(repeat_slice.len));
            }
            self.states.items[self.states.items.len - 1].accept = false;
            self.states.items[self.states.items.len - 1].transitions = try self.allocator.alloc(Transition, 2);
            self.states.items[self.states.items.len - 1].transitions[0] = .{ .to = @intCast(self.states.items.len), .dtype = .none };
            self.states.items[self.states.items.len - 1].transitions[1] = .{ .to = accept_state_i, .dtype = .none };
        }
        self.states.items[self.states.items.len - 1].transitions = try self.allocator.realloc(
            self.states.items[self.states.items.len - 1].transitions,
            1,
        );
        try self.states.append(self.allocator, .{ .id = accept_state_i - 1, .accept = false });
        try self.states.append(self.allocator, .{ .id = accept_state_i, .accept = true });
        ssm_repeat.accept.NFA = accept_state_i;
        self.substate_machines.appendAssumeCapacity(ssm_repeat);
    }
    /// As quantifier `{by,}`
    pub fn repetition_gte(self: *RegexFSM, by: u32) !void {
        for (0..by) |_| try self.clone_ssm();
        try self.kleene_star();
        for (0..by) |_| try self.concatenation();
    }
    /// As quantifier `{from,to}`
    pub fn repetition_between(self: *RegexFSM, from: u32, to: u32) !void {
        std.debug.assert(from != 0 and from <= to);
        if (from != to) {
            for (0..from) |_| try self.clone_ssm();
            try self.repetition_lte(to - from);
            for (0..from) |_| try self.concatenation();
        } else {
            try self.repetition(from);
        }
    }
    pub fn add_set_datatype(self: *RegexFSM, dt: DataType) !void {
        try self.set_datatypes.append(self.allocator, dt);
    }
    pub fn add_set(self: *RegexFSM) !void {
        var dtpl: DataTypePartitionList = .{};
        defer dtpl.deinit(self.allocator);
        for (self.set_datatypes.items) |dt| try dtpl.add(self.allocator, dt);
        const init_state_i: u32 = @intCast(self.states.items.len);
        try self.states.ensureUnusedCapacity(self.allocator, 2);
        self.states.appendSliceAssumeCapacity(&.{
            .{ .id = init_state_i },
            .{ .id = init_state_i + 1, .accept = true },
        });
        self.states.items[init_state_i].transitions = try self.allocator.alloc(Transition, dtpl.list.items.len);
        for (dtpl.list.items, 0..) |dt, i| self.states.items[init_state_i].transitions[i] = .{ .to = init_state_i + 1, .dtype = dt };
        try Transition.union_merge(&self.states.items[init_state_i].transitions, self.allocator);
        self.set_datatypes.clearRetainingCapacity();
        try self.substate_machines.append(self.allocator, .{ .init = init_state_i, .accept = .{ .NFA = init_state_i + 1 } });
    }
    pub fn add_set_complement(self: *RegexFSM) !void {
        var dtpl: DataTypePartitionList = .{};
        defer dtpl.deinit(self.allocator);
        try dtpl.add(self.allocator, .{ .range = .{ .min = 0, .max = 0xffff } });
        for (self.set_datatypes.items) |dt| try dtpl.delete(self.allocator, dt);
        const init_state_i: u32 = @intCast(self.states.items.len);
        try self.states.ensureUnusedCapacity(self.allocator, 2);
        self.states.appendSliceAssumeCapacity(&.{
            .{ .id = init_state_i },
            .{ .id = init_state_i + 1, .accept = true },
        });
        self.states.items[init_state_i].transitions = try self.allocator.alloc(Transition, dtpl.list.items.len);
        for (dtpl.list.items, 0..) |dt, i| self.states.items[init_state_i].transitions[i] = .{ .to = init_state_i + 1, .dtype = dt };
        self.set_datatypes.clearRetainingCapacity();
        try self.substate_machines.append(self.allocator, .{ .init = init_state_i, .accept = .{ .NFA = init_state_i + 1 } });
    }
    pub fn nfa_to_dfa(self: *RegexFSM) !void {
        var ssm = self.substate_machines.pop();
        std.debug.assert(ssm.accept == .NFA);
        const shift_ssm: u32 = ssm.accept.NFA - ssm.init + 1;
        const dfa_init: u32 = ssm.accept.NFA + 1;
        var powerset_hm: PowerSetHashMap = .{ .state_i = @intCast(self.states.items.len) };
        defer powerset_hm.deinit(self.allocator);
        var iterate_subsets = try RingBufferQueue([]const u32).init(self.allocator);
        defer iterate_subsets.deinit(self.allocator);
        var merged_accept_states: std.ArrayListUnmanaged(u32) = .{};
        errdefer merged_accept_states.deinit(self.allocator);
        const init_ss = try self.get_reachable_states(.NFA, &.{ssm.init});
        try powerset_hm.put(self.allocator, init_ss);
        try iterate_subsets.enqueue(self.allocator, init_ss);
        while (iterate_subsets.len() != 0) {
            const this_ss = iterate_subsets.dequeue_non_empty();
            const this_ss_sp = powerset_hm.get_properties(self, this_ss);
            if (this_ss_sp.accept) try merged_accept_states.append(self.allocator, this_ss_sp.id - shift_ssm); //Shift by size of NFA
            try self.states.append(self.allocator, .{ .id = this_ss_sp.id, .accept = this_ss_sp.accept, .transitions = try self.allocator.alloc(Transition, 0) });
            std.debug.print(ESC("{any} subset becomes .id {} ({s})\n", .{ 1, 30 }), .{ this_ss, this_ss_sp.id, if (this_ss_sp.accept) "accepted" else "non-accepted" });
            const dt_partitions = try self.get_partitions(this_ss);
            defer self.allocator.free(dt_partitions);
            for (dt_partitions) |dt_partition| {
                var reachable: SortedIntList(u32) = .{};
                defer reachable.deinit(self.allocator);
                for (this_ss) |state_i| {
                    const state = self.states.items[state_i];
                    switch (dt_partition) {
                        .none => continue,
                        .unicode, .char => |wc| {
                            const new_state = state.to_state(wc);
                            if (new_state != RegexState.ErrorState) _ = try reachable.insert_unique(self.allocator, new_state);
                        },
                        .range => |r| {
                            const tr_list = try state.to_states(self.allocator, r);
                            defer self.allocator.free(tr_list);
                            std.debug.assert(tr_list.len == 1); //get_keys should partition sets from this_ss so that it should point to 1 state.
                            if (tr_list[0].to != RegexState.ErrorState) _ = try reachable.insert_unique(self.allocator, tr_list[0].to);
                        },
                    }
                }
                if (reachable.list.items.len != 0) { //Add every non-error state transition and any new subset state to iterate.
                    const new_ss = try self.get_reachable_states(.NFA, reachable.list.items);
                    const putgp = try powerset_hm.put_get_properties(self.allocator, new_ss);
                    if (putgp.is_new) try iterate_subsets.enqueue(self.allocator, new_ss);
                    const this_ss_state = &self.states.items[this_ss_sp.id];
                    this_ss_state.transitions = try self.allocator.realloc(this_ss_state.transitions, this_ss_state.transitions.len + 1);
                    this_ss_state.transitions[this_ss_state.transitions.len - 1] = .{ .to = putgp.state, .dtype = dt_partition };
                }
            }
        }
        std.sort.block(u32, merged_accept_states.items, {}, index_sort);
        ssm.accept = .{ .DFA = try merged_accept_states.toOwnedSlice(self.allocator) };
        self.substate_machines.appendAssumeCapacity(ssm);
        try self.shift_left(true, dfa_init, shift_ssm);
        std.debug.print(ESC("Replaced NFA states with subset DFA states\n", .{ 32, 1 }), .{});
    }
    const MergeGroup = struct {
        P: u32,
        Q: u32,
        const HMContext = struct { //Context to make QP as PQ
            pub fn hash(_: HMContext, a: MergeGroup) u64 {
                const merge_group_number: [2]u32 = .{ @min(a.P, a.Q), @max(a.P, a.Q) };
                return @bitCast(merge_group_number);
            }
            pub fn eql(_: HMContext, a: MergeGroup, b: MergeGroup) bool {
                return (a.P == b.P and a.Q == b.Q) or (a.P == b.Q and a.Q == b.P);
            }
        };
        const HashMap = std.HashMapUnmanaged(MergeGroup, bool, MergeGroup.HMContext, std.hash_map.default_max_load_percentage);
        const PQSort = struct { //Q then P to merge the highest state numbers first.
            fn f(_: void, a: MergeGroup, b: MergeGroup) std.math.Order {
                const q_order = std.math.order(b.Q, a.Q);
                if (q_order != .eq) return q_order;
                return std.math.order(b.P, a.P);
            }
        }.f;
        const PQ = std.PriorityQueue(MergeGroup, void, MergeGroup.PQSort);
    };
    pub fn myhill_nerode(self: *RegexFSM) !void {
        var ssm = self.substate_machines.pop();
        std.debug.assert(ssm.accept == .DFA);
        errdefer ssm.deinit(self.allocator);
        const merged_states = try self.get_reachable_states(.DFA, &.{ RegexState.ErrorState, ssm.init });
        defer self.allocator.free(merged_states);
        const dt_partitions = try self.get_partitions(merged_states);
        defer self.allocator.free(dt_partitions);
        var pair_hm: MergeGroup.HashMap = .{};
        defer pair_hm.deinit(self.allocator);
        for (0..merged_states.len) |i| {
            for (i..merged_states.len) |j| {
                if (i == j) continue;
                const accept1 = self.states.items[merged_states[i]].accept;
                const accept2 = self.states.items[merged_states[j]].accept;
                try pair_hm.put(self.allocator, .{ .P = merged_states[i], .Q = merged_states[j] }, accept1 != accept2);
            }
        }
        std.debug.assert(pair_hm.size * 2 == (merged_states.len - 1) * (merged_states.len)); //Number of keys must be a triangle number.
        var pair_hm_it = pair_hm.iterator();
        var unmarked_pair_group: std.ArrayListUnmanaged(MergeGroup) = .{};
        defer unmarked_pair_group.deinit(self.allocator);
        while (pair_hm_it.next()) |kv|
            if (!kv.value_ptr.*) try unmarked_pair_group.append(self.allocator, kv.key_ptr.*);
        var has_marked: bool = undefined;
        while (true) {
            has_marked = false;
            next_unmarked: for (unmarked_pair_group.items) |upg| {
                const state_p = self.states.items[upg.P];
                const state_q = self.states.items[upg.Q];
                for (dt_partitions) |dtp| {
                    switch (dtp) {
                        .none => unreachable,
                        .unicode, .char => |wc| {
                            const P2 = state_p.to_state(wc);
                            const Q2 = state_q.to_state(wc);
                            if (pair_hm.get(.{ .P = P2, .Q = Q2 })) |marked| {
                                if (marked) {
                                    try pair_hm.put(self.allocator, upg, true);
                                    has_marked = true;
                                    continue :next_unmarked;
                                }
                            }
                        },
                        .range => |r| {
                            const P2tr = try state_p.to_states(self.allocator, r);
                            defer self.allocator.free(P2tr);
                            std.debug.assert(P2tr.len == 1);
                            const Q2tr = try state_q.to_states(self.allocator, r);
                            defer self.allocator.free(Q2tr);
                            std.debug.assert(Q2tr.len == 1);
                            if (pair_hm.get(.{ .P = P2tr[0].to, .Q = Q2tr[0].to })) |marked| {
                                if (marked) {
                                    try pair_hm.put(self.allocator, upg, true);
                                    has_marked = true;
                                    continue :next_unmarked;
                                }
                            }
                        },
                    }
                }
            }
            if (!has_marked) break; //Because keys are unordered, get new unmarked pairs and do algorithm again.
            pair_hm_it = pair_hm.iterator();
            unmarked_pair_group.clearRetainingCapacity();
            while (pair_hm_it.next()) |kv|
                if (!kv.value_ptr.*) try unmarked_pair_group.append(self.allocator, kv.key_ptr.*);
        }
        pair_hm_it = pair_hm.iterator();
        var pq_pairs: MergeGroup.PQ = MergeGroup.PQ.init(self.allocator, {});
        defer pq_pairs.deinit();
        while (pair_hm_it.next()) |kv|
            if (!kv.value_ptr.*)
                try pq_pairs.add(kv.key_ptr.*);
        var just_deleted: SortedIntList(u32) = .{};
        defer just_deleted.deinit(self.allocator);
        while (pq_pairs.removeOrNull()) |pair| {
            std.debug.print(ESC("Possible merge {} and {}\n", .{30}), .{ pair.P, pair.Q });
            if (!try just_deleted.insert_unique(self.allocator, pair.Q)) continue; //Don't merge Q state number twice.
            try self.merge_states(pair.P, pair.Q, merged_states[0 .. merged_states.len - just_deleted.list.items.len + 1]);
        }
        var new_accept: std.ArrayListUnmanaged(u32) = .{};
        defer new_accept.deinit(self.allocator);
        const merged_states2 = try self.get_reachable_states(.DFA, &.{ssm.init});
        defer self.allocator.free(merged_states2);
        for (merged_states2) |state_i| {
            try Transition.union_merge(&self.states.items[state_i].transitions, self.allocator);
            if (self.states.items[state_i].accept) try new_accept.append(self.allocator, state_i);
        }
        const new_accept_str = try new_accept.toOwnedSlice(self.allocator);
        ssm.deinit(self.allocator);
        ssm.accept = .{ .DFA = new_accept_str };
        self.substate_machines.appendAssumeCapacity(ssm);
        std.debug.print(ESC("DFA minimization complete\n", .{ 1, 32 }), .{});
        //TODO: Merge close ranges and points as a bigger range.
    }
    /// State and transitions are merged into state1_1.
    fn merge_states(self: *RegexFSM, state1_i: u32, state2_i: u32, merged_states: []u32) !void {
        std.debug.print(ESC("Merging states {} and {}\n", .{ 1, 33 }), .{ state1_i, state2_i });
        var delete_state_i: usize = undefined;
        for (merged_states, 0..) |state_i, delete_i| {
            if (state_i == state2_i) {
                delete_state_i = delete_i;
                continue;
            } else if (state_i == state1_i) {
                const state1 = &self.states.items[state1_i];
                const state2 = self.states.items[state2_i];
                const old_state1_tr_len = state1.transitions.len;
                state1.transitions = try self.allocator.realloc(state1.transitions, state1.transitions.len + state2.transitions.len);
                for (0..state2.transitions.len) |i| {
                    var edited_transition = state2.transitions[i];
                    if (edited_transition.to == state2_i) edited_transition.to = state1_i;
                    state1.transitions[old_state1_tr_len + i] = edited_transition;
                }
                try Transition.union_merge(&state1.transitions, self.allocator);
                for (state1.transitions) |*tr| {
                    if (tr.to == state2_i) tr.to = state1_i;
                    if (tr.to > state2_i) tr.to -= 1;
                }
            } else {
                const state = self.states.items[state_i];
                for (state.transitions) |*tr| {
                    if (tr.to == state2_i) tr.to = state1_i;
                    if (state_i < state2_i) { //shift_left already shifts the rightmost states by -1,
                        if (tr.to > state2_i) tr.to -= 1;
                    }
                }
            }
        }
        try self.shift_left(true, state2_i + 1, 1);
        merged_states[delete_state_i] = merged_states[merged_states.len - 1]; //Update array for future merges (No realloc, update slices.)
        for (merged_states) |*s| {
            if (s.* > state2_i) s.* -= 1;
        }
    }
    fn get_reachable_states(self: RegexFSM, comptime for_type: enum { NFA, DFA }, at_states: []const u32) ![]u32 {
        var set: SortedIntList(u32) = .{};
        errdefer set.deinit(self.allocator);
        var visit: std.ArrayListUnmanaged(u32) = .{};
        defer visit.deinit(self.allocator);
        for (at_states) |state| {
            try visit.append(self.allocator, state);
            _ = try set.insert_unique(self.allocator, state);
        }
        while (visit.items.len != 0) {
            const this_state = visit.pop();
            for (self.states.items[this_state].transitions) |tr| {
                if (for_type == .NFA) {
                    if (tr.dtype == .none and try set.insert_unique(self.allocator, tr.to))
                        try visit.append(self.allocator, tr.to);
                } else {
                    if (tr.dtype != .none and try set.insert_unique(self.allocator, tr.to))
                        try visit.append(self.allocator, tr.to);
                }
            }
        }
        return set.list.toOwnedSlice(self.allocator);
    }
    /// Gets all transitions of points/partitioned ranges sorted
    fn get_partitions(self: *RegexFSM, merged_states: []const u32) ![]DataType {
        var dtpl: DataTypePartitionList = .{};
        defer dtpl.deinit(self.allocator);
        for (merged_states) |state| {
            const check_state = self.states.items[state];
            for (check_state.transitions) |tr| if (tr.dtype != .none) try dtpl.add(self.allocator, tr.dtype);
        }
        return dtpl.list.toOwnedSlice(self.allocator);
    }
    fn index_sort(_: void, l: u32, r: u32) bool {
        return l < r;
    }
    fn shift_left(self: *RegexFSM, free_alloc: bool, offset: u32, by: u32) !void {
        for (offset..self.states.items.len) |i| { //Change .to and .id states for the ones that are shifted.
            for (self.states.items[i].transitions) |*tr| {
                if (tr.to >= offset) tr.to -= by;
            }
            self.states.items[i].id -= by;
            if (free_alloc)
                if (i - by < offset)
                    self.allocator.free(self.states.items[i - by].transitions);
            self.states.items[i - by] = self.states.items[i];
        }
        if (free_alloc)
            if (by > self.states.items.len - offset) //Between the new states position and below the old states position.
                for (self.states.items.len - by..offset) |i|
                    self.allocator.free(self.states.items[i].transitions);
        try self.states.resize(self.allocator, self.states.items.len - by);
    }
    /// States self.states.items[offset..offset+by] are uninitialized.
    fn shift_right(self: *RegexFSM, offset: u32, by: u32) !void {
        try self.states.resize(self.allocator, self.states.items.len + by);
        var i: usize = self.states.items.len - 2;
        while (i != offset - by) : (i -= 1) {
            for (self.states.items[i].transitions) |*tr| {
                if (tr.to >= offset) tr.to += by;
            }
            self.states.items[i].id += by;
            self.states.items[i + by] = self.states.items[i];
            self.states.items[i] = undefined;
        }
    }
    pub fn deinit(self: *RegexFSM) void {
        for (self.states.items) |state|
            self.allocator.free(state.transitions);
        self.states.deinit(self.allocator);
        for (self.substate_machines.items) |ssm|
            ssm.deinit(self.allocator);
        self.substate_machines.deinit(self.allocator);
        self.set_datatypes.deinit(self.allocator);
    }
};
const PowerSetHashMap = struct {
    const SetContext = struct {
        pub fn hash(_: SetContext, s: []const u32) u64 {
            return std.hash.Wyhash.hash(0, std.mem.asBytes(s));
        }
        pub fn eql(_: SetContext, a: []const u32, b: []const u32) bool {
            return std.mem.eql(u32, a, b);
        }
    };
    /// K := Set of merged states (Assume sorted), V := Index of the merged states
    const SetHashMap = std.HashMapUnmanaged([]const u32, u32, SetContext, std.hash_map.default_max_load_percentage);
    hm: SetHashMap = .{},
    state_i: u32,
    ///Function takes memory ownership of `set`
    fn put(self: *PowerSetHashMap, allocator: std.mem.Allocator, set: []const u32) !void {
        errdefer allocator.free(set);
        try self.hm.put(allocator, set, self.state_i);
        self.state_i += 1;
    }
    const GetProperties = struct {
        state: u32,
        is_new: bool,
    };
    ///Function takes memory ownership of `set`
    fn put_get_properties(self: *PowerSetHashMap, allocator: std.mem.Allocator, set: []const u32) !GetProperties {
        errdefer allocator.free(set);
        const gop = try self.hm.getOrPut(allocator, set);
        if (gop.found_existing) {
            allocator.free(set);
            return .{ .state = gop.value_ptr.*, .is_new = false };
        }
        gop.value_ptr.* = self.state_i;
        self.state_i += 1;
        return .{ .state = self.state_i - 1, .is_new = true };
    }
    const StateProperties = struct {
        id: u32,
        accept: bool,
    };
    ///Assuming set already exists.
    fn get_properties(self: PowerSetHashMap, rfsm: *const RegexFSM, set: []const u32) StateProperties {
        return .{
            .id = self.hm.get(set).?,
            .accept = for (set) |state| {
                if (rfsm.states.items[state].accept) break true;
            } else false,
        };
    }
    fn deinit(self: *PowerSetHashMap, allocator: std.mem.Allocator) void {
        var hm_it = self.hm.keyIterator();
        while (hm_it.next()) |k| allocator.free(k.*);
        self.hm.deinit(allocator);
    }
};
/// This sorts Datatypes (excluding .none) and partition ranges into smaller ranges.
const DataTypePartitionList = struct {
    list: std.ArrayListUnmanaged(DataType) = .{},
    fn add(self: *DataTypePartitionList, allocator: std.mem.Allocator, value: DataType) !void {
        if (value == .none) return;
        if (self.list.items.len != 0) {
            var low_i: usize = 0;
            var high_i: usize = self.list.items.len - 1;
            var mid_i = high_i / 2;
            while (high_i >= low_i) : (mid_i = (high_i + low_i) / 2) {
                range_next_mid_i: {
                    switch (value) { //If value point/range intersects a point/range, exclude/partition it.
                        .none => unreachable,
                        .unicode, .char => |wc| {
                            switch (self.list.items[mid_i]) {
                                .none => {},
                                .unicode, .char => |wc2| if (wc == wc2) return, //No duplicates
                                .range => |r2| if (r2.intersect_split(.{ .min = wc, .max = wc })) |range_split| { //Partition into smaller ranges.
                                    if (range_split.l != null and range_split.u != null) {
                                        try self.list.replaceRange(allocator, mid_i, 1, &.{
                                            DataType.r_or_un(range_split.l.?),
                                            value,
                                            DataType.r_or_un(range_split.u.?),
                                        });
                                    } else if (range_split.l != null and range_split.u == null) {
                                        try self.list.replaceRange(allocator, mid_i, 1, &.{
                                            DataType.r_or_un(range_split.l.?),
                                            value,
                                        });
                                    } else if (range_split.l == null and range_split.u != null) {
                                        try self.list.replaceRange(allocator, mid_i, 1, &.{
                                            value,
                                            DataType.r_or_un(range_split.u.?),
                                        });
                                    }
                                    return;
                                },
                            }
                        },
                        .range => |r| {
                            var has_partitioned: bool = false;
                            var leftover_l: ?Range(u16) = null; //Once partitioned, check any leftover ranges with other DataTypes around mid_i if it can be further partitioned.
                            var leftover_u: ?Range(u16) = null;
                            switch (self.list.items[mid_i]) {
                                .none => {},
                                .unicode, .char => |wc2| if (r.intersect_split(.{ .min = wc2, .max = wc2 })) |range_split| {
                                    leftover_l = range_split.l;
                                    leftover_u = range_split.u;
                                    has_partitioned = true;
                                },
                                .range => |r2| if (r.union_split(r2)) |range_split| {
                                    leftover_l = range_split.l;
                                    leftover_u = range_split.u;
                                    self.list.items[mid_i] = DataType.r_or_un(range_split.i);
                                    has_partitioned = true;
                                },
                            }
                            if (!has_partitioned) break :range_next_mid_i;
                            var i_ptr = mid_i;
                            while (leftover_u) |lu| {
                                i_ptr += 1;
                                if (i_ptr == self.list.items.len) {
                                    try self.list.append(allocator, DataType.r_or_un(lu));
                                    break;
                                }
                                switch (self.list.items[i_ptr]) {
                                    .none => continue,
                                    .unicode, .char => |wc2| if (lu.intersect_split(.{ .min = wc2, .max = wc2 })) |range_split| {
                                        leftover_u = range_split.u;
                                        if (range_split.l) |ll| {
                                            try self.list.insert(allocator, i_ptr, DataType.r_or_un(ll));
                                            i_ptr += 1;
                                        }
                                    } else { //Break early if there are no more intersections
                                        try self.list.insert(allocator, i_ptr, DataType.r_or_un(lu));
                                        break;
                                    },
                                    .range => |r2| if (lu.union_split(r2)) |range_split| {
                                        leftover_u = range_split.u;
                                        self.list.items[i_ptr] = DataType.r_or_un(range_split.i);
                                        if (range_split.l) |ll| {
                                            try self.list.insert(allocator, i_ptr, DataType.r_or_un(ll));
                                            i_ptr += 1;
                                        }
                                    } else {
                                        try self.list.insert(allocator, i_ptr, DataType.r_or_un(lu));
                                        break;
                                    },
                                }
                            }
                            i_ptr = mid_i;
                            while (leftover_l) |ll| {
                                if (i_ptr == 0) {
                                    try self.list.insert(allocator, 0, DataType.r_or_un(ll));
                                    break;
                                }
                                i_ptr -= 1;
                                switch (self.list.items[i_ptr]) {
                                    .none => continue,
                                    .unicode, .char => |wc2| if (ll.intersect_split(.{ .min = wc2, .max = wc2 })) |range_split| {
                                        leftover_l = range_split.l;
                                        if (range_split.u) |lu|
                                            try self.list.insert(allocator, i_ptr + 1, DataType.r_or_un(lu));
                                    } else {
                                        try self.list.insert(allocator, i_ptr + 1, DataType.r_or_un(ll));
                                        break;
                                    },
                                    .range => |r2| if (ll.union_split(r2)) |range_split| {
                                        leftover_l = range_split.l;
                                        self.list.items[i_ptr] = DataType.r_or_un(range_split.i);
                                        if (range_split.u) |lu|
                                            try self.list.insert(allocator, i_ptr + 1, DataType.r_or_un(lu));
                                    } else {
                                        try self.list.insert(allocator, i_ptr + 1, DataType.r_or_un(ll));
                                        break;
                                    },
                                }
                            }
                            return;
                        },
                    }
                }
                if (DataType.lt({}, self.list.items[mid_i], value)) {
                    low_i = mid_i + 1;
                } else {
                    if (mid_i != 0) {
                        high_i = mid_i - 1;
                    } else { //Insert at left end
                        try self.list.insert(allocator, 0, value);
                        return;
                    }
                }
            }
            try self.list.insert(allocator, mid_i + 1, value); //Insert at right of mid_i
        } else {
            try self.list.append(allocator, value);
        }
    }
    /// Similar algorithm as add, but partitions and removes the given point/range values.
    fn delete(self: *DataTypePartitionList, allocator: std.mem.Allocator, value: DataType) !void {
        if (value == .none) return;
        if (self.list.items.len == 0) return;
        var low_i: usize = 0;
        var high_i: usize = self.list.items.len - 1;
        var mid_i = high_i / 2;
        while (high_i >= low_i) : (mid_i = (high_i + low_i) / 2) {
            range_next_mid_i: {
                switch (value) {
                    .none => unreachable,
                    .unicode, .char => |wc| {
                        switch (self.list.items[mid_i]) {
                            .none => {},
                            .unicode, .char => |wc2| if (wc == wc2) {
                                try self.list.replaceRange(allocator, mid_i, 1, &.{});
                                return;
                            },
                            .range => |r2| if (r2.intersect_split(.{ .min = wc, .max = wc })) |range_split| {
                                if (range_split.l != null and range_split.u != null) {
                                    try self.list.replaceRange(allocator, mid_i, 1, &.{
                                        DataType.r_or_un(range_split.l.?),
                                        DataType.r_or_un(range_split.u.?),
                                    });
                                } else if (range_split.l != null and range_split.u == null) {
                                    self.list.items[mid_i] = DataType.r_or_un(range_split.l.?);
                                } else if (range_split.l == null and range_split.u != null) {
                                    self.list.items[mid_i] = DataType.r_or_un(range_split.u.?);
                                }
                                return;
                            },
                        }
                    },
                    .range => |r| {
                        var has_partitioned: bool = false;
                        var leftover_l: ?Range(u16) = null;
                        var leftover_u: ?Range(u16) = null;
                        switch (self.list.items[mid_i]) {
                            .none => {},
                            .unicode, .char => |wc2| if (r.intersect_split(.{ .min = wc2, .max = wc2 })) |range_split| {
                                leftover_l = range_split.l;
                                leftover_u = range_split.u;
                                try self.list.replaceRange(allocator, mid_i, 1, &.{});
                                has_partitioned = true;
                            },
                            .range => |r2| if (r2.contains_range(r)) {
                                const range_split = r2.intersect_split(r).?;
                                if (range_split.l != null and range_split.u != null) {
                                    try self.list.replaceRange(allocator, mid_i, 1, &.{
                                        DataType.r_or_un(range_split.l.?),
                                        DataType.r_or_un(range_split.u.?),
                                    });
                                } else if (range_split.l != null and range_split.u == null) {
                                    self.list.items[mid_i] = DataType.r_or_un(range_split.l.?);
                                } else if (range_split.l == null and range_split.u != null) {
                                    self.list.items[mid_i] = DataType.r_or_un(range_split.u.?);
                                } else try self.list.replaceRange(allocator, mid_i, 1, &.{});
                                return;
                            } else if (r.union_split(r2)) |range_split| {
                                leftover_l = range_split.l;
                                leftover_u = range_split.u;
                                try self.list.replaceRange(allocator, mid_i, 1, &.{});
                                has_partitioned = true;
                            },
                        }
                        if (!has_partitioned) break :range_next_mid_i;
                        while (leftover_u) |lu| {
                            if (mid_i == self.list.items.len) break;
                            switch (self.list.items[mid_i]) {
                                .none => continue,
                                .unicode, .char => |wc2| if (lu.intersect_split(.{ .min = wc2, .max = wc2 })) |range_split| {
                                    leftover_u = range_split.u;
                                    try self.list.replaceRange(allocator, mid_i, 1, &.{});
                                } else break,
                                .range => |r2| if (lu.union_split(r2)) |range_split| {
                                    if (lu.max >= r2.max) {
                                        leftover_u = range_split.u;
                                        try self.list.replaceRange(allocator, mid_i, 1, &.{});
                                    } else { //Deleted range partially.
                                        self.list.items[mid_i] = DataType.r_or_un(range_split.u.?);
                                        break;
                                    }
                                } else break,
                            }
                        }
                        var i_ptr = mid_i;
                        while (leftover_l) |ll| {
                            if (i_ptr == 0) return;
                            i_ptr -= 1;
                            switch (self.list.items[i_ptr]) {
                                .none => continue,
                                .unicode, .char => |wc2| if (ll.intersect_split(.{ .min = wc2, .max = wc2 })) |range_split| {
                                    leftover_l = range_split.l;
                                    try self.list.replaceRange(allocator, i_ptr, 1, &.{});
                                } else break,
                                .range => |r2| if (ll.union_split(r2)) |range_split| {
                                    if (ll.min <= r2.min) {
                                        leftover_l = range_split.l;
                                        try self.list.replaceRange(allocator, i_ptr, 1, &.{});
                                    } else {
                                        self.list.items[i_ptr] = DataType.r_or_un(range_split.l.?);
                                        break;
                                    }
                                } else break,
                            }
                        }
                        return;
                    },
                }
            }
            if (DataType.lt({}, self.list.items[mid_i], value)) {
                low_i = mid_i + 1;
            } else {
                if (mid_i != 0) {
                    high_i = mid_i - 1;
                } else return;
            }
        }
    }

    fn deinit(self: *DataTypePartitionList, allocator: std.mem.Allocator) void {
        self.list.deinit(allocator);
    }
    const t_allocator = std.testing.allocator;
    test "DataTypePartitionList add char/unicode in order" {
        var dtpl: DataTypePartitionList = .{};
        defer dtpl.deinit(t_allocator);
        try dtpl.add(t_allocator, .{ .char = 1 });
        try dtpl.add(t_allocator, .{ .unicode = 3 });
        try dtpl.add(t_allocator, .{ .char = 2 });
        try dtpl.add(t_allocator, .{ .unicode = 0 });
        try std.testing.expectEqualSlices(
            DataType,
            &[_]DataType{ .{ .unicode = 0 }, .{ .char = 1 }, .{ .char = 2 }, .{ .unicode = 3 } },
            dtpl.list.items,
        );
    }
    test "DataTypePartitionList add range in order" {
        var dtpl: DataTypePartitionList = .{};
        defer dtpl.deinit(t_allocator);
        const r1: DataType = .{ .range = .{ .min = 21, .max = 30 } };
        const r2: DataType = .{ .range = .{ .min = 31, .max = 40 } };
        const r3: DataType = .{ .range = .{ .min = 1, .max = 10 } };
        const r4: DataType = .{ .range = .{ .min = 11, .max = 20 } };
        try dtpl.add(t_allocator, r1);
        try dtpl.add(t_allocator, r2);
        try dtpl.add(t_allocator, r3);
        try dtpl.add(t_allocator, r4);
        try std.testing.expectEqualSlices(
            DataType,
            &[_]DataType{ r3, r4, r1, r2 },
            dtpl.list.items,
        );
    }
    test "DataTypePartitionList add range/char/unicode in order" {
        var dtpl: DataTypePartitionList = .{};
        defer dtpl.deinit(t_allocator);
        const r1: DataType = .{ .char = 'e' };
        const r2: DataType = .{ .range = .{ .min = 'n', .max = 'p' } };
        const r3: DataType = .{ .range = .{ .min = 'a', .max = 'b' } };
        const r4: DataType = .{ .unicode = 'g' };
        const r5: DataType = .{ .range = .{ .min = 'i', .max = 'l' } };
        try dtpl.add(t_allocator, r1);
        try dtpl.add(t_allocator, r2);
        try dtpl.add(t_allocator, r3);
        try dtpl.add(t_allocator, r4);
        try dtpl.add(t_allocator, r5);
        try std.testing.expectEqualSlices(
            DataType,
            &[_]DataType{ r3, r1, r4, r5, r2 },
            dtpl.list.items,
        );
    }
    test "DataTypePartitionList add point partition range" {
        var dtpl: DataTypePartitionList = .{};
        defer dtpl.deinit(t_allocator);
        try dtpl.add(t_allocator, .{ .range = .{ .min = 'a', .max = 'z' } });
        try dtpl.add(t_allocator, .{ .char = 'm' });
        try dtpl.add(t_allocator, .{ .char = 'm' }); //Duplicates are not added as char/unicode
        try dtpl.add(t_allocator, .{ .unicode = 'm' });
        try dtpl.add(t_allocator, .{ .unicode = 't' });
        try dtpl.add(t_allocator, .{ .unicode = 't' });
        try dtpl.add(t_allocator, .{ .char = 't' });
        try std.testing.expectEqualSlices(
            DataType,
            &[_]DataType{
                .{ .range = .{ .min = 'a', .max = 'l' } },
                .{ .char = 'm' },
                .{ .range = .{ .min = 'n', .max = 's' } },
                .{ .unicode = 't' },
                .{ .range = .{ .min = 'u', .max = 'z' } },
            },
            dtpl.list.items,
        );
    }
    test "DataTypePartitionList add range partition point" {
        var dtpl: DataTypePartitionList = .{};
        defer dtpl.deinit(t_allocator);
        try dtpl.add(t_allocator, .{ .char = 'a' - 1 });
        try dtpl.add(t_allocator, .{ .char = 'm' });
        try dtpl.add(t_allocator, .{ .char = 'c' });
        try dtpl.add(t_allocator, .{ .unicode = 'x' });
        try dtpl.add(t_allocator, .{ .char = 'z' + 1 });
        try dtpl.add(t_allocator, .{ .range = .{ .min = 'a', .max = 'z' } });
        try std.testing.expectEqualSlices(
            DataType,
            &[_]DataType{
                .{ .char = 'a' - 1 },
                .{ .range = .{ .min = 'a', .max = 'b' } },
                .{ .char = 'c' },
                .{ .range = .{ .min = 'd', .max = 'l' } },
                .{ .char = 'm' },
                .{ .range = .{ .min = 'n', .max = 'w' } },
                .{ .unicode = 'x' },
                .{ .range = .{ .min = 'y', .max = 'z' } },
                .{ .char = 'z' + 1 },
            },
            dtpl.list.items,
        );
    }
    test "DataTypePartitionList add range partition range and single letter ranges become unicode" {
        var dtpl: DataTypePartitionList = .{};
        defer dtpl.deinit(t_allocator);
        try dtpl.add(t_allocator, .{ .range = .{ .min = 'a', .max = 'b' } });
        try dtpl.add(t_allocator, .{ .range = .{ .min = 'c', .max = 'e' } });
        try dtpl.add(t_allocator, .{ .range = .{ .min = 'g', .max = 'i' } });
        try dtpl.add(t_allocator, .{ .range = .{ .min = 'j', .max = 'm' } });
        try dtpl.add(t_allocator, .{ .range = .{ .min = 'n', .max = 'o' } });
        try dtpl.add(t_allocator, .{ .range = .{ .min = 'd', .max = 'l' } });
        try std.testing.expectEqualSlices(
            DataType,
            &[_]DataType{
                .{ .range = .{ .min = 'a', .max = 'b' } },
                .{ .unicode = 'c' },
                .{ .range = .{ .min = 'd', .max = 'e' } },
                .{ .unicode = 'f' },
                .{ .range = .{ .min = 'g', .max = 'i' } },
                .{ .range = .{ .min = 'j', .max = 'l' } },
                .{ .unicode = 'm' },
                .{ .range = .{ .min = 'n', .max = 'o' } },
            },
            dtpl.list.items,
        );
    }
    test "DataTypePartitionList delete point from points and ranges" {
        var dtpl: DataTypePartitionList = .{};
        defer dtpl.deinit(t_allocator);
        try dtpl.add(t_allocator, .{ .char = 'a' });
        try dtpl.add(t_allocator, .{ .char = 'b' });
        try dtpl.add(t_allocator, .{ .char = 'c' });
        try dtpl.add(t_allocator, .{ .range = .{ .min = 'd', .max = 'h' } });
        try dtpl.delete(t_allocator, .{ .char = 'a' });
        try dtpl.delete(t_allocator, .{ .char = 'c' });
        try dtpl.delete(t_allocator, .{ .char = 'e' });
        try dtpl.delete(t_allocator, .{ .char = 'h' });
        try dtpl.delete(t_allocator, .{ .char = 'f' });
        try dtpl.delete(t_allocator, .{ .char = 'b' });
        try dtpl.delete(t_allocator, .{ .char = 'd' });
        try dtpl.delete(t_allocator, .{ .char = 'g' });
        try dtpl.delete(t_allocator, .{ .char = 'g' });
        try std.testing.expectEqualSlices(
            DataType,
            &[_]DataType{},
            dtpl.list.items,
        );
    }
    test "DataTypePartitionList delete range contained in range exact" {
        var dtpl: DataTypePartitionList = .{};
        defer dtpl.deinit(t_allocator);
        try dtpl.add(t_allocator, .{ .range = .{ .min = 'A', .max = 'Z' } });
        try dtpl.add(t_allocator, .{ .range = .{ .min = 'a', .max = 'z' } });
        try dtpl.delete(t_allocator, .{ .range = .{ .min = 'a', .max = 'z' } });
        try std.testing.expectEqualSlices(
            DataType,
            &[_]DataType{
                .{ .range = .{ .min = 'A', .max = 'Z' } },
            },
            dtpl.list.items,
        );
    }
    test "DataTypePartitionList delete range contained in range" {
        var dtpl: DataTypePartitionList = .{};
        defer dtpl.deinit(t_allocator);
        try dtpl.add(t_allocator, .{ .range = .{ .min = 'a', .max = 'z' } });
        try dtpl.delete(t_allocator, .{ .range = .{ .min = 'm', .max = 'n' } });
        try std.testing.expectEqualSlices(
            DataType,
            &[_]DataType{
                .{ .range = .{ .min = 'a', .max = 'l' } },
                .{ .range = .{ .min = 'o', .max = 'z' } },
            },
            dtpl.list.items,
        );
    }
    test "DataTypePartitionList delete range not contained in range" {
        var dtpl: DataTypePartitionList = .{};
        defer dtpl.deinit(t_allocator);
        try dtpl.add(t_allocator, .{ .range = .{ .min = 'm', .max = 'n' } });
        try dtpl.delete(t_allocator, .{ .range = .{ .min = 'a', .max = 'z' } });
        try std.testing.expectEqualSlices(
            DataType,
            &[_]DataType{},
            dtpl.list.items,
        );
    }
    test "DataTypePartitionList delete range with range first and then points" {
        var dtpl: DataTypePartitionList = .{};
        defer dtpl.deinit(t_allocator);
        try dtpl.add(t_allocator, .{ .char = 'a' });
        try dtpl.add(t_allocator, .{ .char = 'b' });
        try dtpl.add(t_allocator, .{ .char = 'o' });
        try dtpl.add(t_allocator, .{ .char = 'z' });
        try dtpl.add(t_allocator, .{ .range = .{ .min = 'm', .max = 'n' } });
        try dtpl.delete(t_allocator, .{ .range = .{ .min = 'b', .max = 'y' } });
        try std.testing.expectEqualSlices(
            DataType,
            &[_]DataType{
                .{ .char = 'a' },
                .{ .char = 'z' },
            },
            dtpl.list.items,
        );
    }
    test "DataTypePartitionList delete range with range first and then points end of lists" {
        var dtpl: DataTypePartitionList = .{};
        defer dtpl.deinit(t_allocator);
        try dtpl.add(t_allocator, .{ .char = 'a' });
        try dtpl.add(t_allocator, .{ .char = 'b' });
        try dtpl.add(t_allocator, .{ .char = 'o' });
        try dtpl.add(t_allocator, .{ .char = 'z' });
        try dtpl.add(t_allocator, .{ .range = .{ .min = 'm', .max = 'n' } });
        try dtpl.delete(t_allocator, .{ .range = .{ .min = 'a', .max = 'z' } });
        try std.testing.expectEqualSlices(
            DataType,
            &[_]DataType{},
            dtpl.list.items,
        );
    }
    test "DataTypePartitionList delete range with all ranges" {
        var dtpl: DataTypePartitionList = .{};
        defer dtpl.deinit(t_allocator);
        try dtpl.add(t_allocator, .{ .range = .{ .min = 'a', .max = 'c' } });
        try dtpl.add(t_allocator, .{ .range = .{ .min = 'j', .max = 'k' } });
        try dtpl.add(t_allocator, .{ .range = .{ .min = 'm', .max = 'n' } });
        try dtpl.add(t_allocator, .{ .range = .{ .min = 'p', .max = 't' } });
        try dtpl.add(t_allocator, .{ .range = .{ .min = 'x', .max = 'z' } });
        try dtpl.delete(t_allocator, .{ .range = .{ .min = 'd', .max = 'w' } });
        try std.testing.expectEqualSlices(
            DataType,
            &[_]DataType{
                .{ .range = .{ .min = 'a', .max = 'c' } },
                .{ .range = .{ .min = 'x', .max = 'z' } },
            },
            dtpl.list.items,
        );
    }
    test "DataTypePartitionList delete range with all ranges boundaries" {
        var dtpl: DataTypePartitionList = .{};
        defer dtpl.deinit(t_allocator);
        try dtpl.add(t_allocator, .{ .range = .{ .min = 'a', .max = 'c' } });
        try dtpl.add(t_allocator, .{ .range = .{ .min = 'j', .max = 'k' } });
        try dtpl.add(t_allocator, .{ .range = .{ .min = 'm', .max = 'n' } });
        try dtpl.add(t_allocator, .{ .range = .{ .min = 'p', .max = 't' } });
        try dtpl.add(t_allocator, .{ .range = .{ .min = 'x', .max = 'z' } });
        try dtpl.delete(t_allocator, .{ .range = .{ .min = 'a', .max = 'z' } });
        try std.testing.expectEqualSlices(
            DataType,
            &[_]DataType{},
            dtpl.list.items,
        );
    }
    test "DataTypePartitionList delete range with all ranges partial" {
        var dtpl: DataTypePartitionList = .{};
        defer dtpl.deinit(t_allocator);
        try dtpl.add(t_allocator, .{ .range = .{ .min = 'a', .max = 'c' } });
        try dtpl.add(t_allocator, .{ .range = .{ .min = 'j', .max = 'k' } });
        try dtpl.add(t_allocator, .{ .range = .{ .min = 'm', .max = 'n' } });
        try dtpl.add(t_allocator, .{ .range = .{ .min = 'p', .max = 't' } });
        try dtpl.add(t_allocator, .{ .range = .{ .min = 'x', .max = 'z' } });
        try dtpl.delete(t_allocator, .{ .range = .{ .min = 'b', .max = 'x' } });
        try std.testing.expectEqualSlices(
            DataType,
            &[_]DataType{
                .{ .unicode = 'a' },
                .{ .range = .{ .min = 'y', .max = 'z' } },
            },
            dtpl.list.items,
        );
    }
};
comptime {
    _ = DataTypePartitionList;
}
pub fn input_string(init: u32, ctx: *RegexStateContext, str: []const u8) *const RegexState {
    std.debug.assert(init != 0);
    var state = &ctx.array[init];
    ctx.str = str[0..0];
    for (str) |ch| {
        state = state.to_state_ptr(ctx, ch);
        ctx.str.len += 1;
    }
    return state;
}
test "RegexState" {
    var rfsm: RegexFSM = try RegexFSM.init(std.testing.allocator);
    defer rfsm.deinit();
    try rfsm.add_datatype(.{ .char = 'a' });
    try rfsm.add_datatype(.{ .char = 'b' });
    try rfsm.alternation();
    try rfsm.add_datatype(.{ .char = 'c' });
    try rfsm.alternation();
    try rfsm.add_datatype(.{ .char = 'd' });
    try rfsm.add_datatype(.{ .char = 'e' });
    try rfsm.alternation();
    try rfsm.add_datatype(.{ .char = 'f' });
    try rfsm.alternation();
    try rfsm.concatenation();
    try rfsm.add_datatype(.{ .char = 'g' });
    try rfsm.add_datatype(.{ .char = 'h' });
    try rfsm.alternation();
    try rfsm.add_datatype(.{ .char = 'i' });
    try rfsm.alternation();
    try rfsm.concatenation();
    try rfsm.add_datatype(.{ .char = 'j' });
    try rfsm.add_datatype(.{ .char = 'k' });
    try rfsm.alternation();
    try rfsm.add_datatype(.{ .char = 'l' });
    try rfsm.alternation();
    try rfsm.concatenation();
    std.debug.print(ESC("Sub state machines: {any}\n", .{1}), .{rfsm.substate_machines.items});
    for (rfsm.states.items) |state| std.debug.print("{}\n", .{state});
    try rfsm.nfa_to_dfa();
    std.debug.print(ESC("Sub state machines: {any}\n", .{1}), .{rfsm.substate_machines.items});
    for (rfsm.states.items) |state| std.debug.print("{}\n", .{state});
    try rfsm.myhill_nerode();
    std.debug.print(ESC("Sub state machines: {any}\n", .{1}), .{rfsm.substate_machines.items});
    for (rfsm.states.items) |state| std.debug.print("{}\n", .{state});
    //var rsctx: RegexStateContext = .{ .array = rfsm.states.items };
    //std.debug.print("States: {any}\nState now: {any}\n", .{
    //    rfsm.states.items,
    //    input_string(1, &rsctx, "B"),
    //});
}
