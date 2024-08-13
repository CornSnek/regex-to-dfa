const std = @import("std");
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
