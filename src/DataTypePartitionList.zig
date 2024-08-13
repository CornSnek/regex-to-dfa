const std = @import("std");
const DataType = @import("regex_fsm.zig").DataType;
const Range = @import("range.zig").Range;
const DataTypePartitionList = @This();
list: std.ArrayListUnmanaged(DataType) = .{},
pub fn add(self: *DataTypePartitionList, allocator: std.mem.Allocator, value: DataType) !void {
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
pub fn delete(self: *DataTypePartitionList, allocator: std.mem.Allocator, value: DataType) !void {
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
pub fn deinit(self: *DataTypePartitionList, allocator: std.mem.Allocator) void {
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
