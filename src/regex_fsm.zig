const std = @import("std");
const ESC = @import("main.zig").ESC;
const SortedList = @import("sorted_list.zig").SortedList;
const SortedIntList = @import("sorted_list.zig").SortedIntList;
const RingBuffer = @import("ring_buffer.zig").RingBuffer;
const Range = @import("range.zig").Range;
/// This sorts Datatypes (excluding .none) and partition ranges into smaller ranges.
const DataTypePartitionList = @import("DataTypePartitionList.zig");
test {
    _ = DataTypePartitionList;
}
pub const DataType = union(enum) {
    none: void,
    char: u8,
    unicode: u16,
    range: Range(u16),
    pub fn r_or_un(range: Range(u16)) DataType {
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
                cmp_tr = cmp_next_tr;
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
pub const CharacterSet = enum {
    @".",
    whitespace,
    nonwhitespace,
    digit,
    nondigit,
    word,
    nonword,
    pub fn datatypes(self: CharacterSet) []const DataType {
        return switch (self) {
            .@"." => &[_]DataType{
                .{ .range = .{ .min = 0, .max = 0xffff } },
            },
            .whitespace => &[_]DataType{
                .{ .range = .{ .min = 9, .max = 13 } },
                .{ .char = ' ' },
            },
            .nonwhitespace => &[_]DataType{
                .{ .range = .{ .min = 0, .max = 8 } },
                .{ .range = .{ .min = 14, .max = ' ' - 1 } },
                .{ .range = .{ .min = ' ' + 1, .max = 0xffff } },
            },
            .digit => &[_]DataType{
                .{ .range = .{ .min = '0', .max = '9' } },
            },
            .nondigit => &[_]DataType{
                .{ .range = .{ .min = 0, .max = '0' - 1 } },
                .{ .range = .{ .min = '9' + 1, .max = 0xffff } },
            },
            .word => &[_]DataType{
                .{ .range = .{ .min = '0', .max = '9' } },
                .{ .range = .{ .min = 'A', .max = 'Z' } },
                .{ .char = '_' },
                .{ .range = .{ .min = 'a', .max = 'z' } },
            },
            .nonword => &[_]DataType{
                .{ .range = .{ .min = 0, .max = '0' - 1 } },
                .{ .range = .{ .min = '9' + 1, .max = 'A' - 1 } },
                .{ .range = .{ .min = 'Z' + 1, .max = '_' - 1 } },
                .{ .range = .{ .min = '_' + 1, .max = 'a' - 1 } },
                .{ .range = .{ .min = 'z' + 1, .max = 0xffff } },
            },
        };
    }
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
        var iterate_subsets = try RingBuffer([]const u32).init(self.allocator);
        defer iterate_subsets.deinit(self.allocator);
        var merged_accept_states: std.ArrayListUnmanaged(u32) = .{};
        errdefer merged_accept_states.deinit(self.allocator);
        const init_ss = try self.get_reachable_states(.NFA, &.{ssm.init});
        try powerset_hm.put(self.allocator, init_ss);
        try iterate_subsets.tail_push(self.allocator, init_ss);
        while (iterate_subsets.len() != 0) {
            const this_ss = iterate_subsets.head_pop_non_empty();
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
                    if (putgp.is_new) try iterate_subsets.tail_push(self.allocator, new_ss);
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
    fn _finish_dfa_minimization(self: *RegexFSM, ssm: *SubStateMachine) !void {
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
        self.substate_machines.appendAssumeCapacity(ssm.*);
        std.debug.print(ESC("DFA minimization complete\n", .{ 1, 32 }), .{});
    }
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
        try self._finish_dfa_minimization(&ssm);
    }
    const EquivalenceClass = struct {
        group: u32,
        state: u32,
        accept: bool,
        pub fn group_then_state_sort(_: void, a: EquivalenceClass, b: EquivalenceClass) bool {
            if (a.group != b.group) return a.group < b.group;
            return a.state < b.state;
        }
        pub fn highest_state_sort(_: void, a: EquivalenceClass, b: EquivalenceClass) bool {
            return a.state > b.state;
        }
    };
    pub fn hopcroft_algorithm(self: *RegexFSM) !void {
        var ssm = self.substate_machines.pop();
        std.debug.assert(ssm.accept == .DFA);
        errdefer ssm.deinit(self.allocator);
        const merged_states = try self.get_reachable_states(.DFA, &.{ RegexState.ErrorState, ssm.init });
        defer self.allocator.free(merged_states);
        const dt_partitions = try self.get_partitions(merged_states);
        defer self.allocator.free(dt_partitions);
        var eq_classes = try self.allocator.alloc(EquivalenceClass, merged_states.len);
        defer self.allocator.free(eq_classes);
        for (merged_states, 0..) |state, i| {
            var ec: EquivalenceClass = undefined;
            ec.state = state;
            ec.group = @as(u32, @intFromBool(self.states.items[state].accept));
            ec.accept = self.states.items[state].accept;
            eq_classes[i] = ec;
        }
        {
            var group_i: u32 = 2;
            var eq_structs = try EQStructs.init(self, eq_classes, dt_partitions, group_i);
            defer eq_structs.deinit(self.allocator);
            var has_changed: bool = true;
            while (has_changed) {
                has_changed = false;
                try eq_structs.update_first_i_arr(self.allocator, eq_classes, group_i);
                for (0..group_i) |group_ptr| {
                    const offset = eq_structs.first_i_arr.items[group_ptr];
                    const pivot_eq = eq_classes[offset];
                    var i: usize = 1;
                    while (offset + i != eq_classes.len) : (i += 1) {
                        const cmp_eq = eq_classes[offset + i];
                        if (pivot_eq.group != cmp_eq.group) break;
                        const cmp_state = self.states.items[cmp_eq.state];
                        const gop_get = try eq_structs.transitions_cache.getOrPut(self.allocator, cmp_eq.state);
                        var cmp_transition: []u32 = undefined;
                        if (gop_get.found_existing) {
                            cmp_transition = gop_get.value_ptr.*;
                        } else {
                            cmp_transition = try self.allocator.alloc(u32, dt_partitions.len);
                            for (dt_partitions, 0..) |dtp, dt_i| {
                                switch (dtp) {
                                    .none => unreachable,
                                    .unicode, .char => |wc| {
                                        cmp_transition[dt_i] = cmp_state.to_state(wc);
                                    },
                                    .range => |r| {
                                        const trlist = try cmp_state.to_states(self.allocator, r);
                                        defer self.allocator.free(trlist);
                                        std.debug.assert(trlist.len == 1);
                                        cmp_transition[dt_i] = trlist[0].to;
                                    },
                                }
                            }
                            gop_get.value_ptr.* = cmp_transition;
                        }
                        if (!eq_structs.cmp_transitions(pivot_eq.state, cmp_transition)) {
                            for (group_ptr + 1..group_i) |ci| {
                                const other_eq = eq_classes[eq_structs.first_i_arr.items[ci]];
                                std.debug.assert(other_eq.group == ci);
                                if (other_eq.accept != pivot_eq.accept) continue;
                                if (eq_structs.cmp_transitions(other_eq.state, cmp_transition)) {
                                    eq_classes[offset + i].group = other_eq.group;
                                    try eq_structs.state_to_group.put(self.allocator, cmp_eq.state, other_eq.group);
                                    break;
                                }
                            } else { //Append new equivalence class group if it doesn't exist yet.
                                eq_classes[offset + i].group = group_i;
                                try eq_structs.state_to_group.put(self.allocator, cmp_eq.state, group_i);
                                group_i += 1;
                                try eq_structs.first_i_arr.append(self.allocator, @intCast(offset + i));
                            }
                            has_changed = true;
                        }
                    }
                }
            }
        }
        std.sort.block(EquivalenceClass, eq_classes, {}, EquivalenceClass.highest_state_sort);
        for (eq_classes) |eqc| std.debug.print("{any}\n", .{eqc});
        var ring_buf_q_hm: std.AutoHashMapUnmanaged(u32, RingBuffer(u32)) = .{};
        defer {
            var vit = ring_buf_q_hm.valueIterator();
            while (vit.next()) |v|
                v.deinit(self.allocator);
            ring_buf_q_hm.deinit(self.allocator);
        }
        for (eq_classes) |eqc| { // Makes the highest states merge first.
            const gop = try ring_buf_q_hm.getOrPut(self.allocator, eqc.group);
            if (!gop.found_existing) gop.value_ptr.* = try RingBuffer(u32).init(self.allocator);
            try gop.value_ptr.tail_push(self.allocator, eqc.state);
        }
        var ring_buf_pq: RingBufferPQ = RingBufferPQ.init(self.allocator, {});
        defer ring_buf_pq.deinit();
        var it = ring_buf_q_hm.valueIterator();
        while (it.next()) |v|
            if (v.len() != 1) try ring_buf_pq.add(v.*);
        var merges: u32 = 0;
        while (ring_buf_pq.count() != 0) {
            var this_buf = ring_buf_pq.remove();
            if (this_buf.len() != 1) {
                const highest = this_buf.head_pop_non_empty();
                try self.merge_states(this_buf.buffer[this_buf.head], highest, merged_states[0 .. merged_states.len - merges]);
                merges += 1;
                try ring_buf_pq.add(this_buf);
            }
        }
        try self._finish_dfa_minimization(&ssm);
    }
    pub fn hopcroft_algorithm2(self: *RegexFSM) !void {
        var ssm = self.substate_machines.pop();
        std.debug.assert(ssm.accept == .DFA);
        errdefer ssm.deinit(self.allocator);
        const merged_states = try self.get_reachable_states(.DFA, &.{ RegexState.ErrorState, ssm.init });
        defer self.allocator.free(merged_states);
        const dt_partitions = try self.get_partitions(merged_states);
        defer self.allocator.free(dt_partitions);
        var eq_classes = try self.allocator.alloc(EquivalenceClass, merged_states.len);
        defer self.allocator.free(eq_classes);
        for (merged_states, 0..) |state, i| {
            var ec: EquivalenceClass = undefined;
            ec.state = state;
            ec.group = @as(u32, @intFromBool(self.states.items[state].accept));
            ec.accept = self.states.items[state].accept;
            eq_classes[i] = ec;
        }
        try self._finish_dfa_minimization(&ssm);
    }
    const RingBufferPQ = std.PriorityQueue(RingBuffer(u32), void, struct {
        fn f(_: void, a: RingBuffer(u32), b: RingBuffer(u32)) std.math.Order {
            return std.math.order(b.buffer[b.head], a.buffer[a.head]);
        }
    }.f);
    const EQStructs = struct {
        first_i_arr: std.ArrayListUnmanaged(usize),
        state_to_group: std.AutoHashMapUnmanaged(u32, u32),
        ///Only get states from first_i_arr.
        transitions_cache: std.AutoArrayHashMapUnmanaged(u32, []u32),
        fn init(rfsm: *const RegexFSM, eq_classes: []EquivalenceClass, dt_partitions: []const DataType, group_i: u32) !EQStructs {
            var self: EQStructs = undefined;
            self.first_i_arr = .{};
            errdefer self.first_i_arr.deinit(rfsm.allocator);
            try self.first_i_arr.append(rfsm.allocator, 0);
            try self.update_first_i_arr(rfsm.allocator, eq_classes, group_i);
            self.state_to_group = .{};
            errdefer self.state_to_group.deinit(rfsm.allocator);
            for (eq_classes) |eqc|
                try self.state_to_group.put(rfsm.allocator, eqc.state, eqc.group);
            self.transitions_cache = .{};
            for (self.first_i_arr.items) |i| {
                const state = eq_classes[i].state;
                const this_state = rfsm.states.items[state];
                const state_transition = try rfsm.allocator.alloc(u32, dt_partitions.len);
                errdefer rfsm.allocator.free(state_transition);
                for (dt_partitions, 0..) |dtp, dt_i| {
                    switch (dtp) {
                        .none => unreachable,
                        .unicode, .char => |wc| {
                            state_transition[dt_i] = this_state.to_state(wc);
                        },
                        .range => |r| {
                            const trlist = try this_state.to_states(rfsm.allocator, r);
                            defer rfsm.allocator.free(trlist);
                            std.debug.assert(trlist.len == 1);
                            state_transition[dt_i] = trlist[0].to;
                        },
                    }
                }
                try self.transitions_cache.put(rfsm.allocator, state, state_transition);
            }
            return self;
        }
        fn cmp_transitions(self: EQStructs, cmp_state: u32, other_transition: []const u32) bool {
            const cmp_transition = self.transitions_cache.get(cmp_state).?;
            for (cmp_transition, other_transition) |c, o|
                if (self.state_to_group.get(c).? != self.state_to_group.get(o).?) return false;
            return true;
        }
        fn update_first_i_arr(self: *EQStructs, allocator: std.mem.Allocator, eq_classes: []EquivalenceClass, group_i: u32) !void {
            self.first_i_arr.items.len = 1;
            std.debug.assert(self.first_i_arr.items[0] == 0);
            std.sort.block(EquivalenceClass, eq_classes, {}, EquivalenceClass.group_then_state_sort);
            next_num: for (1..group_i) |eq_c| { //Binary search the first index of each group > 0.
                var low_i: usize = 0;
                var high_i: usize = eq_classes.len - 1;
                var mid_i = high_i / 2;
                while (high_i >= low_i) : (mid_i = (high_i + low_i) / 2) {
                    if (eq_c == eq_classes[mid_i].group and eq_c == eq_classes[mid_i - 1].group + 1) {
                        try self.first_i_arr.append(allocator, mid_i);
                        continue :next_num;
                    }
                    if (eq_c > eq_classes[mid_i].group) {
                        low_i = mid_i + 1;
                    } else {
                        if (mid_i != 1) {
                            high_i = mid_i - 1;
                        } else unreachable; //Should be contiguous
                    }
                }
                unreachable;
            }
        }
        fn deinit(self: *EQStructs, allocator: std.mem.Allocator) void {
            self.first_i_arr.deinit(allocator);
            self.state_to_group.deinit(allocator);
            for (self.transitions_cache.values()) |v| {
                allocator.free(v);
            }
            self.transitions_cache.deinit(allocator);
        }
    };
    /// State and transitions are merged into state1_1.
    fn merge_states(self: *RegexFSM, state1_i: u32, state2_i: u32, merged_states: []u32) !void {
        std.debug.print(ESC("Merging state {[1]} into {[0]}\n", .{ 1, 33 }), .{ state1_i, state2_i });
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
    try rfsm.hopcroft_algorithm();
    std.debug.print(ESC("Sub state machines: {any}\n", .{1}), .{rfsm.substate_machines.items});
    for (rfsm.states.items) |state| std.debug.print("{}\n", .{state});
    //var rsctx: RegexStateContext = .{ .array = rfsm.states.items };
    //std.debug.print("States: {any}\nState now: {any}\n", .{
    //    rfsm.states.items,
    //    input_string(1, &rsctx, "B"),
    //});
}
