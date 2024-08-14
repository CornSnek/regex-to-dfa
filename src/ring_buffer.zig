const std = @import("std");
/// Growable array queue/stack
pub fn RingBuffer(T: type) type {
    return struct {
        buffer: []T,
        head: usize,
        tail: usize,
        capacity: usize,
        pub fn init(allocator: std.mem.Allocator) !RingBuffer(T) {
            return .{
                .buffer = try allocator.alloc(T, 1),
                .head = 0,
                .tail = 0,
                .capacity = 1,
            };
        }
        pub fn deinit(self: RingBuffer(T), allocator: std.mem.Allocator) void {
            allocator.free(self.buffer);
        }
        pub fn full(self: RingBuffer(T)) bool {
            return (self.tail + 1) % self.capacity == self.head;
        }
        pub fn len(self: RingBuffer(T)) usize {
            return (self.tail + self.capacity - self.head) % self.capacity;
        }
        pub fn tail_push(self: *RingBuffer(T), allocator: std.mem.Allocator, value: T) !void {
            if (self.full()) _ = try self.resize(allocator, (self.capacity * 3) / 2 + 1);
            self.buffer[self.tail] = value;
            self.tail = (self.tail + 1) % self.capacity;
        }
        pub fn head_push(self: *RingBuffer(T), allocator: std.mem.Allocator, value: T) !void {
            if (self.full()) _ = try self.resize(allocator, (self.capacity * 3) / 2 + 1);
            self.head = (self.head + self.capacity - 1) % self.capacity;
            self.buffer[self.head] = value;
        }
        pub fn head_pop_non_empty(self: *RingBuffer(T)) T {
            std.debug.assert(self.head != self.tail);
            return self._head_pop();
        }
        pub fn head_pop(self: *RingBuffer(T)) ?T {
            if (self.head == self.tail) return null;
            return self._head_pop();
        }
        fn _head_pop(self: *RingBuffer(T)) T {
            const value = self.buffer[self.head];
            self.buffer[self.head] = undefined;
            self.head = (self.head + 1) % self.capacity;
            return value;
        }
        /// Shrinking is allowed only if there are less items than `new_capacity + 1`
        pub fn resize(self: *RingBuffer(T), allocator: std.mem.Allocator, new_capacity: usize) !bool {
            if (new_capacity < self.capacity)
                if (self.len() > new_capacity + 1)
                    return false;
            const new_buffer = try allocator.alloc(T, new_capacity);
            errdefer allocator.free(new_buffer);
            var j = self.head;
            for (0..@min(self.capacity, new_capacity)) |i| { //Copy from old buffer so that only the new buffer data is at [0..self.capacity]
                new_buffer[i] = self.buffer[j];
                j = (j + 1) % self.capacity;
            }
            @memset(self.buffer, undefined);
            allocator.free(self.buffer);
            self.buffer = new_buffer;
            self.head = 0;
            self.tail = @min(self.capacity, new_capacity) - 1;
            self.capacity = new_capacity;
            return true;
        }
    };
}
