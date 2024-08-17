//!Overrides logFn and panic to log output to JavaScript.
const std = @import("std");
extern fn JSPrint([*c]const u8, usize, PrintType) void;
pub fn panic(mesg: []const u8, _: ?*std.builtin.StackTrace, _: ?usize) noreturn {
    @setCold(true);
    FlushPrint();
    var ebi: usize = 0;
    var error_buffer: [256]u8 = undefined;
    for ("Wasm has panicked with the following error message: '") |ch| {
        error_buffer[ebi] = ch;
        ebi += 1;
    }
    for (0..mesg.len) |i| {
        error_buffer[ebi] = mesg[i];
        ebi += 1;
    }
    for ("'\n") |ch| {
        error_buffer[ebi] = ch;
        ebi += 1;
    }
    JSPrint(&error_buffer[0], ebi, .err);
    @trap();
}
var wasm_printer = WasmPrinter{};
pub const std_options = struct {
    pub const log_level = .debug;
    pub fn logFn(
        comptime l: std.log.Level,
        comptime _: @Type(.EnumLiteral),
        comptime format: []const u8,
        args: anytype,
    ) void {
        const pt: PrintType = switch (l) {
            .debug, .info => .log,
            .warn => .warn,
            .err => .err,
        };
        if (wasm_printer.init_type == null) wasm_printer.init_type = pt;
        while (true) {
            if (std.fmt.format(wasm_printer.writer(), format, args)) {
                break;
            } else |err| {
                switch (err) {
                    error.NeedsFlush => wasm_printer.flush_finished(),
                    error.Truncate => { //Also console.error that PrintBufferMax is too small to print all characters.
                        JSPrint(&wasm_printer.buf[0], PrintBufferMax(), pt);
                        JSPrint(@ptrCast(&BufferTooFullMsg), BufferTooFullMsg.len, .err);
                        wasm_printer.reset();
                        return;
                    },
                }
            }
        }
    }
};
/// Print/clear buffer.
pub export fn FlushPrint() void {
    if (wasm_printer.pos != 0) JSPrint(&wasm_printer.buf[0], wasm_printer.pos, wasm_printer.init_type orelse .log);
    wasm_printer.reset();
}
const PrintType = enum(i32) { log, warn, err };
const WasmPrinter = struct {
    const WriteError = error{ NeedsFlush, Truncate };
    const Writer = std.io.Writer(*WasmPrinter, WasmPrinter.WriteError, WasmPrinter.write);
    pos: usize = 0,
    buf: PrintBufferT = undefined,
    init_type: ?PrintType = null,
    /// Copied from std std.io.BufferedWriter, but errors to flush to js console.
    fn write(self: *@This(), bytes: []const u8) WriteError!usize {
        if (bytes.len > self.buf.len) {
            @memcpy(&self.buf, bytes[0..self.buf.len]);
            return WriteError.Truncate;
        }
        if (self.pos + bytes.len > self.buf.len) {
            return WriteError.NeedsFlush;
        }
        @memcpy(self.buf[self.pos..(self.pos + bytes.len)], bytes);
        self.pos += bytes.len;
        return bytes.len;
    }
    fn flush_finished(self: *@This()) void {
        const old_pos = self.pos;
        while (self.buf[self.pos - 1] != '\n') : (self.pos -= 1) { //Exclude unfinished \n terminated strings if possible.
            if (self.pos == 1) {
                self.pos = old_pos;
                break;
            }
        }
        FlushPrint();
    }
    fn reset(self: *@This()) void {
        wasm_printer.init_type = null;
        self.pos = 0;
    }
    fn writer(self: *@This()) Writer {
        return .{ .context = self };
    }
};
pub fn WasmError(err: anyerror) noreturn {
    FlushPrint();
    std.log.err("Wasm found error: '{s}'", .{@errorName(err)});
    FlushPrint();
    @trap();
}
export fn PrintBufferMax() usize {
    return 8192;
}
const PrintBufferT = [PrintBufferMax()]u8;
const BufferTooFullMsg = "Buffer is too small. Cannot print all of the characters.".*;
