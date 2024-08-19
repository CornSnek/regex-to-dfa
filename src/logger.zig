const std = @import("std");
pub const std_options_impl = if (@import("builtin").os.tag != .freestanding) NonWasmLog else WasmLog;
const NonWasmLog = struct {
    pub fn logFn(
        comptime l: std.log.Level,
        comptime _: @Type(.EnumLiteral),
        comptime format: []const u8,
        args: anytype,
    ) void {
        const stderr = std.io.getStdErr().writer();
        var bw = std.io.bufferedWriter(stderr);
        const writer = bw.writer();
        std.debug.lockStdErr();
        defer std.debug.unlockStdErr();
        nosuspend {
            if (l != .err) {
                writer.print(format, args) catch return;
            } else {
                writer.print(ESC(format, .{ 1, 31 }), args) catch return;
            }
            bw.flush() catch return;
        }
    }
};
const WasmLog = @import("wasm_print.zig").std_options;
///If used in WASM .freestanding, excludes the ansi_codes. debug.print for tests.
pub fn os_log_debug(comptime format: []const u8, args: anytype, ansi_codes: anytype) void {
    if (@import("builtin").is_test) {
        std.debug.print(ESC(format, ansi_codes), args);
    } else if (@import("builtin").os.tag != .freestanding) {
        std.log.debug(ESC(format, ansi_codes), args);
    } else {
        std.log.debug(format, args);
    }
}
pub fn ESC(comptime str: []const u8, comptime codes: anytype) []const u8 {
    if (codes.len == 0) return str;
    var return_this: []const u8 = "\x1b[";
    for (codes, 0..) |code, i|
        return_this = return_this ++ std.fmt.comptimePrint("{}{c}", .{ code, if (i != codes.len - 1) ';' else 'm' });
    return_this = return_this ++ str ++ "\x1b[0m";
    return return_this;
}
