const std = @import("std");
const logger = @import("logger.zig");
const regex_fsm = @import("regex_fsm.zig");
const regex_engine = @import("regex_engine.zig");
const wasm_print = @import("wasm_print.zig");
extern fn JSPrint([*c]const u8, usize, wasm_print.PrintType) void;
pub const allocator = std.heap.wasm_allocator;
pub const std_options: std.Options = .{
    .logFn = logger.std_options_impl.logFn,
};
pub const panic = wasm_print.panic;
//temporary to see if modules work in .wasm
export fn something() void {
    something_with_errors() catch |e| wasm_print.WasmError(e);
}
fn something_with_errors() !void {
    var lexer = try regex_engine.RegexLexer.init(allocator, "((25[0-5]|(2[0-4]|1\\d|[1-9]|)\\d)\\.?){4}{5,6}");
    defer lexer.deinit();
    const parse_tree = try regex_engine.create_parse_tree(allocator, lexer);
    defer _ = parse_tree.deinit(allocator);
    var rc = try regex_engine.RegexEngine.init(allocator, lexer.str);
    defer rc.deinit();
    try parse_tree.construct(&rc, regex_engine.RegexEngine.construct);
    wasm_print.FlushPrint();
}
