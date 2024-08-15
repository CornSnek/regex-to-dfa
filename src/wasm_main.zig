const std = @import("std");
const logger = @import("logger.zig");
const regex_fsm = @import("regex_fsm.zig");
const regex_engine = @import("regex_engine.zig");
const wasm_print = @import("wasm_print.zig");
const wasm_jsalloc = @import("wasm_jsalloc.zig");
pub const allocator = std.heap.wasm_allocator;
pub const std_options: std.Options = .{
    .logFn = logger.std_options_impl.logFn,
};
pub const panic = wasm_print.panic;
export fn RegexToDFA(regex_ptr: [*c]const u8, regex_len: u32) void {
    compile(regex_ptr[0..regex_len]) catch |e| wasm_print.WasmError(e);
}
fn compile(regex_str: []const u8) !void {
    var lexer = try regex_engine.RegexLexer.init(allocator, regex_str);
    defer lexer.deinit();
    const parse_tree = try regex_engine.create_parse_tree(allocator, lexer);
    defer _ = parse_tree.deinit(allocator);
    var rc = try regex_engine.RegexEngine.init(allocator, lexer.str);
    defer rc.deinit();
    try parse_tree.construct(&rc, regex_engine.RegexEngine.construct);
    wasm_print.FlushPrint();
}
