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
export fn RegexToDFA(regex_ptr: [*c]const u8, regex_len: usize) void {
    const regex_slice: []const u8 = if (regex_ptr != 0) regex_ptr[0..regex_len] else &.{};
    compile(regex_slice) catch |e| wasm_print.WasmError(e);
}
pub var regex_e: ?regex_engine.RegexEngine = null;
pub var transition_graph: ?regex_fsm.RegexFSM.TransitionsGraph = null;
fn compile(regex_str: []const u8) !void {
    if (regex_e != null) {
        regex_e.?.deinit();
        regex_e = null;
    }
    if (transition_graph != null) {
        transition_graph.?.deinit(allocator);
        transition_graph = null;
        TransitionGraphString = 0;
    }
    var lexer = try regex_engine.RegexLexer.init(allocator, regex_str);
    defer lexer.deinit();
    const parse_tree = try regex_engine.create_parse_tree(allocator, lexer);
    defer _ = parse_tree.deinit(allocator);
    regex_e = try regex_engine.RegexEngine.init(allocator, lexer.str);
    try parse_tree.construct(&regex_e.?, regex_engine.RegexEngine.construct);
    wasm_print.FlushPrint();
}
export fn TransitionGraphU8(test_str_ptr: [*c]const u8, test_str_len: usize) void {
    const test_str_slice: []const u8 = if (test_str_ptr != 0) test_str_ptr[0..test_str_len] else &.{};
    if (regex_e) |re| {
        transition_graph = re.fsm.get_string_state_transitions_u8(test_str_slice) catch |e| wasm_print.WasmError(e);
        for (transition_graph.?.list.items) |tr| {
            std.log.debug("{any}\n", .{tr});
        }
        std.log.debug("Final state: {} ({s})\n", .{ transition_graph.?.final_state, if (transition_graph.?.accept) "accepted" else "not accepted" });
        create_transition_graph_u8_js(transition_graph.?) catch |e| wasm_print.WasmError(e);
    }
    wasm_print.FlushPrint();
}
/// Bytes are formatted as {final_state, accept, transitions_byte_count, (Transitions) ...}
/// (Transitions) are formatted as { tr.to, accept }
fn create_transition_graph_u8_js(tg: regex_fsm.RegexFSM.TransitionsGraph) !void {
    if (TransitionGraphString != 0) { //Free old
        wasm_jsalloc.WasmFree(@ptrCast(TransitionGraphString));
        TransitionGraphString = 0;
    }
    var tg_arr: std.ArrayListUnmanaged(u32) = .{};
    defer tg_arr.deinit(allocator);
    try tg_arr.appendSlice(allocator, &.{ tg.final_state, @intFromBool(tg.accept), @intCast(tg.list.items.len * 2) });
    for (tg.list.items) |tr|
        try tg_arr.appendSlice(allocator, &.{ tr.to, @intFromBool(regex_e.?.fsm.states.items[tr.to].accept) });
    std.log.debug("{any}\n", .{tg_arr.items});
    const tg_arr_slice = std.mem.sliceAsBytes(try tg_arr.toOwnedSlice(allocator));
    errdefer allocator.free(tg_arr_slice);
    try wasm_jsalloc.slice_to_js(tg_arr_slice);
    TransitionGraphString = @ptrCast(tg_arr_slice.ptr);
    TransitionGraphStringLen = tg_arr_slice.len / 4;
}
export var TransitionGraphString: [*c]u32 = 0;
export var TransitionGraphStringLen: usize = 0;
