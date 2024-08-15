const std = @import("std");
const regex_fsm = @import("regex_fsm.zig");
const RegexFSM = regex_fsm.RegexFSM;
const CharacterSet = regex_fsm.CharacterSet;
const Range = @import("range.zig").Range;
const DataTypePartitionList = @import("DataTypePartitionList.zig");
const logger = @import("logger.zig");
const ESC = logger.ESC;
const os_log_debug = logger.os_log_debug;
const regex_engine = @import("regex_engine.zig");
pub const std_options: std.Options = .{
    .logFn = logger.std_options_impl.logFn,
};
test {
    _ = regex_fsm;
    _ = @import("sorted_list.zig");
    _ = Range;
}
pub fn main_loop(allocator: std.mem.Allocator) void {
    while (true) {
        (err_label: {
            os_log_debug("Type regular expression here to convert to minimized DFA (Ctrl+C to exit): ", .{}, .{});
            const regex_str = std.io.getStdIn().reader().readUntilDelimiterAlloc(allocator, '\n', 4096) catch |e| break :err_label e;
            defer allocator.free(regex_str);
            const regex_str_no_r = regex_str[0 .. regex_str.len - 1];
            var lexer = regex_engine.RegexLexer.init(allocator, regex_str_no_r) catch |e| break :err_label e;
            defer lexer.deinit();
            const parse_tree = regex_engine.create_parse_tree(allocator, lexer) catch |e| break :err_label e;
            defer _ = parse_tree.deinit(allocator);
            var rc: regex_engine.RegexEngine = regex_engine.RegexEngine.init(allocator, lexer.str) catch |e| break :err_label e;
            defer rc.deinit();
            parse_tree.construct(&rc, regex_engine.RegexEngine.construct) catch |e| break :err_label e;
        }) catch {
            os_log_debug("DFA Compilation failure...\n", .{}, .{});
            //return;
        };
    }
}
pub fn main() !void {
    var gpa: std.heap.GeneralPurposeAllocator(.{}) = .{};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    main_loop(allocator);
}
