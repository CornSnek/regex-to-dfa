const std = @import("std");
const regex_fsm = @import("regex_fsm.zig");
const RegexFSM = regex_fsm.RegexFSM;
const CharacterSet = regex_fsm.CharacterSet;
test {
    _ = regex_fsm;
    _ = @import("sorted_list.zig");
}
const TokenTag = union(enum) {
    char: u8,
    unicode: u16,
    quant_exact: u32,
    quant_lte: u32,
    quant_gte: u32,
    char_set: CharacterSet,
    @"|": void,
    @"(": void,
    @")": void,
    @"[": void,
    @"set^": void,
    @"-": void,
    @"]": void,
    @"?": void,
    @"*": void,
    @"+": void,
    eof: void,
    fn eq_tag(self: TokenTag, num: GetMinTagType(TokenTag)) bool {
        return @intFromEnum(self) == num;
    }
};
const Token = struct {
    begin: u32 = 0,
    end: u32 = 0,
    tt: TokenTag,
};
/// Least amount of bits used to extract an enum out of a tagged union.
/// Using @intFromEnum to get the enum value out of the union.
/// The enum values of TokenT should be the set from [0 to (Tagged Union).len-1]
fn GetMinTagType(comptime TokenT: anytype) type {
    const len_float: comptime_float = @floatFromInt(switch (@typeInfo(TokenT)) {
        .Union => |u| u.fields.len,
        else => @compileError(@typeName(TokenT) ++ " must be a .Union type.\n"),
    });
    if (len_float == 0) return u0;
    if (len_float == 1) return u1;
    const log_res = @log2(len_float);
    const evenly = log_res == @floor(log_res);
    const bits: comptime_int = @as(comptime_int, @intFromFloat(log_res)) + @as(comptime_int, @intFromBool(!evenly));
    return std.meta.Int(.unsigned, bits);
}
const ReadState = union(enum) {
    begin: void,
    escaped: bool, //If '-' can be escaped.
    set: u32,
    quant_lhs: []const u8, //Number range.
    quant_rhs_with_left: []const u8,
    quant_rhs_no_left: []const u8,
};
pub const RegexLexer = struct {
    pub const LexerError = error{
        NotEnoughCharactersX,
        NotEnoughCharactersU,
        InvalidCharacterToEscape,
        EscapedAtEnd,
        MissingSquareBracketEnd,
        StatePopToNull,
        LHSQuantifierNumberRequired,
        RHSQuantifierNumberRequired,
        MissingEndQuantifier,
        MissingEndCurlyBracketQuantifier,
        NotANumberQuantifier,
    };
    pub const RegexTokenArray = std.ArrayList(Token);
    token_array: RegexTokenArray,
    str: []const u8,
    pub fn init(allocator: std.mem.Allocator, regex_str: []const u8) !RegexLexer {
        var read_state: ReadState = .begin;
        var token_array: RegexTokenArray = RegexTokenArray.init(allocator);
        errdefer token_array.deinit();
        var i: u32 = 0;
        var esc_last_read_state: ReadState = undefined; //.escaped has two states to go into.
        while (i < regex_str.len) : (i += 1) {
            const c = regex_str[i];
            switch (read_state) {
                .begin => {
                    switch (c) {
                        '(' => try token_array.append(.{ .tt = .@"(", .begin = i, .end = i }),
                        ')' => try token_array.append(.{ .tt = .@")", .begin = i, .end = i }),
                        '|' => try token_array.append(.{ .tt = .@"|", .begin = i, .end = i }),
                        '?' => try token_array.append(.{ .tt = .@"?", .begin = i, .end = i }),
                        '*' => try token_array.append(.{ .tt = .@"*", .begin = i, .end = i }),
                        '+' => try token_array.append(.{ .tt = .@"+", .begin = i, .end = i }),
                        '{' => read_state = .{ .quant_lhs = regex_str[i + 1 .. i + 1] }, //Set as 0 length pointing to the next character.
                        '\\' => {
                            esc_last_read_state = read_state;
                            read_state = .{ .escaped = false };
                        },
                        '[' => {
                            try token_array.append(.{ .tt = .@"[", .begin = i, .end = i });
                            read_state = .{ .set = @intCast(i + 1) };
                        },
                        '.' => try token_array.append(.{ .tt = .{ .char_set = .@"." }, .begin = i, .end = i }),
                        else => try token_array.append(.{ .tt = .{ .char = c }, .begin = i, .end = i }),
                    }
                },
                .set => |set_begin_i| {
                    switch (c) {
                        '\\' => {
                            esc_last_read_state = read_state;
                            read_state = .{ .escaped = true };
                        },
                        '^' => if (set_begin_i != i) try token_array.append(.{ .tt = .{ .char = '^' }, .begin = i, .end = i }) else try token_array.append(.{ .tt = .@"set^", .begin = i, .end = i }),
                        '-' => try token_array.append(.{ .tt = .@"-", .begin = i, .end = i }),
                        ']' => {
                            if (i == set_begin_i) {
                                highlight_error(regex_str, set_begin_i - 1, i);
                                std.debug.print(ESC("Empty set is disallowed (all states would point to the 0 error state)\n", .{ 1, 31 }), .{});
                                return error.LexerError;
                            }
                            try token_array.append(.{ .tt = .@"]", .begin = i, .end = i });
                            read_state = .begin;
                        },
                        else => try token_array.append(.{ .tt = .{ .char = c }, .begin = i, .end = i }),
                    }
                },
                .escaped => |has_minus| {
                    switch (c) {
                        '0' => try token_array.append(.{ .tt = .{ .char = 0 }, .begin = i - 1, .end = i }),
                        'v' => try token_array.append(.{ .tt = .{ .char = 11 }, .begin = i - 1, .end = i }),
                        'f' => try token_array.append(.{ .tt = .{ .char = 12 }, .begin = i - 1, .end = i }),
                        'r' => try token_array.append(.{ .tt = .{ .char = '\r' }, .begin = i - 1, .end = i }),
                        't' => try token_array.append(.{ .tt = .{ .char = '\t' }, .begin = i - 1, .end = i }),
                        'n' => try token_array.append(.{ .tt = .{ .char = '\n' }, .begin = i - 1, .end = i }),
                        'x' => {
                            if (i >= regex_str.len - 2) {
                                std.debug.print(ESC("\\x requires 2 hexadecimal characters to parse\n", .{ 1, 31 }), .{});
                                return error.LexerError;
                            }
                            const num = try std.fmt.parseInt(u8, regex_str[i + 1 .. i + 3], 16);
                            try token_array.append(.{ .tt = .{ .char = num }, .begin = i - 1, .end = i + 2 });
                            i += 2;
                        },
                        'u' => {
                            if (i >= regex_str.len - 4) {
                                std.debug.print(ESC("\\u requires 4 hexadecimal characters to parse\n", .{ 1, 31 }), .{});
                                return error.LexerError;
                            }
                            const num = try std.fmt.parseInt(u16, regex_str[i + 1 .. i + 5], 16);
                            try token_array.append(.{ .tt = .{ .unicode = num }, .begin = i - 1, .end = i + 4 });
                            i += 4;
                        },
                        's' => try token_array.append(.{ .tt = .{ .char_set = .whitespace }, .begin = i - 1, .end = i }),
                        'S' => try token_array.append(.{ .tt = .{ .char_set = .nonwhitespace }, .begin = i - 1, .end = i }),
                        'd' => try token_array.append(.{ .tt = .{ .char_set = .digit }, .begin = i - 1, .end = i }),
                        'D' => try token_array.append(.{ .tt = .{ .char_set = .nondigit }, .begin = i - 1, .end = i }),
                        'w' => try token_array.append(.{ .tt = .{ .char_set = .word }, .begin = i - 1, .end = i }),
                        'W' => try token_array.append(.{ .tt = .{ .char_set = .nonword }, .begin = i - 1, .end = i }),
                        '+', '*', '?', '^', '$', '\\', '.', '[', ']', '{', '}', '(', ')', '|', '/' => |ch| try token_array.append(.{ .tt = .{ .char = ch }, .begin = i - 1, .end = i }),
                        '-' => if (has_minus) {
                            try token_array.append(.{ .tt = .{ .char = '-' }, .begin = i - 1, .end = i });
                        } else {
                            std.debug.print(ESC("'-' is not a valid character to escape\n", .{ 1, 31 }), .{});
                            return error.LexerError;
                        },
                        else => |ch| {
                            std.debug.print(ESC("'{c}' is not a valid character to escape\n", .{ 1, 31 }), .{ch});
                            return error.LexerError;
                        },
                    }
                    read_state = esc_last_read_state;
                },
                .quant_lhs => |*num_str| {
                    switch (c) {
                        '0'...'9' => num_str.len += 1,
                        '}' => {
                            if (num_str.len == 0) {
                                std.debug.print(ESC("Numbers are required inside '{{' and '}}'\n", .{ 1, 31 }), .{});
                                return error.LexerError;
                            }
                            const num = try std.fmt.parseInt(u32, num_str.*, 10);
                            try token_array.append(.{ .tt = .{ .quant_exact = num }, .begin = @intCast(i - num_str.len - 1), .end = i });
                            read_state = .begin;
                        },
                        ',' => {
                            if (num_str.len != 0) {
                                const num = try std.fmt.parseInt(u32, num_str.*, 10);
                                try token_array.append(.{ .tt = .{ .quant_gte = num }, .begin = @intCast(i - num_str.len - 1), .end = i });
                                read_state = .{ .quant_rhs_with_left = regex_str[i + 1 .. i + 1] };
                            } else {
                                read_state = .{ .quant_rhs_no_left = regex_str[i + 1 .. i + 1] };
                            }
                        },
                        else => {
                            std.debug.print(ESC("Numbers are required inside '{{' and '}}'\n", .{ 1, 31 }), .{});
                            return error.LexerError;
                        },
                    }
                },
                .quant_rhs_with_left => |*num_str| {
                    switch (c) {
                        '0'...'9' => num_str.len += 1,
                        '}' => { //For {number1,number2} or {number1,}
                            if (num_str.len != 0) {
                                const num = try std.fmt.parseInt(u32, num_str.*, 10);
                                try token_array.append(.{ .tt = .{ .quant_lte = num }, .begin = @intCast(i - num_str.len - 1), .end = i });
                            }
                            read_state = .begin;
                        },
                        else => {
                            std.debug.print(ESC("Numbers are required inside '{{' and '}}'\n", .{ 1, 31 }), .{});
                            return error.LexerError;
                        },
                    }
                },
                .quant_rhs_no_left => |*num_str| {
                    switch (c) {
                        '0'...'9' => num_str.len += 1,
                        '}' => { //For {,number}
                            if (num_str.len != 0) {
                                const num = try std.fmt.parseInt(u32, num_str.*, 10);
                                try token_array.append(.{ .tt = .{ .quant_lte = num }, .begin = @intCast(i - num_str.len - 1), .end = i });
                            } else {
                                std.debug.print(ESC("Numbers are required inside '{{' and '}}'\n", .{ 1, 31 }), .{});
                                return error.LexerError;
                            }
                            read_state = .begin;
                        },
                        else => {
                            std.debug.print(ESC("Numbers are required inside '{{' and '}}'\n", .{ 1, 31 }), .{});
                            return error.LexerError;
                        },
                    }
                },
            }
        }
        try token_array.append(.{ .tt = .eof, .begin = i, .end = i });
        switch (read_state) {
            .begin => return .{ .token_array = token_array, .str = regex_str },
            else => {
                std.debug.print(ESC("Unexpected end of string\n", .{ 1, 31 }), .{});
                return error.LexerError;
            },
        }
    }
    pub fn deinit(self: RegexLexer) void {
        self.token_array.deinit();
    }
};
const NonTermEnum = enum {
    Regex, //start symbol
    MExpr,
    Expr,
    Quantifier,
    SubExpr,
    Group,
    GMExpr,
    GMExpr2,
    GAMExpr,
    Set,
    Set2,
    SetExpr,
    _Error1,
    _Error2,
    _Error3,
};

fn BNFStruct(_TokenT: type, _ConstructorT: type) type {
    return struct {
        const RuleCall = union(enum) {
            none: void,
            accept: *const fn (*_ConstructorT) anyerror!void,
            reject: *const fn (std.mem.Allocator, []const _TokenT) anyerror![]const u8,
        };
        bnf: []const u8,
        func: RuleCall,
    };
}
fn BNFParser(
    comptime _TermT: type,
    comptime _TokenT: type,
    comptime term_field: []const u8,
    comptime _NonTermT: type,
    comptime _ConstructorT: type,
    comptime rules: []const BNFStruct(_TokenT, _ConstructorT),
    comptime quota: u32,
) type {
    return struct {
        pub const TermT = _TermT;
        pub const TokenT = _TokenT;
        pub const NonTermT = _NonTermT;
        pub const ConstructorT = _ConstructorT;
        pub const Symbol = union(enum) {
            t: GetMinTagType(TermT),
            nt: NonTermT,
            pub fn format(self: @This(), comptime _: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
                if (self == .t) {
                    try writer.writeByte('\'');
                    try std.fmt.formatBuf(@tagName(@as(@typeInfo(TermT).Union.tag_type.?, @enumFromInt(self.t))), options, writer);
                    try writer.writeByte('\'');
                } else {
                    try std.fmt.formatBuf(@tagName(self.nt), options, writer);
                }
            }
            pub fn eq(self: @This(), other: @This()) bool {
                return if (self == .t and other == .t)
                    return self.t == other.t
                else if (self == .nt and other == .nt)
                    return self.nt == other.nt
                else
                    false;
            }
        };
        pub const NonTermTNode = struct {
            nodes: []*SymbolNode,
            offset: u32,
            count: u32 = 0,
            nt: NonTermT,
            pub fn push(self: *NonTermTNode, symbol: *SymbolNode) void {
                std.debug.assert(self.count < @as(u32, @intCast(self.nodes.len)));
                self.nodes[self.count] = symbol;
                self.count += 1;
            }
            pub fn pop(self: *NonTermTNode) *SymbolNode {
                std.debug.assert(self.count != 0);
                defer self.count -= 1;
                return self.nodes[self.count - 1];
            }
            /// If a rule is not correct. Salvage children nodes for the next rule.
            pub fn extract(self: *NonTermTNode, allocator: std.mem.Allocator) ![]*SymbolNode {
                defer self.count = 0;
                return allocator.dupe(*SymbolNode, self.nodes[0..self.count]);
            }
            /// extract() children first before changing rule
            pub fn change(self: *NonTermTNode, allocator: std.mem.Allocator, offset: u32) !void {
                std.debug.assert(self.count == 0);
                std.debug.assert(offset < RuleRanges[@intFromEnum(self.nt)].count);
                self.nodes = try allocator.realloc(self.nodes, Rules[RuleRanges[@intFromEnum(self.nt)].start + offset].sym.len);
                self.offset = offset;
            }
        };
        pub const SymbolNode = union(enum) {
            t: TokenT,
            nt: NonTermTNode,
            /// Created on allocator because its lifetime should be a descendant of a root SymbolNode (To be deallocated)
            pub fn init_term(allocator: std.mem.Allocator, token: TokenT) !*SymbolNode {
                const self = try allocator.create(SymbolNode);
                self.* = .{ .t = token };
                return self;
            }
            /// Created on allocator because its lifetime should be a descendant of a root SymbolNode (To be deallocated)
            pub fn init_nonterm(allocator: std.mem.Allocator, nt: NonTermT, offset: u32) !*SymbolNode {
                std.debug.assert(offset < RuleRanges[@intFromEnum(nt)].count);
                const self = try allocator.create(SymbolNode);
                errdefer allocator.destroy(self);
                self.* = try init_nonterm_root(allocator, nt, offset);
                return self;
            }
            /// Root SymbolNode that should contain all the heap-allocated subnodes
            pub fn init_nonterm_root(allocator: std.mem.Allocator, nt: NonTermT, offset: u32) !SymbolNode {
                std.debug.assert(offset < RuleRanges[@intFromEnum(nt)].count);
                return .{
                    .nt = .{
                        .nt = nt,
                        .offset = offset,
                        .nodes = try allocator.alloc(*SymbolNode, Rules[RuleRanges[@intFromEnum(nt)].start + offset].sym.len),
                    },
                };
            }
            pub fn eq_sym(self: SymbolNode, sym: Symbol) bool {
                if (self == .nt) {
                    return sym.eq(.{ .nt = self.nt.nt });
                } else {
                    return sym.eq(.{ .t = @intFromEnum(@field(self.t, term_field)) });
                }
            }
            pub fn print_tree(self: SymbolNode, comptime order: enum { pre, post }, depth: u32) void {
                if (order == .pre) self.print(depth);
                if (self == .nt) {
                    for (self.nt.nodes) |node| {
                        node.print_tree(order, depth + 1);
                    }
                }
                if (order == .post) self.print(depth);
            }
            pub fn print(self: SymbolNode, depth: u32) void {
                if (self == .nt) {
                    const rule_range = RuleRanges[@intFromEnum(self.nt.nt)];
                    std.debug.print(ESC("{[sp]:-<[w]}-{[r]}\n", .{1}), .{ .sp = depth, .w = depth, .r = Rules[rule_range.start + self.nt.offset] });
                } else {
                    std.debug.print(ESC("{[sp]:-<[w]}-{[p]any}\n", .{ 1, 30 }), .{ .sp = depth, .p = self.t, .w = depth });
                }
            }
            ///Construct nodes to ConstructFn in post-order traversal
            pub fn construct(
                self: SymbolNode,
                context: *ConstructorT,
                comptime ConstructFn: fn (ctx: *ConstructorT, node: SymbolNode) anyerror!void,
            ) !void {
                if (self == .nt)
                    for (self.nt.nodes) |node|
                        try node.construct(context, ConstructFn);
                try ConstructFn(context, self);
            }
            /// Returns the number of terminal symbols removed after deinizializing this self.nt.nodes, and all children (Including children's memory from the heap).
            pub fn deinit(self: SymbolNode, allocator: std.mem.Allocator) u32 {
                var count_deinit: u32 = 0;
                if (self == .nt) {
                    for (self.nt.nodes[0..self.nt.count]) |node| {
                        count_deinit += node.deinit(allocator);
                        allocator.destroy(node);
                    }
                    allocator.free(self.nt.nodes);
                } else count_deinit += 1;
                return count_deinit;
            }
        };
        pub const Rule = struct {
            nt: NonTermT,
            sym: []const Symbol,
            pub fn format(self: @This(), comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
                if (std.mem.eql(u8, fmt, "|")) {
                    try std.fmt.formatBuf(fmt, options, writer);
                    try writer.writeByte(' ');
                } else {
                    try std.fmt.formatBuf(@tagName(self.nt), options, writer);
                    try writer.writeAll(" ::= ");
                }
                for (self.sym) |s| {
                    try s.format(fmt, options, writer);
                    try writer.writeByte(' ');
                }
            }
            pub fn eq(self: @This(), other: @This()) bool {
                if (self.nt != other.nt) return false;
                if (self.sym.len != other.sym.len) return false;
                for (self.sym, other.sym) |s, o| if (!s.eq(o)) return false;
                return true;
            }
        };
        fn get_nt(comptime nt_str: []const u8) NonTermT {
            for (std.meta.fields(NonTermT)) |f| {
                if (std.mem.eql(u8, f.name, nt_str))
                    return @enumFromInt(f.value);
            } else @compileError("Unrecognized " ++ @typeName(NonTermT) ++ " symbol name: '" ++ nt_str ++ "'");
        }
        fn get_t_enum(comptime t_str: []const u8) GetMinTagType(TermT) {
            const TTTermT = @typeInfo(TermT).Union;
            for (TTTermT.fields, 0..) |f, i| {
                if (std.mem.eql(u8, f.name, t_str))
                    return i;
            } else @compileError("Unrecognized " ++ @typeName(TermT) ++ " symbol name: '" ++ t_str ++ "'");
        }
        pub const RuleCallFn = f: {
            var _fns: []const BNFStruct(_TokenT, _ConstructorT).RuleCall = &.{};
            for (rules) |rule|
                _fns = _fns ++ &[1]BNFStruct(_TokenT, _ConstructorT).RuleCall{rule.func};
            break :f _fns;
        };
        pub const Rules = r: {
            @setEvalBranchQuota(quota);
            var _rules: []const Rule = &.{};
            for (rules, 0..) |rule, i| {
                var sym_str_it = std.mem.tokenizeScalar(u8, rule.bnf, ' ');
                const nt_str = sym_str_it.next() orelse @compileError("BNF string must start with a " ++ @typeName(NonTermT) ++ " symbol.");
                var rule_struct: Rule = undefined;
                if (std.mem.eql(u8, nt_str, "|")) {
                    rule_struct = .{ .nt = _rules[i - 1].nt, .sym = &.{} };
                } else {
                    const coloncolonequal = sym_str_it.next() orelse @compileError("2nd word in BNF string must contain '::='");
                    if (!std.mem.eql(u8, coloncolonequal, "::=")) @compileError("2nd word in BNF string must contain '::='");
                    rule_struct = .{ .nt = get_nt(nt_str), .sym = &.{} };
                }
                while (sym_str_it.next()) |sym_str| {
                    switch (sym_str[0]) {
                        '\'', '"' => |ch| {
                            if (sym_str[sym_str.len - 1] != ch) @compileError("Character (" ++ &[_]u8{ch} ++ ") is missing at the end for the " ++ @typeName(TermT) ++ " enum");
                            const new_sym_str = sym_str[1 .. sym_str.len - 1];
                            rule_struct.sym = rule_struct.sym ++ &[1]Symbol{.{ .t = get_t_enum(new_sym_str) }};
                        },
                        else => rule_struct.sym = rule_struct.sym ++ &[1]Symbol{.{ .nt = get_nt(sym_str) }},
                    }
                }
                _rules = _rules ++ &[1]Rule{rule_struct};
            }
            break :r _rules;
        };
        pub const RuleRange = struct { start: u32, count: u32 };
        pub const RuleRanges = rr: {
            var _rule_ranges: []const RuleRange = &.{};
            var check_v = -1;
            var rule_i = 0;
            for (std.meta.fields(NonTermT)) |f| {
                if (check_v != f.value - 1)
                    @compileError("The values in " ++ @typeName(NonTermT) ++ " must be from 0 to (Length-1)");
                var count = 0;
                const old_rule_i = rule_i;
                while (@intFromEnum(Rules[rule_i].nt) == f.value) {
                    count += 1;
                    rule_i += 1;
                    if (rule_i == Rules.len) break;
                }
                _rule_ranges = _rule_ranges ++ &[1]RuleRange{.{ .start = old_rule_i, .count = count }};
                check_v += 1;
                if (rule_i == Rules.len) break;
                if (@intFromEnum(Rules[rule_i].nt) != check_v + 1) @compileError("The rules must be ordered by " ++ @typeName(NonTermT) ++ " enum order");
            }
            break :rr _rule_ranges;
        };
    };
}
const RegexBNF = BNFParser(TokenTag, Token, "tt", NonTermEnum, RegexEngine, &[_]BNFStruct(Token, RegexEngine){
    .{ .bnf = "Regex ::= MExpr 'eof'                        ", .func = .{ .accept = RegexEngine.finalize } },
    .{ .bnf = "MExpr ::= Expr MExpr                         ", .func = .{ .accept = RegexEngine.concatenation } },
    .{ .bnf = "        |                                    ", .func = .{ .accept = RegexEngine.empty_ssm } },
    .{ .bnf = "Expr ::= SubExpr Quantifier '|' Expr         ", .func = .{ .accept = RegexEngine.alternation } },
    .{ .bnf = "       | SubExpr Quantifier                  ", .func = .none },
    .{ .bnf = "       | _Error1                             ", .func = .none },
    .{ .bnf = "       | _Error2                             ", .func = .none },
    .{ .bnf = "Quantifier ::= 'quant_gte' 'quant_lte'       ", .func = .{ .accept = RegexEngine.quant_between } },
    .{ .bnf = "             | 'quant_gte'                   ", .func = .{ .accept = RegexEngine.quant_gte } },
    .{ .bnf = "             | 'quant_lte'                   ", .func = .{ .accept = RegexEngine.quant_lte } },
    .{ .bnf = "             | '?'                           ", .func = .{ .accept = RegexEngine.optional } },
    .{ .bnf = "             | '*'                           ", .func = .{ .accept = RegexEngine.kleene } },
    .{ .bnf = "             | '+'                           ", .func = .{ .accept = RegexEngine.plus } },
    .{ .bnf = "             | 'quant_exact'                 ", .func = .{ .accept = RegexEngine.quant_exact } },
    .{ .bnf = "             |                               ", .func = .none },
    .{ .bnf = "SubExpr ::= 'char'                           ", .func = .{ .accept = RegexEngine.char } },
    .{ .bnf = "          | 'unicode'                        ", .func = .{ .accept = RegexEngine.unicode } },
    .{ .bnf = "          | 'char_set'                       ", .func = .{ .accept = RegexEngine.char_set } },
    .{ .bnf = "          | Group                            ", .func = .none },
    .{ .bnf = "          | Set                              ", .func = .none },
    .{ .bnf = "Group ::= '(' GMExpr ')'                     ", .func = .{ .accept = RegexEngine.clear_p } },
    .{ .bnf = "        | '(' ')'                            ", .func = .{ .accept = RegexEngine.clear_p_empty_ssm } },
    .{ .bnf = "GMExpr ::= GMExpr2 GAMExpr                   ", .func = .none },
    .{ .bnf = "GMExpr2 ::= SubExpr Quantifier GMExpr2       ", .func = .{ .accept = RegexEngine.concatenation } },
    .{ .bnf = "         | _Error1                           ", .func = .none },
    .{ .bnf = "         |                                   ", .func = .{ .accept = RegexEngine.empty_ssm } },
    .{ .bnf = "GAMExpr ::= '|' GMExpr GAMExpr               ", .func = .{ .accept = RegexEngine.alternation } },
    .{ .bnf = "          |                                  ", .func = .none },
    .{ .bnf = "Set ::= '[' 'set^' Set2                      ", .func = .{ .accept = RegexEngine.set_complement } },
    .{ .bnf = "      | '[' Set2                             ", .func = .{ .accept = RegexEngine.set } },
    .{ .bnf = "Set2 ::= SetExpr Set2                        ", .func = .none },
    .{ .bnf = "       | ']'                                 ", .func = .{ .accept = RegexEngine.set_end } },
    .{ .bnf = "SetExpr ::= 'char' '-' 'char'                ", .func = .{ .accept = RegexEngine.set_range_char } },
    .{ .bnf = "          | 'char'                           ", .func = .{ .accept = RegexEngine.set_char } },
    .{ .bnf = "          | 'unicode' '-' 'unicode'          ", .func = .{ .accept = RegexEngine.set_range_unicode } },
    .{ .bnf = "          | 'unicode'                        ", .func = .{ .accept = RegexEngine.set_unicode } },
    .{ .bnf = "          | 'char_set'                       ", .func = .{ .accept = RegexEngine.set_char_set } },
    .{ .bnf = "          | _Error3                          ", .func = .none },
    .{ .bnf = "_Error1 ::= 'quant_gte'                      ", .func = .{ .reject = stray_quantifier } },
    .{ .bnf = "          | 'quant_lte'                      ", .func = .{ .reject = stray_quantifier } },
    .{ .bnf = "          | '?'                              ", .func = .{ .reject = stray_quantifier } },
    .{ .bnf = "          | '*'                              ", .func = .{ .reject = stray_quantifier } },
    .{ .bnf = "          | '+'                              ", .func = .{ .reject = stray_quantifier } },
    .{ .bnf = "          | 'quant_exact'                    ", .func = .{ .reject = stray_quantifier } },
    .{ .bnf = "_Error2 ::= '|'                              ", .func = .{ .reject = stray_alternation } },
    .{ .bnf = "_Error3 ::= '-'                              ", .func = .{ .reject = set_non_escaped_minus } },
}, 10000);
fn highlight_error(str: []const u8, begin_i: usize, end_i: usize) void {
    std.debug.print("{s}\x1b[30;47;1m{s}\x1b[0m{s}\n", .{
        str[0..begin_i],
        str[begin_i .. end_i + 1],
        str[end_i + 1 ..],
    });
}
pub fn stray_quantifier(allocator: std.mem.Allocator, q: []const Token) anyerror![]const u8 {
    return std.fmt.allocPrint(
        allocator,
        "At string index[{}..{}], stray unescaped quantifier found.\nNo subexpression (character, unicode, character set, set, or group expression) used before this quantifier.\n",
        .{ q[0].begin, q[q.len - 1].end },
    );
}
pub fn stray_alternation(allocator: std.mem.Allocator, a: []const Token) anyerror![]const u8 {
    return std.fmt.allocPrint(allocator, "At string index[{}..{}], stray unescaped '|' found\n", .{ a[0].begin, a[0].end });
}
pub fn set_non_escaped_minus(allocator: std.mem.Allocator, m: []const Token) anyerror![]const u8 {
    return std.fmt.allocPrint(allocator, "At string index[{}..{}], stray unescaped '-' found\n", .{ m[0].begin, m[0].end });
}
const RegexEngine = struct {
    allocator: std.mem.Allocator,
    str: []const u8,
    fsm: RegexFSM,
    token_stack: std.ArrayListUnmanaged(Token) = .{},
    fn init(allocator: std.mem.Allocator, str: []const u8) !RegexEngine {
        return .{ .allocator = allocator, .str = str, .fsm = try RegexFSM.init(allocator) };
    }
    fn construct(self: *RegexEngine, node: RegexBNF.SymbolNode) !void {
        //node.print(0);
        //std.debug.print(ESC("Token stack: {any}\n", .{30}), .{self.token_stack.items});
        if (node == .nt) {
            const rule_range = RegexBNF.RuleRanges[@intFromEnum(node.nt.nt)];
            const rule_call_fn = RegexBNF.RuleCallFn[rule_range.start + node.nt.offset];
            if (rule_call_fn == .accept)
                try rule_call_fn.accept(self);
        } else {
            try self.token_stack.append(self.allocator, node.t);
        }
    }
    fn char(self: *RegexEngine) !void {
        try self.fsm.add_datatype(.{ .char = self.token_stack.pop().tt.char });
    }
    fn unicode(self: *RegexEngine) !void {
        try self.fsm.add_datatype(.{ .unicode = self.token_stack.pop().tt.unicode });
    }
    fn char_set(self: *RegexEngine) !void {
        for (self.token_stack.pop().tt.char_set.datatypes()) |dt| {
            try self.fsm.add_datatype(dt);
        }
    }
    fn set_char_set(self: *RegexEngine) !void {
        for (self.token_stack.pop().tt.char_set.datatypes()) |dt| {
            try self.fsm.add_set_datatype(dt);
        }
    }
    fn set_char(self: *RegexEngine) !void {
        try self.fsm.add_set_datatype(.{ .char = self.token_stack.pop().tt.char });
    }
    fn set_unicode(self: *RegexEngine) !void {
        try self.fsm.add_set_datatype(.{ .unicode = self.token_stack.pop().tt.unicode });
    }
    fn set_range_char(self: *RegexEngine) !void {
        const ch_max_token = self.token_stack.pop();
        std.debug.assert(ch_max_token.tt == .char);
        std.debug.assert(self.token_stack.pop().tt == .@"-");
        const ch_min_token = self.token_stack.pop();
        std.debug.assert(ch_min_token.tt == .char);
        if (ch_min_token.tt.char > ch_max_token.tt.char) {
            highlight_error(self.str, ch_min_token.begin, ch_max_token.end);
            std.debug.print(ESC("At string index[{}..{}], range values are backwards\n", .{ 1, 31 }), .{ ch_min_token.begin, ch_max_token.end });
            return error.Reversed;
        }
        try self.fsm.add_set_datatype(.{ .range = .{ .min = ch_min_token.tt.char, .max = ch_max_token.tt.char } });
    }
    fn set_range_unicode(self: *RegexEngine) !void {
        const un_max_token = self.token_stack.pop();
        std.debug.assert(un_max_token.tt == .unicode);
        std.debug.assert(self.token_stack.pop().tt == .@"-");
        const un_min_token = self.token_stack.pop();
        std.debug.assert(un_min_token.tt == .unicode);
        if (un_min_token.tt.unicode > un_max_token.tt.unicode) {
            highlight_error(self.str, un_min_token.begin, un_max_token.end);
            std.debug.print(ESC("At string index[{}..{}], range values are backwards\n", .{ 1, 31 }), .{ un_min_token.begin, un_max_token.end });
            return error.Reversed;
        }
        try self.fsm.add_set_datatype(.{ .range = .{ .min = un_min_token.tt.unicode, .max = un_max_token.tt.unicode } });
    }
    fn set(self: *RegexEngine) !void {
        std.debug.assert(self.token_stack.pop().tt == .@"[");
        try self.fsm.add_set();
    }
    fn set_complement(self: *RegexEngine) !void {
        std.debug.assert(self.token_stack.pop().tt == .@"set^");
        std.debug.assert(self.token_stack.pop().tt == .@"[");
        try self.fsm.add_set_complement();
    }
    fn set_end(self: *RegexEngine) !void {
        std.debug.assert(self.token_stack.pop().tt == .@"]");
    }
    fn optional(self: *RegexEngine) !void {
        std.debug.assert(self.token_stack.pop().tt == .@"?");
        try self.fsm.optional();
    }
    fn kleene(self: *RegexEngine) !void {
        std.debug.assert(self.token_stack.pop().tt == .@"*");
        try self.fsm.kleene_star();
    }
    fn plus(self: *RegexEngine) !void {
        std.debug.assert(self.token_stack.pop().tt == .@"+");
        try self.fsm.plus();
    }
    fn quant_exact(self: *RegexEngine) !void {
        try self.fsm.repetition(self.token_stack.pop().tt.quant_exact);
    }
    fn quant_between(self: *RegexEngine) !void {
        const lte_token = self.token_stack.pop();
        std.debug.assert(lte_token.tt == .quant_lte);
        const gte_token = self.token_stack.pop();
        std.debug.assert(gte_token.tt == .quant_gte);
        if (gte_token.tt.quant_gte > lte_token.tt.quant_lte) {
            highlight_error(self.str, gte_token.begin, lte_token.end);
            std.debug.print(ESC("At string index[{}..{}], range values are backwards\n", .{ 1, 31 }), .{ gte_token.begin, lte_token.end });
            return error.Reversed;
        }
        try self.fsm.repetition_between(gte_token.tt.quant_gte, lte_token.tt.quant_lte);
    }
    fn quant_lte(self: *RegexEngine) !void {
        try self.fsm.repetition_lte(self.token_stack.pop().tt.quant_lte);
    }
    fn quant_gte(self: *RegexEngine) !void {
        try self.fsm.repetition_gte(self.token_stack.pop().tt.quant_gte);
    }
    fn alternation(self: *RegexEngine) !void {
        std.debug.assert(self.token_stack.pop().tt == .@"|");
        try self.fsm.alternation();
    }
    fn concatenation(self: *RegexEngine) !void {
        try self.fsm.concatenation();
    }
    fn empty_ssm(self: *RegexEngine) !void {
        try self.fsm.empty();
    }
    fn clear_p_empty_ssm(self: *RegexEngine) !void {
        std.debug.assert(self.token_stack.pop().tt == .@")");
        std.debug.assert(self.token_stack.pop().tt == .@"(");
        try self.fsm.empty();
    }
    fn clear_p(self: *RegexEngine) !void {
        std.debug.assert(self.token_stack.pop().tt == .@")");
        std.debug.assert(self.token_stack.pop().tt == .@"(");
    }
    fn finalize(self: *RegexEngine) !void {
        std.debug.print(ESC("Sub state machines: {any}\n", .{1}), .{self.fsm.substate_machines.items});
        for (self.fsm.states.items) |state| std.debug.print("{}\n", .{state});
        try self.fsm.nfa_to_dfa();
        std.debug.print(ESC("Sub state machines: {any}\n", .{1}), .{self.fsm.substate_machines.items});
        for (self.fsm.states.items) |state| std.debug.print("{}\n", .{state});
        try self.fsm.myhill_nerode();
        std.debug.print(ESC("Sub state machines: {any}\n", .{1}), .{self.fsm.substate_machines.items});
        for (self.fsm.states.items) |state| std.debug.print("{}\n", .{state});
    }
    fn deinit(self: *RegexEngine) void {
        self.token_stack.deinit(self.allocator);
        self.fsm.deinit();
    }
};
/// Returns root node of the syntax tree.
fn create_parse_tree(allocator: std.mem.Allocator, lexer: RegexLexer) !RegexBNF.SymbolNode {
    //std.debug.print("{any}\n", .{lexer.token_array.items});
    //.r is the offset of the rules of a symbol. .i is the offset of the symbol array within a rule.
    const RuleCursor = struct {
        rule: u16 = 0,
        offset: u16 = 0,
        node: *RegexBNF.SymbolNode,
        pub fn format(self: @This(), comptime _: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
            try writer.writeByte('{');
            try std.fmt.formatBuf(@tagName(self.node.nt.nt), options, writer);
            try writer.writeAll("}:[");
            try std.fmt.formatInt(self.rule, 10, .lower, options, writer);
            try writer.writeByte(',');
            try std.fmt.formatInt(self.offset, 10, .lower, options, writer);
            try writer.writeByte(']');
        }
    };
    var root = try RegexBNF.SymbolNode.init_nonterm_root(allocator, .Regex, 0);
    errdefer _ = root.deinit(allocator);
    var cursors = try std.ArrayList(RuleCursor).initCapacity(allocator, 1);
    defer cursors.deinit();
    cursors.appendAssumeCapacity(.{ .node = &root });
    var lexer_i: u32 = 0;
    new_cursor: while (lexer_i < lexer.token_array.items.len) {
        //std.debug.print("{any} ({})\n", .{ cursors.items, cursors.items.len });
        var cursor_now: *RuleCursor = &cursors.items[cursors.items.len - 1];
        const rule_range = RegexBNF.RuleRanges[@intFromEnum(cursor_now.node.nt.nt)];
        new_rule: while (cursor_now.rule < rule_range.count) : (cursor_now.rule += 1) {
            const rule_now = RegexBNF.Rules[rule_range.start + cursor_now.rule];
            //std.debug.print(ESC("Reading rule[{}]: {}\n", .{1}), .{ cursor_now.rule, rule_now });
            while (cursor_now.offset < rule_now.sym.len) : (cursor_now.offset += 1) {
                const symbol_now = rule_now.sym[cursor_now.offset];
                const token_compare = lexer.token_array.items[lexer_i];
                //std.debug.print(ESC("{} =? {} [{}]? ", .{ 1, 35 }), .{ symbol_now, term_compare, lexer_i });
                if (symbol_now == .nt) { //Search through a sub nonterminal's rules recursively.
                    //std.debug.print(ESC("Adding nonterminal {} to stack\n", .{ 1, 33 }), .{symbol_now});
                    const symbol_nt = try RegexBNF.SymbolNode.init_nonterm(allocator, symbol_now.nt, 0);
                    errdefer {
                        _ = symbol_nt.deinit(allocator);
                        allocator.destroy(symbol_nt);
                    }
                    cursor_now.node.nt.push(symbol_nt);
                    try cursors.append(.{ .node = symbol_nt });
                    continue :new_cursor;
                } else { //Check if terminal symbol is equal and push it.
                    if (@intFromEnum(token_compare.tt) == symbol_now.t) {
                        //std.debug.print(ESC("Equal\n", .{ 1, 32 }), .{});
                        cursor_now.node.nt.push(try RegexBNF.SymbolNode.init_term(allocator, token_compare));
                        lexer_i += 1;
                    } else {
                        //std.debug.print(ESC("Unequal\n", .{ 1, 31 }), .{});
                        if (cursor_now.rule != rule_range.count - 1) {
                            const children = try cursor_now.node.nt.extract(allocator);
                            defer allocator.free(children);
                            errdefer {
                                for (children) |child| {
                                    _ = child.deinit(allocator);
                                    allocator.destroy(child);
                                }
                            }
                            try cursor_now.node.nt.change(allocator, cursor_now.rule + 1);
                            const new_rule_len = cursor_now.node.nt.nodes.len;
                            var new_offset: u16 = 0;
                            while (new_offset < cursor_now.offset) : (new_offset += 1) { //Read the next rule if any children fit consecutively from left to right.
                                smaller_ruleset: {
                                    if (new_offset == new_rule_len) break :smaller_ruleset;
                                    const symbol_cmp = rule_now.sym[new_offset];
                                    const child = children[new_offset];
                                    if (child.eq_sym(symbol_cmp)) {
                                        cursor_now.node.nt.push(child);
                                        continue;
                                    }
                                }
                                var delete_offset = new_offset;
                                while (delete_offset < cursor_now.offset) : (delete_offset += 1) {
                                    const delete_child = children[delete_offset];
                                    lexer_i -= delete_child.deinit(allocator); //Revert tokenizer_i due to deleted children.
                                    allocator.destroy(delete_child);
                                }
                                break;
                            }
                            cursor_now.offset = new_offset;
                            continue :new_rule;
                        }
                        //std.debug.print(ESC("All rules are unequal\n", .{ 1, 31 }), .{});
                        _ = cursors.pop();
                        cursor_now = if (cursors.items.len != 0) &cursors.items[cursors.items.len - 1] else {
                            //std.debug.print(ESC("This is not a Regex string\n", .{ 1, 31 }), .{});
                            return error.ParsingFailed;
                        };
                        const last_rule_node = cursor_now.node.nt.pop(); //Revert tokenizer_i if all rules are unequal.
                        lexer_i -= last_rule_node.deinit(allocator);
                        allocator.destroy(last_rule_node);
                        cursor_now.rule += 1;
                        continue :new_cursor;
                    }
                }
            }
            const expected_rule = RegexBNF.Rules[rule_range.start + cursor_now.node.nt.offset];
            if (!rule_now.eq(expected_rule)) { //Change rule if the while loop skipped checking the last rule.
                //std.debug.print(ESC("Rule {}\nmismatches symbol node\nRule {}\n", .{ 1, 31 }), .{ rule_now, expected_rule });
                const children = try cursor_now.node.nt.extract(allocator);
                defer allocator.free(children);
                errdefer {
                    for (children) |child| {
                        _ = child.deinit(allocator);
                        allocator.destroy(child);
                    }
                }
                try cursor_now.node.nt.change(allocator, cursor_now.rule); //Copy and paste of "Unequal" symbol algorithm, but without +1
                const new_rule_len = cursor_now.node.nt.nodes.len;
                var new_offset: u16 = 0;
                while (new_offset < cursor_now.offset) : (new_offset += 1) { //Read the next rule if any children fit consecutively from left to right.
                    smaller_ruleset: {
                        if (new_offset == new_rule_len) break :smaller_ruleset;
                        const symbol_cmp = rule_now.sym[new_offset];
                        const child = children[new_offset];
                        if (child.eq_sym(symbol_cmp)) {
                            cursor_now.node.nt.push(child);
                            continue;
                        }
                    }
                    var delete_offset = new_offset;
                    while (delete_offset < cursor_now.offset) : (delete_offset += 1) {
                        const delete_child = children[delete_offset];
                        lexer_i -= delete_child.deinit(allocator); //Revert tokenizer_i due to deleted children.
                        allocator.destroy(delete_child);
                    }
                    break;
                }
                cursor_now.offset = new_offset;
            } else { //A nonterminal rule is all equal.
                const rule_call_fn = RegexBNF.RuleCallFn[rule_range.start + cursor_now.rule];
                if (rule_call_fn == .reject) {
                    var tokens_to_print: std.ArrayListUnmanaged(Token) = .{};
                    defer tokens_to_print.deinit(allocator);
                    for (cursor_now.node.nt.nodes[0..cursor_now.node.nt.count]) |token| {
                        try tokens_to_print.append(allocator, token.t);
                    }
                    const error_string = try rule_call_fn.reject(allocator, tokens_to_print.items);
                    defer allocator.free(error_string);
                    const begin_token = tokens_to_print.items[0];
                    const end_token = tokens_to_print.items[tokens_to_print.items.len - 1];
                    highlight_error(lexer.str, begin_token.begin, end_token.end);
                    std.debug.print(ESC("{s}\n", .{ 1, 31 }), .{error_string});
                    return error.MatchedRejectedRule;
                }
                //std.debug.print(ESC("Approved rule {}\n", .{ 1, 32 }), .{rule_now});
                _ = cursors.pop();
                if (cursors.items.len != 0) cursors.items[cursors.items.len - 1].offset += 1 else {
                    //std.debug.print(ESC("This is a Regex string\n", .{ 1, 32 }), .{});
                    return root;
                }
            }
            continue :new_cursor;
        }
        //std.debug.print(ESC("All rules are unequal\n", .{ 1, 31 }), .{});
        _ = cursors.pop();
        cursor_now = if (cursors.items.len != 0) &cursors.items[cursors.items.len - 1] else {
            //std.debug.print(ESC("This is not a Regex string\n", .{ 1, 31 }), .{});
            return error.ParsingFailed;
        };
        const last_rule_node = cursor_now.node.nt.pop(); //Revert tokenizer_i if all rules are unequal.
        lexer_i -= last_rule_node.deinit(allocator);
        allocator.destroy(last_rule_node);
        cursor_now.rule += 1;
    }
    return root;
}
pub fn main_loop(allocator: std.mem.Allocator) void {
    while (true) {
        (err_label: {
            std.debug.print("Type regular expression here to convert to minimized DFA: ", .{});
            const regex_str = std.io.getStdIn().reader().readUntilDelimiterAlloc(allocator, '\n', 4096) catch |e| break :err_label e;
            defer allocator.free(regex_str);
            const regex_str_no_r = regex_str[0 .. regex_str.len - 1];
            var lexer = RegexLexer.init(allocator, regex_str_no_r) catch |e| break :err_label e;
            defer lexer.deinit();
            const parse_tree = create_parse_tree(allocator, lexer) catch |e| break :err_label e;
            defer _ = parse_tree.deinit(allocator);
            var rc: RegexEngine = RegexEngine.init(allocator, lexer.str) catch |e| break :err_label e;
            defer rc.deinit();
            parse_tree.construct(&rc, RegexEngine.construct) catch |e| break :err_label e;
        }) catch std.debug.print("DFA Compilation failure...\n", .{});
    }
}
pub fn main() !void {
    var gpa: std.heap.GeneralPurposeAllocator(.{}) = .{};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    main_loop(allocator);
}
pub fn ESC(comptime str: []const u8, comptime codes: anytype) []const u8 {
    if (codes.len == 0) @compileError("At least one code is required");
    var return_this: []const u8 = "\x1b[";
    for (codes, 0..) |code, i|
        return_this = return_this ++ std.fmt.comptimePrint("{}{c}", .{ code, if (i != codes.len - 1) ';' else 'm' });
    return_this = return_this ++ str ++ "\x1b[0m";
    return return_this;
}
