const std = @import("std");
const regex_fsm = @import("regex_fsm.zig");
const RegexFSM = regex_fsm.RegexFSM;
test {
    _ = regex_fsm;
    _ = @import("sorted_list.zig");
}
const Token = union(enum) {
    char: u8,
    unicode: u16,
    quant_exact: u32,
    quant_lte: u32,
    quant_gte: u32,
    @"^": void,
    @"$": void,
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
    fn eq_tag(self: Token, num: GetMinTagType(Token)) bool {
        return @intFromEnum(self) == num;
    }
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
const ParseState = union(enum) {
    begin: void,
    escaped: void,
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
    pub fn init(allocator: std.mem.Allocator, regex_str: []const u8) !RegexLexer {
        var parse_state: ParseState = .begin;
        var token_array: RegexTokenArray = RegexTokenArray.init(allocator);
        errdefer token_array.deinit();
        var i: u32 = 0;
        while (i < regex_str.len) : (i += 1) {
            const c = regex_str[i];
            switch (parse_state) {
                .begin => {
                    switch (c) {
                        '^' => if (i != 0) try token_array.append(.{ .char = c }) else try token_array.append(.@"^"),
                        '$' => if (i != regex_str.len - 1) try token_array.append(.{ .char = c }) else try token_array.append(.@"$"),
                        '(' => try token_array.append(.@"("),
                        ')' => try token_array.append(.@")"),
                        '|' => try token_array.append(.@"|"),
                        '?' => try token_array.append(.@"?"),
                        '*' => try token_array.append(.@"*"),
                        '+' => try token_array.append(.@"+"),
                        '{' => parse_state = .{ .quant_lhs = regex_str[i + 1 .. i + 1] }, //Set as 0 length pointing to the next character.
                        '\\' => parse_state = .escaped,
                        '[' => {
                            try token_array.append(.@"[");
                            parse_state = .{ .set = @intCast(i + 1) };
                        },
                        else => try token_array.append(.{ .char = c }),
                    }
                },
                .set => |set_begin_i| {
                    switch (c) {
                        '^' => if (set_begin_i != i) try token_array.append(.{ .char = '^' }) else try token_array.append(.@"set^"),
                        '-' => try token_array.append(.@"-"),
                        ']' => {
                            try token_array.append(.@"]");
                            parse_state = .begin;
                        },
                        else => try token_array.append(.{ .char = c }),
                    }
                },
                .escaped => {
                    switch (c) {
                        '0' => try token_array.append(.{ .char = 0 }),
                        'v' => try token_array.append(.{ .char = 11 }),
                        'f' => try token_array.append(.{ .char = 12 }),
                        'r' => try token_array.append(.{ .char = '\r' }),
                        't' => try token_array.append(.{ .char = '\t' }),
                        'n' => try token_array.append(.{ .char = '\n' }),
                        'x' => {
                            if (i >= regex_str.len - 2) return LexerError.NotEnoughCharactersX;
                            const num = try std.fmt.parseInt(u8, regex_str[i + 1 .. i + 3], 16);
                            try token_array.append(.{ .char = num });
                            i += 2;
                        },
                        'u' => {
                            if (i >= regex_str.len - 4) return LexerError.NotEnoughCharactersU;
                            const num = try std.fmt.parseInt(u16, regex_str[i + 1 .. i + 5], 16);
                            try token_array.append(.{ .unicode = num });
                            i += 4;
                        },
                        '+', '*', '?', '^', '$', '\\', '.', '[', ']', '{', '}', '(', ')', '|', '/' => |ch| try token_array.append(.{ .char = ch }),
                        else => return LexerError.InvalidCharacterToEscape,
                    }
                    parse_state = .begin;
                },
                .quant_lhs => |*num_str| {
                    switch (c) {
                        '0'...'9' => num_str.len += 1,
                        '}' => {
                            if (num_str.len == 0) return LexerError.LHSQuantifierNumberRequired;
                            const num = try std.fmt.parseInt(u32, num_str.*, 10);
                            try token_array.append(.{ .quant_exact = num });
                            parse_state = .begin;
                        },
                        ',' => {
                            if (num_str.len != 0) {
                                const num = try std.fmt.parseInt(u32, num_str.*, 10);
                                try token_array.append(.{ .quant_gte = num });
                                parse_state = .{ .quant_rhs_with_left = regex_str[i + 1 .. i + 1] };
                            } else {
                                parse_state = .{ .quant_rhs_no_left = regex_str[i + 1 .. i + 1] };
                            }
                        },
                        else => return LexerError.NotANumberQuantifier,
                    }
                },
                .quant_rhs_with_left => |*num_str| {
                    switch (c) {
                        '0'...'9' => num_str.len += 1,
                        '}' => { //For {number1,number2} or {number1,}
                            if (num_str.len != 0) {
                                const num = try std.fmt.parseInt(u32, num_str.*, 10);
                                try token_array.append(.{ .quant_lte = num });
                            }
                            parse_state = .begin;
                        },
                        else => return LexerError.NotANumberQuantifier,
                    }
                },
                .quant_rhs_no_left => |*num_str| {
                    switch (c) {
                        '0'...'9' => num_str.len += 1,
                        '}' => { //For {,number}
                            if (num_str.len != 0) {
                                const num = try std.fmt.parseInt(u32, num_str.*, 10);
                                try token_array.append(.{ .quant_lte = num });
                            } else return LexerError.RHSQuantifierNumberRequired;
                            parse_state = .begin;
                        },
                        else => return LexerError.NotANumberQuantifier,
                    }
                },
            }
        }
        try token_array.append(.eof);
        switch (parse_state) {
            .escaped => return LexerError.EscapedAtEnd,
            .set => return LexerError.MissingSquareBracketEnd,
            .quant_lhs => return LexerError.MissingEndQuantifier,
            .quant_rhs_with_left => return LexerError.MissingEndCurlyBracketQuantifier,
            .quant_rhs_no_left => return LexerError.MissingEndCurlyBracketQuantifier,
            else => return .{ .token_array = token_array },
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
    SubExpr,
    Group,
    GExpr,
    GMExpr,
    GAMExpr,
    Set,
    Set2,
    SetExpr,
};
fn BNFParser(comptime _TermT: type, comptime _NonTermT: type, comptime _ConstructorT: type, comptime rules: []const struct { bnf: []const u8, func: fn (*_ConstructorT, u32) anyerror!void }, comptime quota: u32) type {
    return struct {
        pub const TermT = _TermT;
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
                std.debug.assert(self.count < @as(u8, @intCast(self.nodes.len)));
                self.nodes[self.count] = symbol;
                self.count += 1;
            }
            pub fn pop(self: *NonTermTNode) *SymbolNode {
                std.debug.assert(self.count != 0);
                defer self.count -= 1;
                return self.nodes[self.count - 1];
            }
            /// If a rule is not correct. Salvage children nodes for the next rule.
            pub fn extract(self: *NonTermTNode) []*SymbolNode {
                defer self.count = 0;
                return self.nodes[0..self.count];
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
            t: TermT,
            nt: NonTermTNode,
            /// Created on allocator because its lifetime should be a descendant of a root SymbolNode (To be deallocated)
            pub fn init_term(allocator: std.mem.Allocator, term: TermT) !*SymbolNode {
                const self = try allocator.create(SymbolNode);
                self.* = .{ .t = term };
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
                    return sym.eq(.{ .t = @intFromEnum(self.t) });
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
        pub const ConstructFns = f: {
            var _fns: []const *const fn (*_ConstructorT, u32) anyerror!void = &.{};
            for (rules) |rule|
                _fns = _fns ++ &[1]*const fn (*_ConstructorT, u32) anyerror!void{rule.func};
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
                        inline '\'', '"' => |ch| {
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
const RegexBNF = BNFParser(Token, NonTermEnum, RegexEngine, &.{
    .{ .bnf = "Regex ::= '^' MExpr '$' 'eof'           ", .func = RegexEngine.finalize },
    .{ .bnf = "        | '^' MExpr 'eof'               ", .func = RegexEngine.finalize },
    .{ .bnf = "        | '^' '$' 'eof'                 ", .func = RegexEngine.finalize },
    .{ .bnf = "        | '^' 'eof'                     ", .func = RegexEngine.finalize },
    .{ .bnf = "        | MExpr '$' 'eof'               ", .func = RegexEngine.finalize },
    .{ .bnf = "        | MExpr 'eof'                   ", .func = RegexEngine.finalize },
    .{ .bnf = "        | '$' 'eof'                     ", .func = RegexEngine.finalize },
    .{ .bnf = "        | 'eof'                         ", .func = RegexEngine.finalize },
    .{ .bnf = "MExpr ::= Expr '|' MExpr                ", .func = RegexEngine.alternation }, //TODO Write "Alternation SubExpression" rule set in SubExpr instead
    .{ .bnf = "        | Expr MExpr                    ", .func = RegexEngine.concatenation },
    .{ .bnf = "        |                               ", .func = RegexEngine.empty_ssm },
    .{ .bnf = "Expr ::= SubExpr 'quant_gte' 'quant_lte'", .func = RegexEngine.quant_between },
    .{ .bnf = "       | SubExpr 'quant_gte'            ", .func = RegexEngine.quant_gte },
    .{ .bnf = "       | SubExpr 'quant_lte'            ", .func = RegexEngine.quant_lte },
    .{ .bnf = "       | SubExpr '?'                    ", .func = RegexEngine.optional },
    .{ .bnf = "       | SubExpr '*'                    ", .func = RegexEngine.kleene },
    .{ .bnf = "       | SubExpr '+'                    ", .func = RegexEngine.plus },
    .{ .bnf = "       | SubExpr 'quant_exact'          ", .func = RegexEngine.quant_exact },
    .{ .bnf = "       | SubExpr                        ", .func = RegexEngine.nothing },
    .{ .bnf = "SubExpr ::= 'char'                      ", .func = RegexEngine.char },
    .{ .bnf = "          | 'unicode'                   ", .func = RegexEngine.unicode },
    .{ .bnf = "          | Group                       ", .func = RegexEngine.nothing },
    .{ .bnf = "          | Set                         ", .func = RegexEngine.nothing },
    .{ .bnf = "Group ::= '(' GExpr ')'                 ", .func = RegexEngine.nothing },
    .{ .bnf = "        | '(' ')'                       ", .func = RegexEngine.nothing },
    .{ .bnf = "GExpr ::= GMExpr GAMExpr                ", .func = RegexEngine.nothing },
    .{ .bnf = "GMExpr ::= Expr GMExpr                  ", .func = RegexEngine.nothing },
    .{ .bnf = "         |                              ", .func = RegexEngine.nothing },
    .{ .bnf = "GAMExpr ::= '|' GMExpr GAMExpr          ", .func = RegexEngine.nothing },
    .{ .bnf = "          |                             ", .func = RegexEngine.nothing },
    .{ .bnf = "Set ::= '[' 'set^' Set2                 ", .func = RegexEngine.nothing },
    .{ .bnf = "      | '[' Set2                        ", .func = RegexEngine.nothing },
    .{ .bnf = "Set2 ::= SetExpr Set2                   ", .func = RegexEngine.nothing },
    .{ .bnf = "       | ']'                            ", .func = RegexEngine.nothing },
    .{ .bnf = "SetExpr ::= 'char' '-' 'char'           ", .func = RegexEngine.nothing },
    .{ .bnf = "          | 'char'                      ", .func = RegexEngine.nothing },
    .{ .bnf = "          | 'unicode' '-' 'unicode'     ", .func = RegexEngine.nothing },
    .{ .bnf = "          | 'unicode'                   ", .func = RegexEngine.nothing },
}, 10000);
const RegexEngine = struct {
    allocator: std.mem.Allocator,
    fsm: RegexFSM,
    token_stack: std.ArrayListUnmanaged(Token) = .{},
    fn init(allocator: std.mem.Allocator) !RegexEngine {
        return .{ .allocator = allocator, .fsm = try RegexFSM.init(allocator) };
    }
    fn construct(self: *RegexEngine, node: RegexBNF.SymbolNode) !void {
        node.print(0);
        if (node == .nt) {
            const rule_range = RegexBNF.RuleRanges[@intFromEnum(node.nt.nt)];
            try RegexBNF.ConstructFns[rule_range.start + node.nt.offset](self, node.nt.count);
        } else {
            try self.token_stack.append(self.allocator, node.t);
        }
    }
    fn char(self: *RegexEngine, _: u32) !void {
        try self.fsm.add_datatype(.{ .char = self.token_stack.pop().char });
    }
    fn unicode(self: *RegexEngine, _: u32) !void {
        try self.fsm.add_datatype(.{ .unicode = self.token_stack.pop().unicode });
    }
    fn optional(self: *RegexEngine, _: u32) !void {
        std.debug.assert(self.token_stack.pop() == .@"?");
        try self.fsm.optional();
    }
    fn kleene(self: *RegexEngine, _: u32) !void {
        std.debug.assert(self.token_stack.pop() == .@"*");
        try self.fsm.kleene_star();
    }
    fn plus(self: *RegexEngine, _: u32) !void {
        std.debug.assert(self.token_stack.pop() == .@"+");
        try self.fsm.plus();
    }
    fn quant_exact(self: *RegexEngine, _: u32) !void {
        try self.fsm.repetition(self.token_stack.pop().quant_exact);
    }
    fn quant_between(self: *RegexEngine, _: u32) !void {
        const lte = self.token_stack.pop().quant_lte;
        try self.fsm.repetition_between(self.token_stack.pop().quant_gte, lte);
    }
    fn quant_lte(self: *RegexEngine, _: u32) !void {
        try self.fsm.repetition_lte(self.token_stack.pop().quant_lte);
    }
    fn quant_gte(self: *RegexEngine, _: u32) !void {
        try self.fsm.repetition_gte(self.token_stack.pop().quant_gte);
    }
    fn alternation(self: *RegexEngine, _: u32) !void {
        std.debug.assert(self.token_stack.pop() == .@"|");
        try self.fsm.alternation();
    }
    fn concatenation(self: *RegexEngine, _: u32) !void {
        try self.fsm.concatenation();
    }
    fn empty_ssm(self: *RegexEngine, _: u32) !void {
        try self.fsm.empty();
    }
    fn finalize(self: *RegexEngine, _: u32) !void {
        std.debug.print(ESC("Regex converted to NFA\n", .{ 1, 32 }), .{});
        std.debug.print(ESC("Sub state machines: {any}\n", .{1}), .{self.fsm.substate_machines.items});
        for (self.fsm.states.items) |state| std.debug.print("{}\n", .{state});
        try self.fsm.nfa_to_dfa();
        std.debug.print(ESC("Sub state machines: {any}\n", .{1}), .{self.fsm.substate_machines.items});
        for (self.fsm.states.items) |state| std.debug.print("{}\n", .{state});
        try self.fsm.myhill_nerode();
        std.debug.print(ESC("Sub state machines: {any}\n", .{1}), .{self.fsm.substate_machines.items});
        for (self.fsm.states.items) |state| std.debug.print("{}\n", .{state});
    }
    fn nothing(self: *RegexEngine, s: u32) !void {
        _ = self; // autofix
        _ = s; // autofix
    }
    fn deinit(self: *RegexEngine) void {
        self.token_stack.deinit(self.allocator);
        self.fsm.deinit();
    }
};
/// Returns root node of the syntax tree.
fn create_parse_tree(allocator: std.mem.Allocator, lexer: RegexLexer) !RegexBNF.SymbolNode {
    std.debug.print("{any}\n", .{lexer.token_array.items});
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
                const term_compare = lexer.token_array.items[lexer_i];
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
                    if (@intFromEnum(term_compare) == symbol_now.t) {
                        //std.debug.print(ESC("Equal\n", .{ 1, 32 }), .{});
                        cursor_now.node.nt.push(try RegexBNF.SymbolNode.init_term(allocator, term_compare));
                        lexer_i += 1;
                    } else {
                        //std.debug.print(ESC("Unequal\n", .{ 1, 31 }), .{});
                        if (cursor_now.rule != rule_range.count - 1) {
                            const children = cursor_now.node.nt.extract();
                            errdefer {
                                for (children) |child| {
                                    _ = child.deinit(allocator);
                                    allocator.destroy(child);
                                }
                            }
                            try cursor_now.node.nt.change(allocator, cursor_now.rule + 1);
                            var new_offset: u16 = 0;
                            while (new_offset < cursor_now.offset) : (new_offset += 1) { //Read the next rule if any children fit consecutively from left to right.
                                const symbol_cmp = rule_now.sym[new_offset];
                                const child = children[new_offset];
                                if (children[new_offset].eq_sym(symbol_cmp)) {
                                    cursor_now.node.nt.push(child);
                                } else {
                                    var delete_offset = new_offset;
                                    while (delete_offset < cursor_now.offset) : (delete_offset += 1) {
                                        const delete_child = children[delete_offset];
                                        lexer_i -= delete_child.deinit(allocator); //Revert tokenizer_i due to deleted children.
                                        allocator.destroy(delete_child);
                                    }
                                    break;
                                }
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
                const children = cursor_now.node.nt.extract();
                errdefer {
                    for (children) |child| {
                        _ = child.deinit(allocator);
                        allocator.destroy(child);
                    }
                }
                try cursor_now.node.nt.change(allocator, cursor_now.rule);
                var new_offset: u16 = 0;
                while (new_offset < cursor_now.offset) : (new_offset += 1) { //Read the next rule if any children fit consecutively from left to right.
                    const symbol_cmp = rule_now.sym[new_offset];
                    const child = children[new_offset];
                    if (children[new_offset].eq_sym(symbol_cmp)) {
                        cursor_now.node.nt.push(child);
                    } else {
                        var delete_offset = new_offset;
                        while (delete_offset < cursor_now.offset) : (delete_offset += 1) {
                            const delete_child = children[delete_offset];
                            lexer_i -= delete_child.deinit(allocator); //Revert tokenizer_i due to deleted children.
                            allocator.destroy(delete_child);
                        }
                        break;
                    }
                }
                cursor_now.offset = new_offset;
            } else { //A nonterminal rule is all equal.
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
pub fn main() !void {
    var gpa: std.heap.GeneralPurposeAllocator(.{}) = .{};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    var lexer = try RegexLexer.init(allocator, "^");
    defer lexer.deinit();
    const parse_tree = try create_parse_tree(allocator, lexer);
    defer _ = parse_tree.deinit(allocator);
    parse_tree.print_tree(.pre, 0);
    std.debug.print("\n", .{});
    parse_tree.print_tree(.post, 0);
    std.debug.print("\n", .{});
    var rc: RegexEngine = try RegexEngine.init(allocator);
    defer rc.deinit();
    try parse_tree.construct(&rc, RegexEngine.construct);
}
pub fn ESC(comptime str: []const u8, comptime codes: anytype) []const u8 {
    if (codes.len == 0) @compileError("At least one code is required");
    var return_this: []const u8 = "\x1b[";
    for (codes, 0..) |code, i|
        return_this = return_this ++ std.fmt.comptimePrint("{}{c}", .{ code, if (i != codes.len - 1) ';' else 'm' });
    return_this = return_this ++ str ++ "\x1b[0m";
    return return_this;
}
