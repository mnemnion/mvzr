//!
//! mvzr: Minimum Viable Zig Regex
//! A minimalistic, but y'know, viable, Zig regex library.
//!
//! Focused on basic support of runtime-provided regular expressions.
const std = @import("std");
const builtin = @import("builtin");
const assert = std.debug.assert;

const XXX = false;

// Zig is very particular about the types of shifts.
const one: u64 = 1;

/// Maximum regex operations.
pub const MAX_REGEX_OPS = 64;
/// Maximum character sets, ASCII only.
pub const MAX_CHAR_SETS = 8;

const RegexType = enum(u8) {
    unused,
    dot,
    begin,
    end,
    left,
    right,
    alt,
    optional,
    star,
    plus,
    lazy_optional,
    lazy_star,
    lazy_plus,
    char,
    class,
    not_class,
    digit,
    not_digit,
    alpha,
    not_alpha,
    whitespace,
    not_whitespace,
};

pub const RegOp = union(RegexType) {
    unused: void,
    dot: void,
    begin: void,
    end: void,
    left: void,
    right: void,
    alt: void,
    optional: void,
    star: void,
    plus: void,
    lazy_optional: void,
    lazy_star: void,
    lazy_plus: void,
    char: u8, // character byte
    class: u8, // offset into class array
    not_class: u8,
    digit: void,
    not_digit: void,
    alpha: void,
    not_alpha: void,
    whitespace: void,
    not_whitespace: void,
};

pub const CharSet = struct {
    low: u64 = 0,
    hi: u64 = 0,
};

const Match = struct {
    j: usize,
    i: usize,
};

const CharSets = [MAX_CHAR_SETS]CharSet;

const Regex = struct {
    patt: [MAX_REGEX_OPS]RegOp = [1]RegOp{undefined} ** MAX_REGEX_OPS,
    sets: [MAX_CHAR_SETS]CharSet = [1]CharSet{undefined} ** MAX_CHAR_SETS,

    /// Match a regex pattern in `haystack`, if found, this returns `.{start, end}`
    pub fn match(regex: *const Regex, haystack: []const u8) ?struct { usize, usize } {
        if (haystack.len == 0) return null;
        // const set_ptr = regex.sets[0..];
        const patt = regex.patt;
        switch (patt[0]) {
            .begin => {
                const width = matchPattern(patt[1..], &regex.sets, haystack);
                if (width) |w| {
                    return .{ 0, w };
                } else return null;
            },
            else => {
                var matchlen: usize = 0;
                while (matchlen < haystack.len) : (matchlen += 1) {
                    const width = matchPattern(&patt, &regex.sets, haystack[matchlen..]);
                    if (width) |w| {
                        return .{ matchlen, matchlen + w };
                    }
                }
                return null;
            },
        }
    }
};

pub fn match(haystack: []const u8, pattern: []const u8) ?usize {
    const maybe_regex = compile(pattern);
    if (maybe_regex) |regex| {
        return regex.match(haystack);
    } else {
        return null;
    }
}

fn matchPattern(regex: []const RegOp, set: *const CharSets, haystack: []const u8) ?usize {
    var j: usize = 0;
    if (XXX) {
        j += 1;
    }
    const alt_count = countAlt(regex);
    if (alt_count > 0) {
        switch (alt_count) {
            1 => return dispatchTwoAlts(regex, set, haystack),
            2 => return dispatchThreeAlts(regex, set, haystack),
            3 => return dispatchFourAlts(regex, set, haystack),
            else => @panic("NYI"),
        }
    }
    return matchPatternNoAlts(regex, set, haystack);
}

fn matchPatternNoAlts(patt: []const RegOp, sets: *const CharSets, haystack: []const u8) ?usize {
    var i: usize = 0;
    var j: usize = 0;
    while (j < patt.len and patt[j] != .unused) {
        const maybe_match = switch (patt[j]) {
            .dot,
            .class,
            .not_class,
            .digit,
            .not_digit,
            .alpha,
            .not_alpha,
            .whitespace,
            .not_whitespace,
            .char,
            => matchOne(patt[j], sets, haystack[i]),
            .star => matchStar(patt[j + 1 ..], sets, haystack[i..]),
            .plus => matchPlus(patt[j + 1 ..], sets, haystack[i..]),
            .optional => matchOptional(patt[j + 1 ..], sets, haystack[i..]),
            .lazy_star => matchLazyStar(patt[j + 1 ..], sets, haystack[i..]),
            else => stub: {
                std.debug.print("cant match {s}\n", .{@tagName(patt[j])});
                break :stub Match{ .j = 1, .i = 1 };
            },
        };
        if (maybe_match) |m| {
            j += m.j;
            i += m.i;
            assert(!(i > haystack.len));
            if (i == haystack.len) break;
        } else {
            if (i == haystack.len) break;
            return null;
        }
    } // TODO check that we finished the pattern!
    return i;
}

const ascii = std.ascii;

fn matchOne(op: RegOp, sets: *const CharSets, c: u8) ?Match {
    const matched = switch (op) {
        .dot => true, // we match newlines, deal with it
        .class => |c_off| matchClass(sets[c_off], c),
        .not_class => |c_off| !matchClass(sets[c_off], c),
        .digit => ascii.isDigit(c),
        .not_digit => !ascii.isDigit(c),
        .alpha => ascii.isAlphabetic(c),
        .not_alpha => !ascii.isAlphabetic(c),
        .whitespace => ascii.isWhitespace(c),
        .not_whitespace => !ascii.isWhitespace(c),
        .char => |ch| (c == ch),
        else => unreachable,
    };
    if (matched) {
        return Match{ .j = 1, .i = 1 };
    } else {
        return null;
    }
}

fn matchStar(patt: []const RegOp, sets: *const CharSets, haystack: []const u8) ?Match {
    var i: usize = 0;
    const group, const this_patt = thisPattern(patt);
    const match_fn = if (group)
        &matchPattern
    else
        &matchPatternNoAlts;
    while (match_fn(this_patt, sets, haystack[i..])) |m| {
        i += m;
        assert(!(i > haystack.len));
        if (i == haystack.len) break;
    }
    return Match{ .i = i, .j = nextPattern(patt) };
}

fn matchPlus(patt: []const RegOp, sets: *const CharSets, haystack: []const u8) ?Match {
    const group, const this_patt = thisPattern(patt);
    const match_fn = if (group)
        &matchPattern
    else
        &matchPatternNoAlts;
    const first_m = match_fn(this_patt, sets, haystack);
    if (first_m == null) return null;
    const i = first_m.?;
    if (i == haystack.len) return Match{ .i = i, .j = nextPattern(patt) };
    const rest = matchStar(patt, sets, haystack[i..]);
    if (rest) |m| {
        return Match{ .i = i + m.i, .j = m.j };
    } else {
        return Match{ .i = i, .j = nextPattern(patt) };
    }
}

fn matchOptional(patt: []const RegOp, sets: *const CharSets, haystack: []const u8) ?Match {
    const group, const this_patt = thisPattern(patt);
    const match_fn = if (group)
        &matchPattern
    else
        &matchPatternNoAlts;
    const maybe_m = match_fn(this_patt, sets, haystack);
    if (maybe_m) |m| {
        return Match{ .i = m, .j = nextPattern(patt) };
    } else {
        return Match{ .i = 0, .j = nextPattern(patt) };
    }
}

fn matchLazyStar(patt: []const RegOp, sets: *const CharSets, haystack: []const u8) ?Match {
    var group, const this_patt = thisPattern(patt);
    const match_fn = if (group)
        &matchPattern
    else
        &matchPatternNoAlts;
    printPattern(this_patt);
    group, const next_patt = thisPattern(patt[this_patt.len..]);
    const next_fn = if (group)
        &matchPattern
    else
        &matchPatternNoAlts;
    printPattern(next_patt);
    var i: usize = 0;
    while (true) {
        const maybe = match_fn(this_patt, sets, haystack[i..]);
        if (maybe) |m1| {
            // let the other guy have some
            i += m1;
            if (i == haystack.len)
                return Match{ .i = i, .j = nextPattern(patt) };
            const m_next = next_fn(next_patt, sets, haystack[i..]);
            if (m_next) |m2| {
                // done
                i += m2;
                // skip our lazy star and our matcher
                return Match{ .i = i, .j = this_patt.len + nextPattern(patt[this_patt.len..]) };
            }
        } else {
            // other guy's turn coming up
            return Match{ .i = i, .j = nextPattern(patt) };
        }
    }
}

// TODO this backtracks, maybe rethink that (lockstep is annoying)
fn matchFourPatterns(
    first: []const RegOp,
    second: []const RegOp,
    third: []const RegOp,
    fourth: []const RegOp,
    sets: *const CharSets,
    haystack: []const u8,
) ?usize {
    const m1m = matchPatternNoAlts(first, sets, haystack);
    if (m1m) |m1| {
        return m1;
    } else {
        return matchThreePatterns(second, third, fourth, sets, haystack);
    }
}

fn thisPattern(patt: []const RegOp) struct { bool, []const RegOp } {
    switch (patt[0]) {
        .left => return .{ true, sliceGroup(patt) },
        else => return .{ false, patt[0..1] },
    }
}

fn matchThreePatterns(
    first: []const RegOp,
    second: []const RegOp,
    third: []const RegOp,
    sets: *const CharSets,
    haystack: []const u8,
) ?usize {
    const m1m = matchPatternNoAlts(first, sets, haystack);
    if (m1m) |m1| {
        return m1;
    } else {
        return matchTwoPatterns(second, third, sets, haystack);
    }
}

fn matchTwoPatterns(
    first: []const RegOp,
    second: []const RegOp,
    sets: *const CharSets,
    haystack: []const u8,
) ?usize {
    const m1m = matchPatternNoAlts(first, sets, haystack);
    if (m1m) |m1| {
        return m1;
    } else {
        return matchPatternNoAlts(second, sets, haystack);
    }
}

fn sliceAlt(regex: []const RegOp) []const RegOp {
    const alt_at = findAlt(regex, 0);
    if (alt_at) |at| {
        return regex[0..at];
    } else unreachable; // verified before dispatch
}

fn sliceGroup(patt: []const RegOp) []const RegOp {
    assert(patt[0] == .left);
    var j: usize = 1;
    var pump: usize = 0;
    while (true) : (j += 1) {
        switch (patt[j]) {
            .right => {
                if (pump == 0) {
                    return patt[1..j];
                } else {
                    pump -= 1;
                }
            },
            .left => pump += 1,
            else => {},
        }
    }
    unreachable;
}

fn dispatchTwoAlts(regex: []const RegOp, set: *const CharSets, haystack: []const u8) ?usize {
    const first = sliceAlt(regex);
    const second = regex[first.len + 1 ..];
    return matchTwoPatterns(first, second, set, haystack);
}

fn dispatchThreeAlts(regex: []const RegOp, set: *const CharSets, haystack: []const u8) ?usize {
    const first = sliceAlt(regex);
    const second = sliceAlt(regex[first.len + 1 ..]);
    const third = regex[second.len + 1 ..];
    return matchThreePatterns(first, second, third, set, haystack);
}

fn dispatchFourAlts(regex: []const RegOp, set: *const CharSets, haystack: []const u8) ?usize {
    const first = sliceAlt(regex);
    const second = sliceAlt(regex[first.len + 1 ..]);
    const third = sliceAlt(regex[second.len + 1 ..]);
    const fourth = regex[second.len + 1 ..];
    return matchFourPatterns(first, second, third, fourth, set, haystack);
}

fn matchClass(set: CharSet, c: u8) bool {
    switch (c) {
        0...63 => {
            const cut_c: u6 = @truncate(c);
            return (set.low | (one << cut_c)) == set.low;
        },
        64...127 => {
            const cut_c: u6 = @truncate(c);
            return (set.hi | (one << cut_c)) == set.hi;
        },
        else => return false,
    }
}

fn nextPattern(patt: []const RegOp) usize {
    switch (patt[0]) {
        .left => return findRight(patt, 0) + 1,
        else => return 1,
    }
}

// Count alts which aren't in a group
fn countAlt(patt: []const RegOp) usize {
    var pump: usize = 0;
    var alts: usize = 0;
    for (patt) |op| {
        switch (op) {
            .left => {
                pump += 1;
            },
            .right => {
                pump -= 1;
            },
            .alt => {
                if (pump == 0) {
                    alts += 1;
                }
            },
            else => {},
        }
    }
    return alts;
}

fn findAlt(patt: []const RegOp, j_in: usize) ?usize {
    var j = j_in;
    while (j < patt.len) : (j += 1) {
        if (patt[j] == .alt) {
            return j;
        }
    }
    return null;
}

fn findRight(patt: []const RegOp, j_in: usize) usize {
    // Compiler made sure these are matched
    var j = j_in;
    var pump: usize = 0;
    while (j < patt.len) : (j += 1) {
        if (patt[j] == .right and pump == 0)
            return j
        else
            continue;
        if (patt[j] == .left) pump += 1;
    }
    unreachable;
}

/// Move modifiers to prefix position.
fn prefixModifier(patt: *[MAX_REGEX_OPS]RegOp, j: usize, op: RegOp) void {
    var find_j = j - 1;
    // If we already have a modifier, two are not kosher:
    switch (patt[find_j]) {
        .begin,
        .end,
        .left,
        .alt,
        .optional,
        .star,
        .plus,
        .lazy_optional,
        .lazy_star,
        .lazy_plus,
        => @panic("throw here"),
        .right => {
            find_j = beforePriorLeft(patt, find_j);
        },
        else => {},
    } // find_j is at our insert offset
    var move_op = patt[find_j];
    patt[find_j] = op;
    find_j += 1;
    while (move_op != .unused) : (find_j += 1) {
        const temp_op = patt[find_j];
        patt[find_j] = move_op;
        move_op = temp_op;
    }
}

fn beforePriorLeft(patt: *const [MAX_REGEX_OPS]RegOp, j: usize) usize {
    std.debug.assert(patt[j] == .right);
    var find_j = j - 1;
    var pump: usize = 0;
    while (find_j != 0) : (find_j -= 1) {
        switch (patt[find_j]) {
            .right => {
                pump += 1;
            },
            .left => {
                if (pump == 0)
                    break
                else
                    pump -= 1;
            },
            else => {},
        }
    }
    if (patt[find_j] != .left) @panic("throw here");
    return find_j;
}

// TODO this should throw errors
/// Compile a regex.
pub fn compile(in: []const u8) ?Regex {
    var out = Regex{};
    var bad_string: bool = false;
    @memset(
        &out.patt,
        RegOp{ .unused = {} },
    );
    @memset(&out.sets, .{ .low = 0, .hi = 0 });
    var patt = &out.patt;
    var sets = &out.sets;
    var i: usize = 0;
    var j: usize = 0;
    var s: u8 = 0;
    var pump: usize = 0;
    dispatch: while (i < in.len and j + 1 < patt.len) : ({
        j += 1;
        i += 1;
    }) {
        const c = in[i];
        switch (c) {
            '$' => {
                patt[j] = RegOp{ .end = {} };
            },
            '^' => {
                patt[j] = RegOp{ .begin = {} };
            },
            '.' => {
                patt[j] = RegOp{ .dot = {} };
            },

            '*' => {
                if (i + 1 < in.len and in[i + 1] == '?') {
                    i += 1;
                    prefixModifier(patt, j, RegOp{ .lazy_star = {} });
                } else {
                    prefixModifier(patt, j, RegOp{ .star = {} });
                }
            },
            '?' => {
                if (i + 1 < in.len and in[i + 1] == '?') {
                    i += 1;
                    prefixModifier(patt, j, RegOp{ .lazy_optional = {} });
                } else {
                    prefixModifier(patt, j, RegOp{ .optional = {} });
                }
            },
            '+' => {
                if (i + 1 < in.len and in[i + 1] == '?') {
                    i += 1;
                    prefixModifier(patt, j, RegOp{ .lazy_plus = {} });
                } else {
                    prefixModifier(patt, j, RegOp{ .plus = {} });
                }
            },
            '|' => {
                patt[j] = RegOp{ .alt = {} };
            },
            '(' => {
                pump += 1;
                patt[j] = RegOp{ .left = {} };
            },
            ')' => {
                if (pump == 0) {
                    bad_string = true;
                    break :dispatch;
                }
                pump -= 1;
                patt[j] = RegOp{ .right = {} };
            },
            '\\' => { // character class or escape
                if (i + 1 == in.len) {
                    bad_string = true;
                    break :dispatch;
                } else {
                    i += 1;
                    // char patterns
                    switch (in[i]) {
                        'd' => {
                            patt[j] = RegOp{ .digit = {} };
                        },
                        'D' => {
                            patt[j] = RegOp{ .not_digit = {} };
                        },
                        'w' => {
                            patt[j] = RegOp{ .alpha = {} };
                        },
                        'W' => {
                            patt[j] = RegOp{ .not_alpha = {} };
                        },
                        's' => {
                            patt[j] = RegOp{ .whitespace = {} };
                        },
                        'S' => {
                            patt[j] = RegOp{ .not_whitespace = {} };
                        },
                        else => |ch| {
                            // Others are accepted as escaped, we don't care
                            // if they're special, you're not special, you're
                            // not my dad, you get the regex you give
                            // TODO ok fine, we need to handle like, \n here
                            patt[j] = RegOp{ .char = ch };
                        },
                    }
                }
            },
            '[' => {
                // character set
                var low: u64 = 0;
                var hi: u64 = 0;
                const this_op: RegOp = which: {
                    if (i + 1 < in.len and in[i + 1] == '^') {
                        i += 1;
                        break :which RegOp{ .not_class = s };
                    } else break :which RegOp{ .class = s };
                };
                i += 1;
                while (i < in.len and in[i] != ']') : (i += 1) {
                    if (s > sets.len) {
                        logError("excessive number of character sets\n", .{});
                        bad_string = true;
                        break :dispatch;
                    }
                    const c1 = in[i];
                    if (i + 1 < in.len and in[i + 1] != '-') {
                        // normal character class
                        switch (c1) {
                            0...63 => {
                                const cut_c: u6 = @truncate(c1);
                                low |= one << cut_c;
                            },
                            64...91, 93...127 => {
                                const cut_c: u6 = @truncate(c1);
                                hi |= one << cut_c;
                            },
                            '\\' => { // escaped value, we don't care what
                                // thought I had established that already but ok
                                // TODO handle \n and such
                                if (i + 1 < in.len) {
                                    i += 1;
                                    const c2 = in[i];
                                    switch (c2) {
                                        0...63 => {
                                            const cut_c: u6 = @truncate(c2);
                                            low |= one << cut_c;
                                        },
                                        64...127 => {
                                            const cut_c: u6 = @truncate(c2);
                                            hi |= one << cut_c;
                                        },
                                        else => {
                                            bad_string = true;
                                            break;
                                        },
                                    }
                                }
                            },
                            else => {
                                bad_string = true;
                                break :dispatch;
                            },
                        }
                    } else {
                        // if paired, it's a range
                        if (i + 2 < in.len and in[i + 2] != ']') {
                            const c_end = which: {
                                if (in[i + 2] != '\\') {
                                    i += 1; // we get one from the while loop
                                    break :which in[i + 1];
                                } else if (i + 3 < in.len) {
                                    i += 2; // likewise
                                    break :which in[i + 2];
                                } else {
                                    // what to do here? don't care, have a 0
                                    break :which 0; // that'll show ya
                                }
                            };
                            if (c1 < c_end + 1) {
                                for (c1..c_end + 1) |c_range| {
                                    switch (c_range) {
                                        0...63 => {
                                            const cut_c: u6 = @truncate(c_range);
                                            low |= one << cut_c;
                                        },
                                        64...127 => {
                                            const cut_c: u6 = @truncate(c_range);
                                            hi |= one << cut_c;
                                        },
                                        else => {
                                            bad_string = true;
                                            break :dispatch;
                                        },
                                    }
                                }
                            } else {
                                bad_string = true;
                                break :dispatch;
                            }
                        } else { // '-' in set, value is 45 so
                            const cut_hyphen: u6 = @truncate('-');
                            low |= one << cut_hyphen;
                        }
                    }
                } // end while
                if (i == in.len or in[i] != ']') {
                    bad_string = true;
                    break :dispatch;
                }
                sets[s] = CharSet{ .low = low, .hi = hi };
                s += 1;
                patt[j] = this_op;
            },
            else => |ch| { // regular ol' character
                patt[j] = RegOp{ .char = ch };
            },
        }
    } // end :dispatch
    if (j == patt.len and i < in.len) {
        logError("Ran out of regex slots before reached end of pattern\n", .{});
        return null;
    }
    if (pump != 0) {
        logError("missing closing parenthesis\n", .{});
        return null;
    }
    if (bad_string) {
        const tail = switch (i) {
            0 => "st",
            1 => "nd",
            2 => "rd",
            else => "th",
        };
        logError("bad string at {d}{s} character\n", .{ i + 1, tail });
        return null;
    }
    return out;
}

fn logError(comptime fmt: []const u8, args: anytype) void {
    if (!builtin.is_test) {
        std.log.err(fmt, args);
    } else { // XXX don't ship this
        std.log.warn(fmt, args);
    }
}

//| TESTS

const expect = std.testing.expect;
const expectEqual = std.testing.expectEqual;

fn printPattern(patt: []const RegOp) void {
    _ = printPatternInternal(patt);
}

fn printRegex(regex: *const Regex) void {
    const patt = regex.patt;
    const set_max = printPatternInternal(&patt);
    if (set_max) |max| {
        for (0..max + 1) |i| {
            std.debug.print("set {d}: ", .{i});
            printCharSet(regex.sets[i]) catch unreachable;
        }
    }
}

fn printPatternInternal(patt: []const RegOp) ?u8 {
    var j: usize = 0;
    var set_max: ?u8 = null;
    std.debug.print("[", .{});
    while (j < patt.len and patt[j] != .unused) : (j += 1) {
        switch (patt[j]) {
            .char,
            => |op| {
                std.debug.print("{s} {u}", .{ @tagName(patt[j]), op });
            },
            .class,
            .not_class,
            => |op| {
                if (set_max) |max| {
                    set_max = @max(max, op);
                } else {
                    set_max = op;
                }
                std.debug.print("{s} {d}", .{ @tagName(patt[j]), op });
            },
            else => {
                std.debug.print("{s}", .{@tagName(patt[j])});
            },
        }
        if (j + 1 < patt.len and patt[j + 1] != .unused) {
            std.debug.print(", ", .{});
        }
    }
    std.debug.print("]\n", .{});
    return set_max;
}

fn printCharSet(set: CharSet) !void {
    const allocator = std.testing.allocator;
    var set_str = try std.ArrayList(u8).initCapacity(allocator, @popCount(set.low) + @popCount(set.hi) + 1);
    defer set_str.deinit();
    if (@popCount(set.low) != 0) {
        for (0..64) |i| {
            const c: u6 = @intCast(i);
            if ((set.low | (one << c)) == set.low) {
                try set_str.append(@as(u8, c));
            }
        }
    }
    if (@popCount(set.hi) != 0) {
        try set_str.append(' ');
        for (0..64) |i| {
            const c: u6 = @intCast(i);
            if ((set.hi | (one << c)) == set.hi) {
                const ch = @as(u8, c) | 0b0100_0000;
                try set_str.append(ch);
            }
        }
    }
    std.debug.print("{s}\n", .{set_str.items});
}

test "compile some things" {
    const a_star = compile("a*").?;
    _ = a_star; // autofix
    // printRegex(&a_star);
    const a_group = compile("(abc)*").?;
    _ = a_group; // autofix
    // printRegex(&a_group);
    const some_alts = compile("a|\\w+|2?").?;
    _ = some_alts; // autofix
    // printRegex(&some_alts);
    const some_ranges = compile("[a-z][^a-z][abc^-]").?;
    _ = some_ranges; // autofix
    // printRegex(&some_ranges);
}

fn testMatchAll(needle: []const u8, haystack: []const u8, print: bool) !void {
    const maybe_regex = compile(needle);
    if (maybe_regex) |regex| {
        if (print)
            printRegex(&regex);
        const maybe_match = regex.match(haystack);
        if (maybe_match) |m| {
            try expectEqual(0, m[0]);
            try expectEqual(haystack.len, m[1]);
        }
    } else {
        try std.testing.expect(false);
    }
}

fn testFail(needle: []const u8, haystack: []const u8) !void {
    const maybe_regex = compile(needle);
    if (maybe_regex) |regex| {
        try expectEqual(null, regex.match(haystack));
    } else {
        try std.testing.expect(false);
    }
}

test "match some things" {
    try testMatchAll("abc", "abc", false);
    try testMatchAll("[a-z]", "d", false);
    try testMatchAll("\\W\\w", "3a", false);
    try testMatchAll("a*", "aaaaa", true);
    try testMatchAll("\\w+", "abdcdFG", false);
    try testFail("a+", "b");
    try testMatchAll("a*b+", "aaaaabbbbbbbb", false);
    try testMatchAll("a?b*", "abbbbb", false);
    try testMatchAll("a?b*", "bbbbbb", false);
}

test "workshop" {
    //const lazy = compile("a*?b").?;
    // printRegex(&lazy);
    try testMatchAll("^\\w*?abc", "qqqqabc", true);
}
