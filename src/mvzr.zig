//! mvzr: Minimum Viable Zig Regex
//!
//! A minimalistic, but y'know, viable, Zig regex library.
//!
//! Focused on basic support of runtime-provided regular expressions.
const std = @import("std");
const builtin = @import("builtin");
const testing = std.testing;

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

const CharSets = []const CharSet;

const Regex = struct {
    patt: [MAX_REGEX_OPS]RegOp,
    sets: [MAX_CHAR_SETS]CharSet,

    /// Match a regex pattern in `haystack`, if found, this returns `.{start, end}`
    pub fn match(regex: *const Regex, haystack: []const u8) ?struct { usize, usize } {
        if (haystack.len == 0) return null;
        var matchlen: usize = 0;
        const set_ptr = &regex.sets;
        switch (regex.out[0].kind) {
            .begin => {
                const width = matchPattern(regex[0..], set_ptr, matchlen, haystack);
                if (width) |w| {
                    return .{ 0, w };
                } else return null;
            },
            else => {
                while (matchlen < haystack.len) : (matchlen += 1) {
                    const width = matchPattern(regex[1..], set_ptr, matchlen, haystack);
                    if (width) {
                        return .{ matchlen, matchlen + width };
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

fn matchPattern(regex: []const RegOp, set: *const CharSets, i: usize, haystack: []const u8) ?usize {
    var j = 0;
    _ = j; // autofix
    if (XXX) {
        _ = haystack[i];
        _ = set;
    }
    const alt_count = countAlt(regex);
    if (alt_count > 0) {
        switch (alt_count) {
            1 => return dispatchTwoAlts(regex, set, i, haystack),
            2 => return dispatchThreeAlts(regex, set, i, haystack),
            3 => return dispatchFourAlts(regex, set, i, haystack),
            else => @panic("NYI"),
        }
    }
    return matchPatternNoAlts(regex, set, i, haystack);
}

fn matchPatternNoAlts(regex: []const RegOp, set: *const CharSets, i: usize, haystack: []const u8) ?usize {
    _ = regex; // autofix
    _ = set; // autofix
    _ = i; // autofix
    _ = haystack; // autofix
}

const ascii = std.ascii;

fn matchOne(op: RegOp, sets: *const CharSets, c: u8) bool {
    switch (op) {
        .dot => return true, // we match newlines, deal with it
        .class => |c_off| return matchClass(sets[c_off], c),
        .not_class => |c_off| return !matchClass(sets[c_off], c),
        .digit => return ascii.isDigit(c),
        .not_digit => return !ascii.isDigit(c),
        .alpha => return ascii.isAlphabetic(c),
        .not_alpha => return !ascii.isAlphabetic(c),
        .whitespace => return ascii.isWhitespace(c),
        .not_whitespace => return !ascii.isWhitespace(c),
        .char => |ch| return (c == ch),
        else => unreachable,
    }
}

// TODO this backtracks, maybe rethink that (lockstep is annoying)
fn matchFourPatterns(
    first: []const RegOp,
    second: []const RegOp,
    third: []const RegOp,
    fourth: []const RegOp,
    sets: *const CharSets,
    i: usize,
    haystack: []const u8,
) ?usize {
    const m1m = matchPatternNoAlts(first, sets, i, haystack);
    if (m1m) |m1| {
        return m1;
    } else {
        return matchThreePatterns(second, third, fourth, sets, i, haystack);
    }
}

fn matchThreePatterns(
    first: []const RegOp,
    second: []const RegOp,
    third: []const RegOp,
    sets: *const CharSets,
    i: usize,
    haystack: []const u8,
) ?usize {
    const m1m = matchPatternNoAlts(first, sets, i, haystack);
    if (m1m) |m1| {
        return m1;
    } else {
        return matchTwoPatterns(second, third, sets, i, haystack);
    }
}

fn matchTwoPatterns(
    first: []const RegOp,
    second: []const RegOp,
    sets: *const CharSets,
    i: usize,
    haystack: []const u8,
) ?usize {
    const m1m = matchPatternNoAlts(first, sets, i, haystack);
    if (m1m) |m1| {
        return m1;
    } else {
        return matchPatternNoAlts(second, sets, i, haystack);
    }
}

fn sliceAlt(regex: []const RegOp) []const RegOp {
    const alt_at = findAlt(regex);
    if (alt_at) |at| {
        return regex[0..at];
    } else unreachable; // verified before dispatch
}

fn dispatchTwoAlts(regex: []const RegOp, set: *const CharSets, i: usize, haystack: []const u8) ?usize {
    const first = sliceAlt(regex);
    const second = regex[first.len + 1 ..];
    return matchTwoPatterns(first, second, set, i, haystack);
}

fn dispatchThreeAlts(regex: []const RegOp, set: *const CharSets, i: usize, haystack: []const u8) ?usize {
    const first = sliceAlt(regex);
    const second = sliceAlt(regex[first.len + 1 ..]);
    const third = regex[second.len + 1 ..];
    return matchThreePatterns(first, second, third, set, i, haystack);
}

fn dispatchFourAlts(regex: []const RegOp, set: *const CharSets, i: usize, haystack: []const u8) ?usize {
    const first = sliceAlt(regex);
    const second = sliceAlt(regex[first.len + 1 ..]);
    const third = sliceAlt(regex[second.len + 1 ..]);
    const fourth = regex[second.len + 1 ..];
    return matchFourPatterns(first, second, third, fourth, set, i, haystack);
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

// Count alts which aren't in a group
fn countAlt(patt: []RegOp) usize {
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

fn findAlt(patt: []const RegOp, j: usize) ?usize {
    while (j < patt.len) : (j += 1) {
        if (patt[j].kind == .alt) {
            return j;
        }
    }
    return null;
}

fn findRight(patt: *const []const RegOp, j: usize) usize {
    // Compiler made sure these are matched
    var pump = 0;
    while (j < patt.len) : (j += 1) {
        const kind = patt[j].kind;
        if (kind == .right and pump == 0)
            return j
        else
            continue;
        if (kind == .left) pump += 1;
    }
    unreachable;
}

/// Move modifiers to prefix position.
fn prefixModifier(patt: *[]RegOp, j: usize, op: RegOp) void {
    var find_j = j - 1;
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
        else => {
            find_j -= 1;
        },
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

fn beforePriorLeft(patt: *const []const RegOp, j: usize) usize {
    std.debug.assert(patt[j] == .left);
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
        RegOp{
            .kind = .unused,
            .what = .{ .cp = 0 },
        },
    );
    @memset(&out.sets, .{ .low = 0, .hi = 0 });
    var patt = &out.patt;
    var set = &out.set;
    var i: usize = 0;
    var j: usize = 0;
    var s: usize = 0;
    var pump = 0;
    dispatch: while (i < in.len and j + 1 < patt.len) : ({
        j += 1;
        i += 1;
    }) {
        const c = in[i];
        switch (c) {
            '^' => {
                patt[j] = RegOp{ .end = {} };
            },
            '$' => {
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
                            patt[j] = RegOp{ .not_whitespace = .{} };
                        },
                        else => |ch| {
                            // Others are accepted as escaped, we don't care
                            // if they're special, you're not special, you're
                            // not my dad, you get the regex you give
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
                        break :which RegOp{ .not_class = {} };
                    } else break :which RegOp{ .class = {} };
                };

                while (in[i] != ']' and i < in.len) : (i += 1) {
                    if (s > set.len) {
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
                                    break :which in[i + 2];
                                } else if (i + 3 < in.len) {
                                    i += 2; // likewise
                                    break :which in[i + 3];
                                } else {
                                    // what to do here? don't care, have a 0
                                    break :which 0; // that'll show ya
                                }
                            };
                            for (c1..c_end) |c_range| {
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
                        } else { // '-' in set, value is 45 so
                            const cut_hyphen: u6 = @truncate('-');
                            low |= 1 < cut_hyphen;
                        }
                    }
                } // end while
                if (i == in.len or in[i] != ']') {
                    bad_string = true;
                    break :dispatch;
                }
                set[s] = CharSet{ .low = low, .hi = hi };
                s += 1;
                patt[j] = this_op;
            },
            else => |ch| { // regular ol' character
                patt[j] = RegOp{ .char = ch };
            },
        }
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
}

fn logError(comptime fmt: []const u8, args: anytype) void {
    if (!builtin.is_test) {
        std.log.err(fmt, args);
    }
}
