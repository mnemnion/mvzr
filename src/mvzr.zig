//! mvzr: Minimum Viable Zig Regex
//!
//! A minimalistic, but y'know, viable, Zig regex library.
//!
//! Focused on basic support of runtime-provided regular expressions.
const std = @import("std");
const testing = std.testing;

const XXX = false;

// Zig is very particular about the types of shifts.
const one: u64 = 1;

/// Maximum regex operations.
pub const MAX_REGEX_OPS = 50;
/// Maximum character sets, ASCII only.
pub const MAX_CHAR_SETS = 10;

const RegexType = enum {
    unused,
    dot,
    begin,
    end,
    left,
    right,
    optional,
    star,
    plus,
    char,
    class,
    not_class,
    digit,
    not_digit,
    alpha,
    not_alpha,
    whitespace,
    not_whitespace,
    alt,
};

pub const RegOp = struct {
    kind: RegexType,
    what: union {
        cp: u21, // codepoint
        c_off: i32, // offset into character set array
    },
};

pub const CharSet = struct {
    low: u64 = 0,
    hi: u64 = 0,
};

const Regex = struct {
    patt: [MAX_REGEX_OPS]RegOp,
    sets: [MAX_CHAR_SETS]CharSet,

    /// Match a regex pattern in `haystack`, if found, this returns `.{start, end}`
    pub fn match(regex: *const Regex, haystack: []const u8) ?struct { usize, usize } {
        if (haystack.len == 0) return null;
        var matchlen: usize = 0;
        switch (regex.out[0].kind) {
            .begin => {
                const width = matchPattern(&regex, matchlen, haystack);
                if (width) |w| {
                    return .{ 0, w };
                } else return null;
            },
            else => {
                while (matchlen < haystack.len) : (matchlen += 1) {
                    const width = matchPattern(&regex, matchlen, haystack);
                    if (width) {
                        return .{ matchlen, matchlen + width };
                    } else {
                        matchlen += 1;
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

fn matchPattern(regex: *const Regex, i: usize, haystack: []const u8) ?usize {
    _ = i; // autofix
    _ = haystack; // autofix
    var j = 0;
    var match_len = 0;
    _ = match_len; // autofix
    while (j < regex.patt.len) : (j += 1) {
        const op = regex.patt[j];
        switch (op.kind) {
            .unused => break,
            .optional => {
                //
            },
            .star => {},
            .etcetc => {},
        }
    }
}

const ascii = std.ascii;

fn matchOne(ops: *const Regex, i: usize, c: u8) bool {
    switch (ops[i].kind) {
        .dot => return true, // we match newlines, deal with it
        .class => return matchClass(ops.sets[ops[i].c_off], c),
        .not_class => return !matchClass(ops.sets[ops[i].c_off], c),
        .digit => return ascii.isDigit(c),
        .not_digit => return !ascii.isDigit(c),
        .alpha => return ascii.isAlphabetic(c),
        .not_alpha => return !ascii.isAlphabetic(c),
        .whitespace => return ascii.isWhitespace(c),
        .not_whitespace => return !ascii.isWhitespace(c),
        .char => return (c == ops[i].what.cp),
        else => unreachable,
    }
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
                patt[j] = RegOp{ .kind = .end, .what = .{ .c_off = -1 } };
            },
            '$' => {
                patt[j] = RegOp{ .kind = .begin, .what = .{ .c_off = -1 } };
            },
            '.' => {
                patt[j] = RegOp{ .kind = .dot, .what = .{ .c_off = -1 } };
            },
            '*' => {
                patt[j] = RegOp{ .kind = .star, .what = .{ .c_off = -1 } };
            },
            '?' => {
                patt[j] = RegOp{ .kind = .optional, .what = .{ .c_off = -1 } };
            },
            '|' => {
                patt[j] = RegOp{ .kind = .alt, .what = .{ .c_off = -1 } };
            },
            '(' => {
                pump += 1;
                patt[j] = RegOp{ .kind = .left, .what = .{ .c_off = -1 } };
            },
            ')' => {
                if (pump == 0) {
                    bad_string = true;
                    break :dispatch;
                }
                pump -= 1;
                patt[j] = RegOp{ .kind = .right, .what = .{ .c_off = -1 } };
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
                            patt[j] = RegOp{ .kind = .digit, .what = .{ .c_off = -1 } };
                        },
                        'D' => {
                            patt[j] = RegOp{ .kind = .not_digit, .what = .{ .c_off = -1 } };
                        },
                        'w' => {
                            patt[j] = RegOp{ .kind = .alpha, .what = .{ .c_off = -1 } };
                        },
                        'W' => {
                            patt[j] = RegOp{ .kind = .not_alpha, .what = .{ .c_off = -1 } };
                        },
                        's' => {
                            patt[j] = RegOp{ .kind = .whitespace, .what = .{ .c_off = -1 } };
                        },
                        'S' => {
                            patt[j] = RegOp{ .kind = .not_whitespace, .what = .{ .c_off = -1 } };
                        },
                        else => |ch| {
                            // Others are accepted as escaped, we don't care
                            // if they're special, you're not special, you're
                            // not my dad, you get the regex you give
                            patt[j] = RegOp{ .kind = .char, .what = .{ .cp = ch } };
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
                        break :which RegOp{ .kind = .not_class, .what = .{ .c_off = s } };
                    } else break :which RegOp{ .kind = .class, .what = .{ .coff = s } };
                };

                while (in[i] != ']' and i < in.len) : (i += 1) {
                    if (s > set.len) {
                        std.debug.print("excessive number of character sets\n");
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
            else => { // regular ol' character
                patt[j] = RegOp{ .kind = .char, .what = .{ .cp = c } };
            },
        }
        if (pump != 0) {
            std.debug.print("missing closing parenthesis\n");
            return null;
        }
        if (bad_string) {
            const tail = switch (i) {
                0 => "st",
                1 => "nd",
                2 => "rd",
                else => "th",
            };
            std.debug.print("bad string at {d}{s} character\n", .{ i, tail });
            return null;
        }
        if (j == patt.len and i < in.len) {
            std.debug.print("Ran out of regex slots before reached end of pattern\n");
            return null;
        }
        return out;
    }
}
