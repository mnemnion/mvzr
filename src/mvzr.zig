//!
//! mvzr: Minimum Viable Zig Regex
//!
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

const RegexType = enum(u5) {
    unused,
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
    eager_optional,
    eager_star,
    eager_plus,
    some,
    up_to,
    dot,
    char,
    class,
    not_class,
    digit,
    not_digit,
    alpha,
    not_alpha,
    whitespace,
    not_whitespace, // #26, can add five before bumping to u6
};

pub const RegOp = union(RegexType) {
    unused: void,
    begin: void,
    end: void,
    left: void, // TODO add offset u8
    right: void, // TODO add head-match offset
    alt: void, // TODO add offset u8
    optional: void,
    star: void,
    plus: void,
    lazy_optional: void,
    lazy_star: void,
    lazy_plus: void,
    eager_optional: void,
    eager_star: void,
    eager_plus: void,
    some: u8,
    up_to: u8,
    dot: void,
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

const OpMatch = struct {
    j: []const RegOp,
    i: usize,
};

const CharSets = [MAX_CHAR_SETS]CharSet;

const Regex: type = SizedRegex(MAX_REGEX_OPS, MAX_CHAR_SETS);

pub fn SizedRegex(ops: comptime_int, char_sets: comptime_int) type {
    return struct {
        patt: [ops]RegOp = [1]RegOp{.{ .unused = {} }} ** ops,
        sets: [char_sets]CharSet = [1]CharSet{.{ .low = 0, .hi = 0 }} ** char_sets,

        const SizedRegexT = @This();

        pub fn compile(patt: []const u8) ?SizedRegex {
            return compile_regex(SizedRegexT, patt);
        }

        /// Match a regex pattern in `haystack`, if found, this returns a `Match`.
        pub fn match(regex: *const SizedRegexT, haystack: []const u8) ?Match {
            if (haystack.len == 0) return null;
            const maybe_matched = regex.matchInternal(haystack);
            if (maybe_matched) |m| {
                const m1 = m[0];
                const m2 = m[1];
                return Match{
                    .slice = haystack[m1..m2],
                    .start = m1,
                    .end = m2,
                };
            } else {
                return null;
            }
        }

        /// Boolean test if the regex matches in the haystack.
        pub fn isMatch(regex: *const SizedRegexT, haystack: []const u8) bool {
            const maybe_matched = regex.matchInternal(haystack);
            if (maybe_matched) |_| {
                return true;
            } else {
                return false;
            }
        }

        /// Return an iterator over all matches.  Call `next` until exhausted.
        /// A `Match` struct is returned for successful matches, consisting of
        /// the match in both slice and bookend form.
        pub fn iterator(regex: *const SizedRegexT, haystack: []const u8) RegexIterator {
            return RegexIterator{
                .regex = regex,
                .haystack = haystack,
            };
        }

        pub const RegexIterator = struct {
            regex: *const SizedRegexT,
            idx: usize = 0,
            haystack: []const u8,

            pub fn next(iter: *RegexIterator) ?Match {
                const maybe_match = iter.regex.match(iter.haystack[iter.idx..]);
                if (maybe_match) |m| {
                    const m_start = m.start + iter.idx;
                    const m_end = m.end + iter.idx;
                    iter.idx += m.end;
                    return Match{
                        .slice = m.slice,
                        .start = m_start,
                        .end = m_end,
                    };
                } else {
                    return null;
                }
            }
        };

        fn matchInternal(regex: *const SizedRegexT, haystack: []const u8) ?struct { usize, usize } {
            const end = regex.findPatternEnd();
            const patt = regex.patt[0..end];
            switch (patt[0]) {
                .begin => {
                    const matched = matchOuterPattern(patt[1..], &regex.sets, haystack);
                    if (matched) |m| {
                        return .{ 0, m.i };
                    } else return null;
                },
                else => {
                    var matchlen: usize = 0;
                    while (matchlen < haystack.len) : (matchlen += 1) {
                        const matched = matchOuterPattern(patt, &regex.sets, haystack[matchlen..]);
                        if (matched) |m| {
                            return .{ matchlen, matchlen + m.i };
                        }
                    }
                    return null;
                },
            }
        }

        fn findPatternEnd(regex: *const SizedRegexT) usize {
            const patt = regex.patt;
            for (0..patt.len) |i| {
                if (patt[i] == .unused) {
                    return i;
                }
            }
            return patt.len;
        }
    };
}

/// A single match as returned from `regex.match` and `regex.iterator`.
/// Note that the slice is borrowed from the haystack: to own it, call
/// `try match.toOwned(allocator)`.
pub const Match = struct {
    slice: []const u8,
    start: usize,
    end: usize,

    /// Return an copy of the Match with fresh memory allocator for the
    /// slice.
    pub fn toOwned(matched: Match, allocator: std.mem.Allocator) !Match {
        const new_slice = try allocator.dupe(u8, matched.slice);
        return Match{
            .slice = new_slice,
            .start = matched.start,
            .end = matched.end,
        };
    }

    /// For freeing copied Match structs created with `toOwned`.
    pub fn deinit(matched: Match, allocator: std.mem.Allocator) void {
        allocator.free(matched.slice);
    }

    pub fn format(
        matched: Match,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        try writer.print("[{d}..{d}]: \"{}\"", .{
            matched.start,
            matched.end,
            std.zig.fmtEscapes(matched.slice),
        });
    }
};

/// Match `pattern` in `haystack`.
pub fn match(haystack: []const u8, pattern: []const u8) ?Match {
    const maybe_regex = compile(pattern);
    if (maybe_regex) |regex| {
        return regex.match(haystack);
    } else {
        return null;
    }
}

fn matchOuterPattern(patt: []const RegOp, sets: *const CharSets, haystack: []const u8) ?OpMatch {
    if (findAlt(patt, 0)) |_| {
        // There's at least one alt.
        var remaining_patt = patt;
        while (true) {
            const this_match = matchAlt(remaining_patt, sets, haystack);
            if (this_match) |m1| {
                // Matched
                return OpMatch{ .i = m1.i, .j = patt[0..0] };
            } else {
                const maybe_next = maybeAlt(remaining_patt);
                if (maybe_next) |next_patt| {
                    remaining_patt = remaining_patt[next_patt.len + 1 ..];
                } else { // Ran out of pattern
                    return null;
                }
            }
        }
    } else { // No alt.
        return matchPattern(patt, sets, haystack);
    }
}

fn matchPattern(patt: []const RegOp, sets: *const CharSets, haystack: []const u8) ?OpMatch {
    var i: usize = 0;
    var this_patt = patt;
    dispatch: while (this_patt.len != 0) {
        if (i == haystack.len) {
            switch (this_patt[0]) {
                .optional,
                .star,
                .lazy_optional,
                .lazy_star,
                .eager_optional,
                .eager_star,
                .up_to,
                .end,
                => {
                    this_patt = nextPattern(this_patt);
                    continue :dispatch;
                },
                .left => {
                    if (groupAcceptsEmpty(this_patt)) {
                        this_patt = nextPattern(this_patt);
                        continue :dispatch;
                    } else {
                        return null;
                    }
                },
                else => return null,
            }
        }
        const maybe_match = switch (this_patt[0]) {
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
            => matchOne(this_patt, sets, haystack[i]),
            .optional => matchOptional(this_patt[1..], sets, haystack[i..]),
            .star => matchStar(this_patt[1..], sets, haystack[i..]),
            .plus => matchPlus(this_patt[1..], sets, haystack[i..]),
            .lazy_optional => matchLazyOptional(this_patt[1..], sets, haystack[i..]),
            .lazy_star => matchLazyStar(this_patt[1..], sets, haystack[i..]),
            .lazy_plus => matchLazyPlus(this_patt[1..], sets, haystack[i..]),
            .eager_optional => matchEagerOptional(this_patt[1..], sets, haystack[i..]),
            .eager_star => matchEagerStar(this_patt[1..], sets, haystack[i..]),
            .eager_plus => matchEagerPlus(this_patt[1..], sets, haystack[i..]),
            .some => matchSome(this_patt, sets, haystack[i..]),
            .up_to => matchUpTo(this_patt, sets, haystack[i..]),
            // Finding .end here always means we've failed.
            .end => return null,
            .left => matchGroup(this_patt, sets, haystack[i..]),
            else => unreachable, // probably
        };
        if (maybe_match) |m| {
            this_patt = m.j;
            i += m.i;
            assert(!(i > haystack.len));
        } else {
            return null;
        }
    }
    if (this_patt.len == 0) // We've matched
        return OpMatch{ .i = i, .j = this_patt }
    else
        return null;
}

// Todo might just be patt.len == 0 now?
inline fn atEnd(patt: []const RegOp, i: usize, haystack: []const u8) bool {
    return patt.len == 0 or (patt[0] == .end and i == haystack.len);
}

const ascii = std.ascii;

fn matchOne(patt: []const RegOp, sets: *const CharSets, sample: u8) ?OpMatch {
    if (matchOneByte(patt[0], sets, sample)) {
        return OpMatch{ .i = 1, .j = patt[1..] };
    } else {
        return null;
    }
}

fn matchOneByte(op: RegOp, sets: *const CharSets, c: u8) bool {
    return switch (op) {
        .dot => true, // we match newlines, deal with it
        .class => |c_off| matchClass(sets[c_off], c),
        .not_class => |c_off| !matchClass(sets[c_off], c),
        .digit => ascii.isDigit(c),
        .not_digit => !ascii.isDigit(c),
        .alpha => ascii.isAlphanumeric(c) or c == '_',
        .not_alpha => (!ascii.isAlphanumeric(c) and !(c == '_')),
        .whitespace => ascii.isWhitespace(c),
        .not_whitespace => !ascii.isWhitespace(c),
        .char => |ch| (c == ch),
        else => unreachable,
    };
}

fn matchStar(patt: []const RegOp, sets: *const CharSets, haystack: []const u8) OpMatch {
    // TODO This should split immediately based on if there even is a next pattern.
    var i: usize = 0;
    const this_patt = thisPattern(patt);
    while (matchPattern(this_patt, sets, haystack[i..])) |m| {
        i += m.i;
        assert(!(i > haystack.len));
        if (i == haystack.len or i == 0) break; // Might need to walk it back
    }

    const next_patt = nextPattern(patt);
    if (next_patt.len == 0) {
        return OpMatch{ .i = i, .j = next_patt };
    }
    if (i == haystack.len) {
        if (next_patt[0] == .end) {
            // That's fine then
            return OpMatch{ .i = i, .j = next_patt };
        } else {
            // We're not done, back off a bit
            i -= 1;
        }
    }

    const maybe_next = matchPattern(next_patt, sets, haystack[i..]);
    if (maybe_next) |m2| {
        return OpMatch{ .i = i + m2.i, .j = nextPattern(next_patt) };
    } // otherwise we gotta do the loopback dance.
    // TODO this logic is wrong because /our/ pattern might not match at every point in the string.
    // fix that later (I have something in mind here: a mask storing an intersection of every real
    // match for us, with a potential match for the next guy)
    i = if (i == 0) 0 else (i - 1);
    while (true) {
        const try_next = matchPattern(next_patt, sets, haystack[i..]);
        if (try_next) |m2| {
            return OpMatch{ .i = i + m2.i, .j = m2.j };
        }
        if (i == 0) break;
        i -= 1;
    } // This might be ok (alts)

    return OpMatch{ .i = 0, .j = nextPattern(next_patt) };
}
fn matchPlus(patt: []const RegOp, sets: *const CharSets, haystack: []const u8) ?OpMatch {
    const this_patt = thisPattern(patt);
    const first_m = matchPattern(this_patt, sets, haystack);
    if (first_m == null) return null;
    const m1 = first_m.?;
    // We can skip matchStar if we've at the end
    if (m1.i == haystack.len) return OpMatch{ .i = m1.i, .j = nextPattern(patt) };
    const m2 = matchStar(patt, sets, haystack[m1.i..]);
    // If matchStar produced 0, then its m2.j is invalid, we use ours
    if (m2.i == 0) {
        return OpMatch{ .i = m1.i, .j = nextPattern(patt) };
    } else {
        return OpMatch{ .i = m1.i + m2.i, .j = m2.j };
    }
}

fn matchOptional(patt: []const RegOp, sets: *const CharSets, haystack: []const u8) ?OpMatch {
    const this_patt = thisPattern(patt);
    const maybe_m = matchPattern(this_patt, sets, haystack);
    if (maybe_m) |m1| {
        // See if we can succeed here
        const next_patt = nextPattern(patt);
        var i = m1.i;
        if (next_patt.len != 0) {
            if (i == haystack.len) {
                // Reset.
                i = 0;
            }
            const maybe_next = matchPattern(next_patt, sets, haystack[i..]);
            if (maybe_next) |m2| {
                return OpMatch{ .i = i + m2.i, .j = m2.j };
            } else { // Drop our match.
                return matchPattern(next_patt, sets, haystack);
            }
        } else {
            return m1;
        }
    } else {
        return OpMatch{ .i = 0, .j = nextPattern(patt) };
    }
}

fn matchEagerOptional(patt: []const RegOp, sets: *const CharSets, haystack: []const u8) OpMatch {
    const this_patt = thisPattern(patt);
    const maybe_m = matchPattern(this_patt, sets, haystack);
    if (maybe_m) |m| {
        return OpMatch{ .i = m.i, .j = nextPattern(patt) };
    } else {
        return OpMatch{ .i = 0, .j = nextPattern(patt) };
    }
}

fn matchLazyStar(patt: []const RegOp, sets: *const CharSets, haystack: []const u8) OpMatch {
    const this_patt = thisPattern(patt);
    const next_patt = nextPattern(patt);
    // Other guy gets the first shot
    const match_first = matchPattern(next_patt, sets, haystack);
    if (match_first) |m| {
        return m;
    }
    var i: usize = 0;
    while (true) {
        if (i == haystack.len)
            return OpMatch{ .i = i, .j = next_patt };
        // Always try next first
        const match_theirs = matchPattern(next_patt, sets, haystack[i..]);
        if (match_theirs) |m1| {
            // All done
            return OpMatch{ .i = i + m1.i, .j = m1.j };
        } else {
            const match_ours = matchPattern(this_patt, sets, haystack[i..]);
            if (match_ours) |m2| {
                // Keep it up
                i += m2.i;
            } else {
                // other guy's turn coming up
                return OpMatch{ .i = i, .j = next_patt };
            }
        }
    }
}

fn matchLazyPlus(patt: []const RegOp, sets: *const CharSets, haystack: []const u8) ?OpMatch {
    const this_patt = thisPattern(patt);
    const first_m = matchPattern(this_patt, sets, haystack);
    if (first_m == null) return null;
    const m1 = first_m.?;
    if (m1.i == haystack.len) return OpMatch{ .i = m1.i, .j = nextPattern(patt) };
    const m2 = matchLazyStar(patt, sets, haystack[m1.i..]);
    return OpMatch{ .i = m1.i + m2.i, .j = m2.j };
}

fn matchLazyOptional(patt: []const RegOp, sets: *const CharSets, haystack: []const u8) ?OpMatch {
    // try the rest first
    const maybe_match = matchPattern(nextPattern(patt), sets, haystack);
    if (maybe_match) |m| {
        return m;
    }
    return matchOptional(patt, sets, haystack);
}

fn matchEagerPlus(patt: []const RegOp, sets: *const CharSets, haystack: []const u8) ?OpMatch {
    const this_patt = thisPattern(patt);
    const first_m = matchPattern(this_patt, sets, haystack);
    if (first_m == null) return null;
    const m1 = first_m.?;
    if (m1.i == haystack.len) return OpMatch{ .i = m1.i, .j = nextPattern(patt) };
    const m2 = matchEagerStar(patt, sets, haystack[m1.i..]);
    return OpMatch{ .i = m1.i + m2.i, .j = m2.j };
}

fn matchEagerStar(patt: []const RegOp, sets: *const CharSets, haystack: []const u8) OpMatch {
    const this_patt = thisPattern(patt);
    var i: usize = 0;
    while (matchPattern(this_patt, sets, haystack[i..])) |m| {
        i += m.i;
        assert(!(i > haystack.len));
        if (i == haystack.len) break;
    }
    return OpMatch{ .i = i, .j = nextPattern(patt) };
}

fn matchSome(patt: []const RegOp, sets: *const CharSets, haystack: []const u8) ?OpMatch {
    var count = patt[0].some;
    var i: usize = 0;
    const this_patt = if (patt[1] == .up_to or patt[1] == .star)
        thisPattern(patt[2..])
    else
        thisPattern(patt[1..]);

    while (count > 0) : (count -= 1) {
        const matched = matchPattern(this_patt, sets, haystack);
        if (matched) |m| {
            i += m.i;
        } else {
            return null;
        }
    } // We may need to slice manually
    if (patt[1] == .up_to or patt[1] == .star) {
        return OpMatch{ .i = i, .j = patt[1..] };
    } else {
        return OpMatch{ .i = i, .j = nextPattern(patt) };
    }
}

fn matchUpTo(patt: []const RegOp, sets: *const CharSets, haystack: []const u8) OpMatch {
    const more_match = matchUpToInner(patt[1..], sets, haystack, patt[0].up_to);
    if (more_match) |m|
        return m
    else // This one we don't need to slice
        return OpMatch{ .i = 0, .j = nextPattern(patt) };
}

fn matchUpToInner(patt: []const RegOp, sets: *const CharSets, haystack: []const u8, count: usize) ?OpMatch {
    if (count == 1) { // Last try is an optional
        const opt = matchOptional(patt, sets, haystack);
        return opt; // suss
    }
    const this_patt = thisPattern(patt);

    const maybe_m = matchPattern(this_patt, sets, haystack);
    if (maybe_m) |m1| {
        // See if we can succeed here
        const next_patt = nextPattern(patt);
        var i = m1.i;
        var maybe_next: ?OpMatch = null;
        if (next_patt.len != 0) {
            if (i == haystack.len) {
                // Reset (we still have m1.i)
                i = 0;
            }
            maybe_next = matchPattern(next_patt, sets, haystack[i..]);
            if (maybe_next) |m2| {
                if (m2.i + i == haystack.len) {
                    // As far as it goes
                    return OpMatch{ .i = i + m2.i, .j = m2.j };
                }
            }
        }
        const maybe_rest = matchUpToInner(patt, sets, haystack[m1.i..], count - 1);
        if (maybe_rest) |m3| {
            return OpMatch{ .i = m1.i + m3.i, .j = m3.j };
        }
        // Otherwise we backtrack from down stack.
        if (maybe_next) |m2| {
            return OpMatch{ .i = i + m2.i, .j = m2.j };
        } else {
            return null;
        }
    } else {
        return null;
    }
}

fn matchGroup(patt: []const RegOp, sets: *const CharSets, haystack: []const u8) ?OpMatch {
    // Cut out the group pattern:
    const inner_patt = sliceGroup(patt);
    // Empty group is a match:
    if (inner_patt.len == 0) {
        return OpMatch{ .i = 0, .j = nextPattern(patt) };
    }
    // And the next pattern (can be empty)
    const next_patt = nextPattern(patt);
    // Are there alts?
    if (!hasAlt(patt)) {
        const maybe_match = matchPattern(inner_patt, sets, haystack);
        if (maybe_match) |m| {
            // Success, return the next pattern.
            return OpMatch{ .i = m.i, .j = next_patt };
        } else { // Fail,  with nothing left to try.
            return null;
        }
    } // There's at least one alt.
    // Is there another pattern to check?
    if (next_patt.len == 0) {
        const our_match = matchAlt(inner_patt, sets, haystack);
        if (our_match) |m| {
            // Strip the remaining matches, may as well use empty next_patt
            return OpMatch{ .i = m.i, .j = next_patt };
        }
    } // Try anything potentially unmatched, using the next pattern to test success
    var remaining_patt = inner_patt;
    while (remaining_patt.len != 0) {
        const this_match = matchAlt(remaining_patt, sets, haystack);
        if (this_match) |m1| {
            // Make sure the next pattern passes:
            const next_match = matchPattern(next_patt, sets, haystack[m1.i..]);
            if (next_match) |m2| {
                // Whole group matches, and we can use m2.j
                return OpMatch{ .i = m1.i + m2.i, .j = m2.j };
            } else { // Try our next pattern, if any.
                remaining_patt = m1.j;
            }
        } else { // Nothing in our group passed
            return null;
        }
    } // Ran out of pattern
    return null;
}

fn matchAlt(patt: []const RegOp, sets: *const CharSets, haystack: []const u8) ?OpMatch {
    const maybe_first = maybeAlt(patt);
    if (maybe_first) |first_patt| {
        const one_m = matchPattern(first_patt, sets, haystack);
        if (one_m) |m1| {
            // groups return what they don't eat
            return OpMatch{ .i = m1.i, .j = patt[first_patt.len + 1 ..] };
        } else {
            // try the next one
            return matchAlt(patt[first_patt.len + 1 ..], sets, haystack);
        }
    } else {
        // Ordinary pattern (last match)
        return matchPattern(patt, sets, haystack);
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

fn nextPatternForSome(patt: []const RegOp) usize {
    switch (patt[0]) {
        .left => return findRight(patt, 0) + 1,
        .star,
        .optional,
        .plus,
        .lazy_star,
        .lazy_optional,
        .lazy_plus,
        .eager_star,
        .eager_plus,
        .eager_optional,
        .up_to,
        => return 1 + nextPatternForSome(patt[1..]),
        .some => {
            if (patt[1] == .star or patt[1] == .up_to) {
                return 2 + nextPatternForSome(patt[2..]);
            } else {
                return 1 + nextPatternForSome(patt[1..]);
            }
        },
        .unused => return 0, // or unreachable idk
        else => return 1,
    }
}

fn nextPattern(patt: []const RegOp) []const RegOp {
    switch (patt[0]) {
        .unused, .begin => @panic("Internal error, .unused or .begin encountered"),
        .right => @panic("Internal error, encountered .right"),
        .left => return patternAfterGroup(patt),
        .alt,
        .optional,
        .star,
        .plus,
        .lazy_optional,
        .lazy_star,
        .lazy_plus,
        .eager_optional,
        .eager_star,
        .eager_plus,
        .some,
        .up_to,
        => return nextPattern(patt[1..]),
        .end,
        .dot,
        .char,
        .class,
        .not_class,
        .digit,
        .not_digit,
        .alpha,
        .not_alpha,
        .whitespace,
        .not_whitespace,
        => return patt[1..],
    }
}

fn patternAfterGroup(patt: []const RegOp) []const RegOp {
    assert(patt[0] == .left);
    var j: usize = 1;
    var pump: usize = 0;
    while (true) : (j += 1) {
        switch (patt[j]) {
            .right => {
                if (pump == 0) {
                    return patt[j + 1 ..];
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

// Returns just our pattern.
fn thisPattern(patt: []const RegOp) []const RegOp {
    switch (patt[0]) {
        .left => return sliceGroup(patt),
        // This function is not called from modifier position by construction,
        // Except for .optional followed by .some, .sometimes, which needs
        // this:
        .some => return patt[0..nextPatternForSome(patt)],
        else => return patt[0..1],
    }
}

/// Returns a whole group, including the ends.
fn thisGroup(patt: []const RegOp) []const RegOp {
    assert(patt[0] == .left);
    var j: usize = 1;
    var pump: usize = 0;
    while (true) : (j += 1) {
        switch (patt[j]) {
            .right => {
                if (pump == 0) {
                    return patt[0 .. j + 1];
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

fn patternAcceptsEmpty(patt: []const RegOp) bool {
    switch (patt[0]) {
        .optional,
        .star,
        .lazy_optional,
        .lazy_star,
        .eager_optional,
        .eager_star,
        .up_to,
        .end,
        => return true,
        .left => {
            if (groupAcceptsEmpty(patt)) {
                return true;
            } else {
                return false;
            }
        },
        else => return false,
    }
}

/// Does the group accept empty matches?
fn groupAcceptsEmpty(patt: []const RegOp) bool {
    if (!hasAlt(patt)) {
        const inner = sliceGroup(patt);
        if (inner.len == 0) {
            return true;
        } else return patternAcceptsEmpty(inner);
    }
    var inner_patt = sliceGroup(patt);
    while (true) {
        const maybe_this_patt = maybeAlt(inner_patt);
        if (maybe_this_patt) |this_patt| {
            if (this_patt.len == 0) return true; // Empty alt matches empty string.
            if (patternAcceptsEmpty(this_patt)) {
                return true;
            } else {
                inner_patt = inner_patt[this_patt.len + 1 ..];
                if (inner_patt.len == 0) return true; // Final empty pattern;
            }
        } else { // final patt
            return (patternAcceptsEmpty(inner_patt));
        }
    }
    unreachable;
}

fn findPatternEnd(regex: *const Regex) usize {
    const patt = regex.patt;
    for (0..patt.len) |i| {
        if (patt[i] == .unused) {
            return i;
        }
    }
    return patt.len;
}

/// Return the first alt, if there is one.
fn maybeAlt(patt: []const RegOp) ?[]const RegOp {
    const alt_at = findAlt(patt, 0);
    if (alt_at) |at| {
        return patt[0..at];
    } else return null;
}

/// Test if a group (must be a group!) has an alt.
fn hasAlt(patt: []const RegOp) bool {
    assert(patt[0] == .left); // We'll use this for a speedup later
    var pump: usize = 0;
    var j: usize = 1;
    while (j < patt.len - 1) : (j += 1) {
        switch (patt[j]) {
            .left => pump += 1,
            .right => {
                if (pump == 0)
                    return true
                else
                    pump -= 1;
            },
            .alt => {
                if (pump == 0) return true;
            },
            else => {},
        }
    }
    return false;
}

/// Returns the *interior* of a group.
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

fn pattEnd(patt: []const RegOp) usize {
    var j: usize = 0;
    while (j < patt.len and patt[j] != .unused) : (j += 1) {}
    return j;
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

/// Answer if there's an alt in the (outermost) pattern.
fn findAlt(patt: []const RegOp, j_in: usize) ?usize {
    var j = j_in;
    var pump: usize = 0;
    while (j < patt.len) : (j += 1) {
        switch (patt[j]) {
            .left => pump += 1,
            .right => pump -= 1,
            .alt => {
                if (pump == 0) return j;
            },
            else => {},
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
fn prefixModifier(patt: []RegOp, j: usize, op: RegOp) bool {
    if (j == 0 or patt[j] == .begin) return false;
    var find_j = j - 1;
    // travel back before last group
    switch (patt[find_j]) {
        .right => {
            find_j = beforePriorLeft(patt, find_j);
        },
        else => {},
    }
    // If we already have a modifier, two are not kosher:
    if (find_j > 0) {
        switch (patt[find_j - 1]) {
            .alt,
            .plus,
            .lazy_optional,
            .lazy_star,
            .lazy_plus,
            .eager_optional,
            .eager_star,
            .eager_plus,
            .optional,
            => {
                logError("found a modifier on a modifier", .{});
                return false;
            }, // Allowed for {M,} and {M,N} purposes
            .some => {
                if (op != .optional) {
                    logError("found a modifier on a modifier", .{});
                    return false;
                } else {
                    find_j -= 1;
                }
            },
            .star => {
                if (op != .some) {
                    logError("found a modifier on a modifier", .{});
                    return false;
                }
            },
            .up_to => {
                if (op == .optional) {
                    find_j -= 1;
                    if (find_j > 0 and patt[find_j - 1] == .some) {
                        find_j -= 1;
                    }
                } else if (op != .some) {
                    logError("found a modifier on a modifier", .{});
                    return false;
                }
            },
            else => {},
        } // find_j is at our insert offset
    }
    var move_op = patt[find_j];
    if (op == .some and find_j > 0) {
        const prev_op = patt[find_j - 1];
        if (prev_op == .up_to or prev_op == .star or prev_op == .optional) {
            find_j -= 1;
            move_op = prev_op;
        }
    }
    patt[find_j] = op;
    find_j += 1;
    while (move_op != .unused) : (find_j += 1) {
        const temp_op = patt[find_j];
        patt[find_j] = move_op;
        move_op = temp_op;
    }
    return true;
}

fn beforePriorLeft(patt: []RegOp, j: usize) usize {
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

inline fn countDigits(in: []const u8) usize {
    var i: usize = 0;
    while (i < in.len and ascii.isDigit(in[i])) : (i += 1) {}
    return i;
}

fn parseByte(in: []const u8) !struct { usize, u8 } {
    const d1 = countDigits(in);
    if (d1 == 0 or d1 >= 4) return error.BadString;
    const c1 = std.fmt.parseInt(u16, in[0..d1], 10) catch {
        return error.BadString;
    };
    if (c1 > 255) {
        logError("repetition counts must fit in a u8, got {d}\n", .{c1});
        return error.BadString;
    }
    return .{ d1, @intCast(c1) };
}

fn parseHex(in: []const u8) !u8 {
    var out_buf: [1]u8 = undefined;
    const b = try std.fmt.hexToBytes(&out_buf, in[0..2]);
    return b[0];
}

pub fn compile(in: []const u8) ?Regex {
    return compile_regex(Regex, in);
}

/// (Attempts to) compile a Regex with a decidedly large default of 4K
/// operations and 1K character sets, returning `.{ops, sets}`, representing
/// the minimum necessary `SizedRegex(ops, sets)` for this pattern.  The
/// string provided must be comptime-known.  Since the values returned are
/// themselves only usable at comptime, it is suggested this function only
/// be used to tune the size of a regex during development, rather than
/// called pointlessly every time the program is compiled.
pub fn resourcesNeeded(comptime in: []const u8) struct { usize, usize } {
    const maybe_out = compile_regex(SizedRegex(4096, 1024), in);
    var max_s: usize = 0;
    if (maybe_out) |out| {
        for (&out.patt, 0..) |op, i| {
            switch (op) {
                .class, .not_class => |s_off| {
                    max_s = @max(max_s, s_off);
                },
                .unused => {
                    return .{ i, max_s };
                },
                else => {},
            }
        }
    } else {
        return .{ 0, 0 };
    }
    return .{ 0, 0 };
}

// TODO this should throw errors
/// Compile a regex.
fn compile_regex(RegexT: type, in: []const u8) ?RegexT {
    var out = RegexT{};
    var patt = out.patt[0..];
    var sets = out.sets[0..];
    var bad_string: bool = false;
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
            '^' => {
                if (i != 0) {
                    bad_string = true;
                    break :dispatch;
                }
                patt[j] = RegOp{ .begin = {} };
            },
            '$' => {
                if (i + 1 < in.len) {
                    bad_string = true;
                    break :dispatch;
                }
                patt[j] = RegOp{ .end = {} };
            },
            '.' => {
                patt[j] = RegOp{ .dot = {} };
            },
            '*' => {
                if (i + 1 < in.len and in[i + 1] == '?') {
                    i += 1;
                    const ok = prefixModifier(patt, j, RegOp{ .lazy_star = {} });
                    if (!ok) {
                        bad_string = true;
                        break :dispatch;
                    }
                } else if (i + 1 < in.len and in[i + 1] == '+') {
                    i += 1;
                    const ok = prefixModifier(patt, j, RegOp{ .eager_star = {} });
                    if (!ok) {
                        bad_string = true;
                        break :dispatch;
                    }
                } else {
                    const ok = prefixModifier(patt, j, RegOp{ .star = {} });
                    if (!ok) {
                        bad_string = true;
                        break :dispatch;
                    }
                }
            },
            '?' => {
                if (i + 1 < in.len and in[i + 1] == '?') {
                    i += 1;
                    const ok = prefixModifier(patt, j, RegOp{ .lazy_optional = {} });
                    if (!ok) {
                        bad_string = true;
                        break :dispatch;
                    }
                } else if (i + 1 < in.len and in[i + 1] == '+') {
                    i += 1;
                    const ok = prefixModifier(patt, j, RegOp{ .eager_optional = {} });
                    if (!ok) {
                        bad_string = true;
                        break :dispatch;
                    }
                } else {
                    const ok = prefixModifier(patt, j, RegOp{ .optional = {} });
                    if (!ok) {
                        bad_string = true;
                        break :dispatch;
                    }
                }
            },
            '+' => {
                if (i + 1 < in.len and in[i + 1] == '?') {
                    i += 1;
                    const ok = prefixModifier(patt, j, RegOp{ .lazy_plus = {} });
                    if (!ok) {
                        bad_string = true;
                        break :dispatch;
                    }
                } else if (i + 1 < in.len and in[i + 1] == '+') {
                    //
                    i += 1;
                    const ok = prefixModifier(patt, j, RegOp{ .eager_plus = {} });
                    if (!ok) {
                        bad_string = true;
                        break :dispatch;
                    }
                } else {
                    const ok = prefixModifier(patt, j, RegOp{ .plus = {} });
                    if (!ok) {
                        bad_string = true;
                        break :dispatch;
                    }
                }
            },
            '{' => { // {M,N} etc, or just character literal is fine
                i += 1;
                if (in[i] == ',') { // {,?
                    i += 1;
                    const d, const c1 = parseByte(in[i..]) catch {
                        bad_string = true;
                        break :dispatch;
                    };
                    i += d;
                    if (in[i] == '}') { // {,N}
                        const ok = prefixModifier(patt, j, RegOp{ .up_to = c1 });
                        if (!ok) {
                            bad_string = true;
                            break :dispatch;
                        } else continue :dispatch;
                    } else {
                        bad_string = true;
                        break :dispatch;
                    }
                }
                const d1, const c1 = parseByte(in[i..]) catch {
                    // This is fine, literal `}`
                    patt[j] = RegOp{ .char = '}' };
                    continue :dispatch;
                };
                i += d1;
                if (in[i] == ',') { // {M,?
                    i += 1;
                    if (in[i] == '}') { // {M,}
                        var ok = prefixModifier(patt, j, RegOp{ .star = {} });
                        if (!ok) {
                            bad_string = true;
                            break :dispatch;
                        }
                        j += 1;
                        ok = prefixModifier(patt, j, RegOp{ .some = c1 });
                        if (!ok) {
                            bad_string = true;
                            break :dispatch;
                        }
                        continue :dispatch;
                    } // {M,N}
                    const d2, const c2 = parseByte(in[i..]) catch {
                        bad_string = true;
                        break :dispatch;
                    };
                    i += d2;
                    if (in[i] != '}') {
                        bad_string = true;
                        break :dispatch;
                    }
                    if (c1 > c2) {
                        logError("{d} > {d}\n", .{ c1, c2 });
                        bad_string = true;
                        break :dispatch;
                    }
                    const c_rest = c2 - c1;
                    var ok = prefixModifier(patt, j, RegOp{ .up_to = c_rest });
                    if (!ok) {
                        bad_string = true;
                        break :dispatch;
                    }
                    j += 1;
                    ok = prefixModifier(patt, j, RegOp{ .some = c1 });
                    if (!ok) {
                        bad_string = true;
                        break :dispatch;
                    }
                } else if (in[i] == '}') {
                    // {M}
                    const ok = prefixModifier(patt, j, RegOp{ .some = c1 });
                    if (!ok) {
                        bad_string = true;
                        break :dispatch;
                    }
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
                        // character escapes
                        'r' => {
                            patt[j] = RegOp{ .char = 0x0d };
                        },
                        'n' => {
                            patt[j] = RegOp{ .char = 0x0a };
                        },
                        't' => {
                            patt[j] = RegOp{ .char = 0x09 };
                        },
                        // byte literal
                        'x' => {
                            i += 1;
                            const b = parseHex(in[i..]) catch {
                                bad_string = true;
                                break :dispatch;
                            };
                            i += 1;
                            patt[j] = RegOp{ .char = b };
                        },
                        else => |ch| {
                            // Others are accepted as escaped, we don't care
                            // if they're special, you're not special, I'm no
                            // your dad, you get the regex you give
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
                    if (s >= sets.len) {
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
                                if (i + 1 < in.len) {
                                    i += 1;
                                    const c2 = in[i];
                                    switch (c2) {
                                        0...63 => {
                                            const cut_c: u6 = @truncate(c2);
                                            low |= one << cut_c;
                                        },
                                        64...109,
                                        111...113,
                                        115,
                                        117,
                                        118,
                                        119,
                                        121...126,
                                        => {
                                            const cut_c: u6 = @truncate(c2);
                                            hi |= one << cut_c;
                                        },
                                        'n' => {
                                            low |= one << 0x0a; // newline
                                        },
                                        't' => {
                                            low |= one << 0x09; // tab
                                        },
                                        'r' => {
                                            low |= one << 0x0d; // carriage return
                                        },
                                        'x' => {
                                            i += 1;
                                            const b = parseHex(in[i..]) catch {
                                                bad_string = true;
                                                break :dispatch;
                                            };
                                            if (b > 127) {
                                                logError("charsets can't fit {d}\n", .{b});
                                                bad_string = true;
                                                break :dispatch;
                                            }
                                            i += 1;
                                            const b_trunc: u6 = @truncate(b);
                                            switch (b) {
                                                0...63 => low |= one << b_trunc,
                                                64...127 => hi |= one << b_trunc,
                                                else => unreachable,
                                            }
                                        },
                                        else => {
                                            bad_string = true;
                                            break;
                                        },
                                    }
                                }
                            },
                            else => {
                                logError("Apologies, but multi=byte characters in sets are not supported.\n", .{});
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
    } else {
        std.log.warn(fmt, args);
    }
}
//| TESTS

const testing = std.testing;
const expect = std.testing.expect;
const expectEqual = std.testing.expectEqual;
const expectEqualStrings = std.testing.expectEqualStrings;

fn printPattern(patt: []const RegOp) void {
    _ = printPatternInternal(patt);
}

fn printRegex(regex: anytype) void {
    const patt = regex.patt;
    const set_max = printPatternInternal(&patt);
    if (set_max) |max| {
        for (0..max + 1) |i| {
            std.debug.print("set {d}: ", .{i});
            printCharSet(regex.sets[i]) catch unreachable;
        }
    }
}

fn printRegexString(in: []const u8) void {
    const reggie = compile(in);
    if (reggie) |RRRRRR| {
        printRegex(&RRRRRR);
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
            .some,
            .up_to,
            => |op| {
                std.debug.print("{s} {d}", .{ @tagName(patt[j]), op });
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

fn testMatchAll(needle: []const u8, haystack: []const u8) !void {
    const maybe_regex = compile(needle);
    if (maybe_regex) |regex| {
        const maybe_match = regex.match(haystack);
        if (maybe_match) |m| {
            try expectEqual(0, m.start);
            try expectEqual(haystack.len, m.end);
        } else {
            try expect(false);
        }
    } else {
        try std.testing.expect(false);
    }
}

fn testMatchEnd(needle: []const u8, haystack: []const u8) !void {
    const maybe_regex = compile(needle);
    if (maybe_regex) |regex| {
        const maybe_match = regex.match(haystack);
        if (maybe_match) |m| {
            try expectEqual(haystack.len, m.end);
        } else {
            try expect(false);
        }
    } else {
        try std.testing.expect(false);
    }
}

fn testMatchAllP(needle: []const u8, haystack: []const u8) !void {
    const maybe_regex = compile(needle);
    if (maybe_regex) |regex| {
        printRegex(&regex);
    }
    try testMatchAll(needle, haystack);
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
    try testMatchAll("abc", "abc");
    try testMatchAll("[a-z]", "d");
    try testMatchAll("\\W\\w", "!a");
    try testMatchAll("\\w+", "abdcdFG");
    try testMatchAll("a*b+", "aaaaabbbbbbbb");
    try testMatchAll("a?b*", "abbbbb");
    try testMatchAll("a?b*", "bbbbbb");
    try testMatchAll("a*", "aaaaa");
    try testFail("a+", "b");
    try testMatchAll("a?", "a");
    try testMatchAll("^\\w*?abc", "qqqqabc");
    // Fail if pattern isn't complete
    try testFail("^\\w*?abcd", "qqqqabc");
    try testMatchAll("^a*?abc", "abc");
    try testMatchAll("^1??abc", "abc");
    try testMatchAll("^1??abc", "1abc");
    try testMatchAll("^1??1abc", "1abc");
    try testMatchAll("[^abc]+", "defgh");
    try testMatchAll("^1??1abc$", "1abc");
    try testFail("^1??1abc$", "1abccc");
    try testMatchAll("foo|bar|baz", "foo");
    try testMatchAll("foo|bar|baz", "bar");
    try testMatchAll("foo|bar|baz", "baz");
    try testMatchAll("foo|bar|baz|quux+", "quuxxxxx");
    try testMatchAll("foo|bar|baz|bux|quux|quuux|quuuux", "quuuux");
    try testMatchAll("foo|bar|(baz|bux|quux|quuux)|quuuux", "quuuux");
    try testMatchAll("(abc)+d", "abcabcabcd");
    try testMatchAll("\t\n\r\xff\xff", "\t\n\r\xff\xff");
    try testMatchAll("[\t\r\n]+", "\t\t\r\r\n\t\n\r");
    try testMatchAll("[fd\\x03\\x04]+", "f\x03d\x04dfd\x03");
    try testMatchAll("a+b", "ab");
    try testMatchAll("a*aaa", "aaaaaaaaaaaaaa");
    try testMatchAll("\\w+foo", "abcdefoo");
    try testFail("\\w+foo", "foo");
    try testMatchAll("\\w*foo", "foo");
    try testFail("a++a", "aaaaaaaa");
    try testFail("a*+a", "aaaaaaaa");
    try testMatchAll("(aaa)?aaa", "aaa");
    try testFail("(aaa)?+aaa", "aaa");
    try testMatchAll("ab?", "ab");
    try testMatchAll("ab?", "a");
    try testMatchAll("^a{3,6}a", "aaaaaa");
    try testMatchAll("^a{3,4}", "aaaa");
    try testMatchAll("^a{3,5}", "aaaaa");
    try testMatchAll("^a{3,5}", "aaa");
    try testMatchAll("\\w{3,5}bc", "abbbc");
    try testMatchAll("\\w{3,5}", "abb");
    try testMatchAll("!{,3}", "!!!");
    try testMatchAll("abc(def(ghi)jkl)mno", "abcdefghijklmno");
    try testMatchAll("abc(def(ghi?)jkl)mno", "abcdefghijklmno");
    try testMatchAll("abc(def(ghi)?jkl)mno", "abcdefjklmno");
    try testMatchAll("abc(def(ghi?)jkl)mno", "abcdefghjklmno");
    try testFail("abc(def(ghi?)jkl)mno", "abcdefjklmno");
    try testMatchAll("abc(def((ghi)?)jkl)mno", "abcdefjklmno");
    try testMatchAll("(abc){5}?", "abcabcabcabcabc");
    try testMatchAll("(abc){3,5}?", "abcabcabcabcabc");
    try testMatchAll("^\\w+?$", "glebarg");
    try testMatchAll("[A-Za-z]+$", "Pabcex");
    try testMatchAll("^[^\n]+$", "a single line");
    try testFail("^[^\n]+$", "several \n lines");
    // Empty stuff
    try testFail("[]+", "abc");
    try testMatchAll("abc()d", "abcd");
    try testMatchAll("abc(|||)d", "abcd");
    // No infinite loops
    try testMatchAll("(a*?)*aa", "aaa");
    try testMatchAll("(){0,1}q$", "q");
    try testMatchAll("(){1,2}q$", "q");
    try testMatchAll("()+q$", "q");
    try testMatchAll("^(q*)*$", "qqqq");
    try testMatchEnd("[bc]*(cd)+", "cbcdcd");
    // MD5 hash?
    try testMatchAll("^[a-f0-9]{32}", "0800fc577294c34e0b28ad2839435945");
    try testMatchAll("ab+c|de+f", "abbbc");
    try testMatchAll("ab+c|de+f", "deeeef");
    try testFail("^ab+c|de+f", "abdef");
    try testMatchAll("employ(er|ee|ment|ing|able)", "employee");
    try testMatchAll("employ(er|ee|ment|ing|able)", "employer");
    try testMatchAll("employ(er|ee|ment|ing|able)", "employment");
    try testMatchAll("employ(er|ee|ment|ing|able)", "employable");
    try testMatchAll("employ(er|ee|ment|ing|able)", "employing");
    try testMatchAll("employ(|er|ee|ment|ing|able)", "employ");
    try testMatchAll("employ(|er|ee|ment|ing|able)$", "employee");
    // non-catastropic backtracking #1
    try testFail("(a+a+)+b", "a" ** 2048);
    // non-catastropic backtracking #2
    try testFail("(a+?a+?)+?b", "a" ** 2048);
    // non-catastropic backtracking #3
    try testFail("^(.*?,){254}P", "12345," ** 255);
}

test "workshop" {
    //  ^(.*?,){11}P
    //try testMatchAll("^\\w*?abc", "qqqqabc");
}

test "badblood" {
    printRegexString("(abc){3,5}?$");
    // try testMatchAll("(abc){3,5}?$", "abcabcabcabcabcabc");
}

test "iteration" {
    const foo_str = "foobarbazfoo";
    var r_iter = compile("foo|bar|baz").?.iterator(foo_str);
    var matched = r_iter.next().?;
    try expectEqualStrings("foo", matched.slice);
    try expectEqualStrings("foo", foo_str[matched.start..matched.end]);
    matched = r_iter.next().?;
    try expectEqualStrings("bar", matched.slice);
    try expectEqualStrings("bar", foo_str[matched.start..matched.end]);
    matched = r_iter.next().?;
    try expectEqualStrings("baz", matched.slice);
    try expectEqualStrings("baz", foo_str[matched.start..matched.end]);
    matched = r_iter.next().?;
    try expectEqualStrings("foo", matched.slice);
    try expectEqualStrings("foo", foo_str[matched.start..matched.end]);
    try expectEqual(null, r_iter.next());
}

test "comptime regex" {
    const comp_regex = comptime compile("foo+").?;
    const run_match = comp_regex.match("foofoofoo");
    try expect(run_match != null);
    const comptime_match = comptime comp_regex.match("foofoofoo");
    try expect(comptime_match != null);
}
