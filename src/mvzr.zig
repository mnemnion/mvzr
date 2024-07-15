//! mvzr: Minimum Viable Zig Regex
//!
//! A minimalistic, but y'know, viable, Zig regex library.
//!
//! Focused on basic support of runtime-provided regular expressions.
const std = @import("std");
const testing = std.testing;

/// Maximum regex operations
pub const MAX_REGEX_OPS = 50;
/// Maximum character sets, ASCII only
pub const MAX_CHAR_SETS = 10;

const RegexType = enum {
    unused,
    dot,
    begin,
    end,
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
        c_off: u32, // offset into character set array
    },
};

pub const CharSet = struct {
    low: u64 = 0,
    hi: u64 = 0,
};

const Regex = struct {
    patt: [MAX_REGEX_OPS]RegOp,
    sets: [MAX_CHAR_SETS]CharSet,
};

/// Compile a regex.
pub fn re_compile(in: []const u8) Regex {
    var out = Regex{};
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
    _ = set; // autofix
    var i: usize = 0;
    var j: usize = 0;
    var s: usize = 0;
    _ = s; // autofix
    while (i < in.len and j + 1 < patt.len) : (j += 1) {
        const c = in[i];
        switch (c) {
            '^' => {},
            '$' => {},
            '.' => {},
            '*' => {},
            '?' => {},
            '|' => {},
        }
    }
}
