# mvzr: The Minimum Viable Zig Regex Library

Finding myself in need of a regular expressions library for a Zig project, and needing it to build regex at runtime, not just comptime, I ended up speedrunning a little library for just that purpose.

This is that library.  It's a simple bytecode-based Commander Pike-style VM.  Less than 1000 lines of load-bearing code, no dependencies other than `std`.

## Features

- Zero Allocation
- Greedy qualifiers: `*`, `+`, `?`
- Lazy qualifiers: `*?`, `+?`, `??`
- Possessive/eager qualifiers: `*+`, `++`, `?+`
- Alternation: `foo|bar|baz`
- Grouping `foo|(bar|baz)+|quux`
- Sets: `[abc]`, `[^abc]`, `[a-z]`, `[^a=z]`
- Built-in character groups (ASCII): `\w`, `\W`, `\s`, `\S`, `\d`, `\D`
- Escape sequences: `\t`, `\n`, `\r`, `\xXX` hex format
- Begin and end `^` and `$`

## Limitations and Quirks

- Only 64 operations per regex
- Only 8 character sets per regex (ASCII only)
    - These values could probably be made comptime-configurable, but I didn't
- No Unicode support to speak of
- No fancy modifiers (you want case-insensitive, great, lowercase your string)
- `.` matches anything, `[^\n\r]` works fine if that's not what you want
- Backtracks (sorry. For this to work without backtracking, we need async back)
- Compiler does some best-effort validation but I haven't really pounded on it
- No min/max `{X,}`, `{X,Y}`.  Might get around to it (escape your curlies just in case)
- No capture groups.  Divide and conquer

As long as you color within the lines, it should be fine.

## Interface

`mvzr.Regex` is available at `comptime` or runtime, and returns an `mvzr.Match`, consisting of a `.slice` field containing the match, as well as the `.start` and `.end` locations in the haystack.  This is a borrowed slice, to own it, call `match.toOwned(allocator)`.

```zig
const regex: mvzr.Regex = mvzr.compile(patt_str).?;
const match: mvzr.Match = regex.match(haystack).?;
const match2: mvzr.Match = match(haystack, patt_str).?;
const did_match: bool = regex.isMatch(haystack);
const iter: mvzr.RegexIterator = regex.iterator(haystack);

while (iter.next()) |m| {
    // ...
}
```

## Bugs

I wrote this thing in ~~two~~ three days flat (check the log).  There are bugs.  Point them out and I might even fix them.
