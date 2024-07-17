# mvzr: The Minimum Viable Zig Regex Library

Finding myself in need of a regular expressions library for a Zig project, and needing it to build regex at runtime, not just comptime, I ended up speedrunning a little library for just that purpose.

This is that library.  It's a simple bytecode-based Commander Pike-style VM.  Less than 1500 lines of load-bearing code, no dependencies other than `std`.

## Features

- Zero allocation, comptime and runtime compiling and matching
- Greedy qualifiers: `*`, `+`, `?`
- Lazy qualifiers: `*?`, `+?`, `??`
- Possessive/eager qualifiers: `*+`, `++`, `?+`
- Alternation: `foo|bar|baz`
- Grouping `foo|(bar|baz)+|quux`
- Sets: `[abc]`, `[^abc]`, `[a-z]`, `[^a=z]`
- Built-in character groups (ASCII): `\w`, `\W`, `\s`, `\S`, `\d`, `\D`
- Escape sequences: `\t`, `\n`, `\r`, `\xXX` hex format
- Begin and end `^` and `$`
- `{M}`, `{M,}`, `{M,N}`

## Limitations and Quirks

- Only 64 operations per regex
- Only 8 character sets per regex (ASCII only)
    - These values could probably be made comptime-configurable, but I didn't
- No Unicode support to speak of
- No fancy modifiers (you want case-insensitive, great, lowercase your string)
- `.` matches any one byte.  `[^\n\r]` works fine if that's not what you want
    - Or split into lines first, divide and conquer
- Backtracks (sorry. For this to work without backtracking, we need async back)
- Compiler does some best-effort validation but I haven't really pounded on it
- No min/max `{X,}`, `{X,Y}`.  Might get around to it (escape your curlies just in case)
- No capture groups.  Divide and conquer

As long as you color within the lines, it should be fine.

The main thing to realize is that this is only for **known regex patterns**.  It uses the stack to store the `M - N` part of an `{M,N}`, it backtracks greedy matches, and if you don't control the strings that get compiled, don't use it.

Much like managing your own memory, if you know your tools and are smart about it, you can get a lot done with `mvzr`.

## Interface

`mvzr.Regex` is available at `comptime` or runtime, and returns an `mvzr.Match`, consisting of a `.slice` field containing the match, as well as the `.start` and `.end` locations in the haystack.  This is a borrowed slice, to own it, call `match.toOwned(allocator)`, and deallocate later with `match.deinit(allocator)`, or just free the `.slice`.

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

Fewer over time, I hope.  This library isn't up to `0.1` yet, but the test suite never shrinks.
