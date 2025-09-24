# mvzr: The Minimum Viable Zig Regex Library

Finding myself in need of a regular expressions library for a Zig project, and needing it to build regex at runtime, not just comptime, I ended up speedrunning a little library for just that purpose.

This is that library.  It's a simple bytecode-based VM, inspired by [LPEG](https://www.inf.puc-rio.br/~roberto/docs/peg.pdf).  Under 2000 lines of load-bearing code, no dependencies other than `std`.

The provided Regex type allows 64 'operations' and 8 unique ASCII character sets.  If you would like more, or less, you can call `SizedRegex(num_ops, num_sets)` to customize the type.

## Installation

Drop the file into your project, or use the Zig build system:

```zig
zig fetch --save "https://github.com/mnemnion/mvzr/archive/refs/tags/v0.3.7.tar.gz"
```

I'll do my best to keep that URL fresh, but it pays to check over here: ➔

For the latest release version.

`v0.3.7` is the first version supporting Zig `0.15.1`, if you're still on `0.14`, stick with `v0.3.6`.

## Features

- Zero allocation, comptime and runtime compiling and matching
- X operations per regex
- Y character sets per regex
- Greedy qualifiers: `*`, `+`, `?`
- Lazy qualifiers: `*?`, `+?`, `??`
- Possessive/eager qualifiers: `*+`, `++`, `?+`
- Alternation: `foo|bar|baz`
- Grouping `foo|(bar|baz)+|quux`
- Sets: `[abc]`, `[^abc]`, `[a-z]`, `[^a-z]`, `[\w+-]`, `[\x04-\x1b]`
- Built-in character groups (ASCII): `\w`, `\W`, `\s`, `\S`, `\d`, `\D`
- Escape sequences: `\t`, `\n`, `\r`, `\xXX` hex format
    - Same set as Zig: if you need the weird C ones, use `\x` format
- Begin and end `^` and `$`
- Word boundaries `\b`, `\B`
- `{M}`, `{M,}`, `{M,N}`, `{,N}`

## Limitations and Quirks

- Minimal multibyte / Unicode support
    - This has improved somewhat.  A regex like `λ?` now matches an optional lambda, not just
      an optional final byte.  Additionally, ranges of bytes greater than 0x7f are now supported,
      this (with some care) can match certain sets: for instance `(\xce[\x91-\xa9])+` will match
      a string of uppercase Greek letters, `\xc2[\x80-\x9f]` matches a C1 control code, and so on.
      But you'll still need to work at the byte level, and use `\x` format, to do these tasks.
- No fancy modifiers (you want case-insensitive, great, lowercase your string)
- `.` matches any one byte.  `[^\n\r]` works fine if that's not what you want
    - Or split into lines first, divide and conquer
    - Note: `$` permits a final newline, but `^` must be the beginning of a string, and `$` _only_ matches a final newline.
- Backtracks (sorry. For this design to work without backtracking, we need async back)
- Compiler does some best-effort validation but I haven't really pounded on it
- No capture groups.  Divide and conquer

As long as you color within the lines, it should be fine.

This library is not intended for use where an attacker could conceivably control the regex pattern.

Much like managing your own memory, if you know your tools and are smart about it, you can get a lot done with `mvzr`.

## Interface

`mvzr.Regex` is available at `comptime` or runtime, and returns an `mvzr.Match`, consisting of a `.slice` field containing the match, as well as the `.start` and `.end` locations in the haystack.  This is a borrowed slice, to own it, call `match.toOwnedMatch(allocator)`, and deallocate later with `match.deinit(allocator)`, or just free the `.slice`.

Similarly, if you need to store a `Regex` or `SizedRegex` for later, call `regex.toOwnedRegex(allocator)`, freeing later with `allocator.destroy(heap_regex)`.

```zig
// aka SizedRegex(64, 8)
const regex: mvzr.Regex = mvzr.compile(patt_str).?;
// or mvzr.Regex.compile(patt_str)
const match: mvzr.Match = regex.match(haystack).?;
const match2: mvzr.Match = match(haystack, patt_str).?;
const did_match: bool = regex.isMatch(haystack);
const iter: mvzr.RegexIterator = regex.iterator(haystack);

while (iter.next()) |m| {
    // ...
}

// Comptime-only
const ops, const sets = mvzr.resourcesNeeded("abc?d*[^efgh]++2");

// I suggest adding the values directly here once they're established
const SlimmedDownRegex = mvzr.SizedRegex(ops, sets);
```

## Compile Errors

If a regex string is unable to compile, `mvzr` will return `null`.  It will also log an informative error message.  While this is useful, it may not be desirable, so `mvzr` uses a [scoped logger](https://ziglang.org/documentation/0.12.0/std/#std.log) with the scope `.mvzr`, to make it easy for a custom logging function to filter those messages out.

## Bugs

Fewer over time, I hope.  The test suite never shrinks.

### Bug Reports

Always welcome.  Ideally, presented as a failing test block, with a note on expected behavior.

