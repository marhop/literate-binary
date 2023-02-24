# literate-binary

There are times when a binary file has to be crafted *by hand*, be it for
testing purposes, for research, or just for some weird kind of fun. Usually, a
hex editor is the tool of choice for straightforward binary editing. But what
about documentation? There is no way to enter inline comments or other
explanatory prose in a hex editor, leading to separation of hex code and
documentation. (Or worse, to no documentation at all.)

Wouldn't it be great if there was a way to combine binary (expressed as hex
code) and textual content in the same file? If both a binary file and
accompanying documentation in formats like HTML or PDF could be generated from
the same source file? There is!

Just write a [Markdown] file [like this example][example]. Run it through your
favourite Markdown converter (mine is [Pandoc]) to create a pretty HTML or PDF
document; run it through [`lb`][lb] to create a binary file from its hex
content.

                    pandoc -> documentation.{html,pdf,docx,...}
                  /
    source.bmp.md
                  \
                    lb     -> binary.bmp

Obviously, this is inspired by [Donald Knuth's ideas on literate
programming](https://en.wikipedia.org/wiki/Literate_programming); hence the
name.

There are several example files in the [examples/](examples/) directory. I have
also written a [long blog post][blog] that explains how to build a TIFF file
using literate binary.

# Syntax

The [`lb` (literate binary) tool][lb] produces a binary file from a [Markdown]
input file. The binary is made up by all [code blocks] in the input file tied
together. All other content including inline code in backticks `` `like this` ``
is ignored. The code blocks must contain nothing but hex strings, whitespace and
comments. The set of valid hex strings is defined as follows:

Each sequence of hex characters (0-9, A-F, upper or lower case) with even length
like `00ff` is a valid hex string; each pair of characters in this sequence
translates to one byte in the usual fashion. Given hex strings x and y and a
positive integer n, the following macros are valid hex strings as well:

  * A *repetition* of the form `(x){n}`. This translates to the byte sequence
    corresponding to x, repeated n times. The integer n may be followed by a
    multiplicative suffix `K` (factor 2¹⁰, KiB), `M` (factor 2²⁰, MiB), or `G`
    (factor 2³⁰, GiB). Examples: `(00ff){3}` → `00ff00ff00ff`, `(00){2M}` → 2
    MiB of NULL bytes
  * An *alternative* of the form `(x|...|y)`. This translates to the byte
    sequence corresponding to either x or ... or y, selected randomly. Example:
    `(00|ff|3333)` → one of `00`, `ff` or `3333`
  * A *range* of the form `(x-y)`. This translates to one random byte sequence
    from the range defined by x and y. Example: `(0c-0f)` → one of `0c`, `0d`,
    `0e` or `0f`
  * The special range `.` (a single dot). This translates to one random byte, so
    it is equivalent to the range `(00-ff)`.
  * A *string* of the form `"..."` or `'...'` with arbitrary text content inside
    the quotes. The quote sign itself may appear inside the string escaped by a
    backslash like `\"`, a literal backslash has to be escaped by another
    backslash like `\\`. This translates to the UTF-8 encoded byte sequence
    corresponding to the quoted string content. (Note that ASCII is a subset of
    UTF-8.) Example: `"foo bar"` → `666f6f20626172`

When combining an alternative, a range or a string with a repetition, redundant
parentheses are not required: `(x|y){n}` is equivalent to `((x|y)){n}`,
`(x-y){n}` is equivalent to `((x-y)){n}`, `.{n}` is equivalent to `(.){n}`, and
`"foo"{3}` is equivalent to `("foo"){3}`.

Comments start with a `#` sign and end at the end of the line.

It is possible to exclude single code blocks from processing; [fenced code
blocks] with the `.nobin` class are ignored by `lb`. This can be used to add
code other than binary, or to "comment out" a whole code block.

# Usage

Get a binary [here][releases] and put it in your PATH (and, if necessary, make
it executable -- hint: `chmod u+x lb`). Then:

    $ lb --help
    $ lb source.bmp.md --output binary.bmp
    $ echo '(00ff){42} 0a' | lb --plain

Note that no Markdown is involved in the last command, just *plain* hex code.
Strictly speaking, this defeats the purpose of literate binary, reducing `lb` to
a beefed-up reverse hex dump tool. Anyway, it's useful for quick tests, so never
mind.

# Contributing

Pull requests, particularly well-documented example files (minimal file format
examples, misuse of format specifications, ...) are greatly appreciated!

# Building from source

This tool is written in Haskell, so you need GHC (compiler) and Cabal (build
tool), best installed with [ghcup] if you use a Unix-like OS. Clone the Git
repository, change to its top level directory and run the following commands to
build and optionally install the `lb` binary:

    $ cabal build
    $ cabal install

[Markdown]: https://daringfireball.net/projects/markdown/basics
[Pandoc]: https://pandoc.org
[lb]: https://github.com/marhop/literate-binary
[releases]: https://github.com/marhop/literate-binary/releases
[example]: examples/bitmap/bitmap.md
[blog]: https://martin.hoppenheit.info/blog/2022/writing-binary-by-hand/
[code blocks]: https://pandoc.org/MANUAL.html#verbatim-code-blocks
[fenced code blocks]: https://pandoc.org/MANUAL.html#fenced-code-blocks
[ghcup]: https://www.haskell.org/ghcup/
