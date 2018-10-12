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
programming](https://en.wikipedia.org/wiki/Literate_programming).

# Conventions

  * The [`lb` (literate binary) tool][lb] expects a [Markdown] file as input.
  * All [code blocks] in this file tied together form the content of the binary
    file created by `lb`. All other content including inline code in backticks
    `` `like this` `` is ignored, only code blocks are relevant.
  * The code blocks must contain nothing but hex characters (upper or lower
    case), macros, whitespace and comments.
  * Macros are used to write long, repetitive hex patterns in a compact way.
    They consist of a sequence of hex characters and a quantifier, similar to
    regular expression syntax. For example, the macro `(ff00){3}` expands to
    `ff00ff00ff00`. Macros may be nested, like
    `(ff(00e2){2}00((21){4}03){12}){8}`.
  * Comments start with a `#` sign and end at the end of the line.
  * There is one exception to the "all code blocks" rule; [fenced code blocks]
    with the `.nobin` class are ignored as well. This can be used to add code
    other than binary, or to "comment out" a whole code block.

[Here is a complete example][example] describing a simple Bitmap image file;
there are several others in the [examples/](examples/) directory.

# Usage

Get a binary [here][releases] and put it in your PATH. Then:

    $ lb --help
    $ lb source.bmp.md --output binary.bmp
    $ echo '(00ff){42} 0a' | lb --plain

Note that no Markdown is involved in the last command, just *plain* hex code.
Strictly speaking, this defeats the purpose of literate binary, reducing `lb` to
a beefed-up reverse hex dump tool. Anyway, it's useful for quick tests, so never
mind.

# Contributing

Pull requests for well-documented example files (minimal file format examples,
misuse of format specifications, ...) are greatly appreciated!

# Building from source

This tool is written in Haskell. It is recommended to use [Stack] for building.
Install Stack, clone the Git repository, change to its top level directory and
run the following commands:

    $ stack setup
    $ stack build
    $ stack install

This will install the `lb` executable to `~/.local/bin/` on Linux and to
`%AppData%\local\bin` on Windows.

[Markdown]: https://daringfireball.net/projects/markdown/basics
[Pandoc]: https://pandoc.org
[lb]: https://github.com/marhop/literate-binary
[releases]: https://github.com/marhop/literate-binary/releases
[example]: examples/bitmap.md
[example-wav]: examples/wave.md
[code blocks]: https://pandoc.org/MANUAL.html#verbatim-code-blocks
[fenced code blocks]: https://pandoc.org/MANUAL.html#fenced-code-blocks
[Stack]: https://docs.haskellstack.org/
