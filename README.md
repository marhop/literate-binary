# literate-binary

There are times when a binary file has to be crafted *by hand*, be it for
testing purposes, for research, or just for some weird kind of fun. Usually, a
hex editor is the tool of choice for straightforward binary editing. But what
about documentation? There is no way to enter inline comments or other
explanatory prose in a hex editor, leading to separation of hex code and
documentation. (Or worse, to no documentation at all.)

Wouldn't it be great if there were a way to combine binary (expressed as hex
code) and textual content in the same file? If both a binary file and
accompanying documentation in formats like HTML or PDF could be generated from
the same source file? There is!

Just write a [Markdown] file [like this example][example]. Run it through your
favourite Markdown converter (mine is [Pandoc]) to create a pretty HTML or PDF
document; run it through the [`lb` tool][lb] to create a binary file from its
hex content.

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
    file created by the `lb` tool. All other content including inline code in
    backticks `` `like this` `` is ignored, only code blocks are relevant.
  * The code blocks must contain nothing but hex characters (upper or lower
    case), whitespace and comments. Comments start with a `#` sign and end at
    the end of the line. [Here is an example.][example]

# Usage

TODO Coming soon ...

[Markdown]: https://daringfireball.net/projects/markdown/basics
[Pandoc]: https://pandoc.org
[lb]: https://github.com/marhop/literate-binary
[example]: examples/minimal.bmp.md
[code blocks]: https://pandoc.org/MANUAL.html#verbatim-code-blocks
