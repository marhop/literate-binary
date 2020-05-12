The documents in this directory describe TIFF files in ["literate binary"][lb]
notation, integrating handcrafted binary (expressed as hex code) with
documentation written in [Markdown][Markdown]. Two different representations can
be derived from these documents:

1. PDF/HTML/Latex ... files, using a Markdown converter like [Pandoc][Pandoc].
2. Binary TIFF files, using the [`lb` tool][lb].

[lb]: https://github.com/marhop/literate-binary
[Markdown]: https://commonmark.org/
[Pandoc]: https://pandoc.org/

The following files are available:

* [tiff-bilevel.md](tiff-bilevel.md) - a basic bilevel (black/white) TIFF file,
  including an introduction to the file format (start here!)
