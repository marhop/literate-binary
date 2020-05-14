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

* [bilevel-tiff.md](bilevel-tiff.md) - a basic bilevel (black/white) TIFF file,
  including an introduction to the file format (start here!)
* [tiff-with-duplicate-pointer-error-1.md](tiff-with-duplicate-pointer-error-1.md)
  - a file that violates the TIFF specification because of a duplicate pointer
  in the XResolution and YResolution fields
* [tiff-with-duplicate-pointer-error-2.md](tiff-with-duplicate-pointer-error-2.md)
  - a file that violates the TIFF specification because of a duplicate pointer
  in the StripOffsets field
