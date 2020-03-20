The documents in this directory describe ZIP files in ["literate binary"][lb]
notation, integrating handcrafted binary (expressed as hex code) with
documentation written in [Markdown][Markdown]. Two different representations can
be derived from these documents:

 1. PDF/HTML/Latex ... files, using a Markdown converter like [Pandoc][Pandoc].
 2. Binary ZIP files, using the [`lb` tool][lb].

[lb]: https://github.com/marhop/literate-binary
[Markdown]: https://commonmark.org/
[Pandoc]: https://pandoc.org/

The following files are available:

  * [zip.md][zip.md] - a basic ZIP file, including an introduction to the file
    format (start here!)
  * [zip64.md][zip64.md] - a ZIP64 file
  * [zip-with-data-descriptor.md][zip-with-data-descriptor.md] - a ZIP file
    using a data descriptor, which is a rather special feature
