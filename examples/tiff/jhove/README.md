The documents in this directory describe TIFF files that violate the [TIFF 6.0
specification][TIFF] in one way or another and are thus flagged as invalid by
the [JHOVE][JHOVE] file format validation tool.

The files are written in ["literate binary"][lb] notation, integrating binary
(expressed as hex code) with documentation written in [Markdown][Markdown]. To
derive actual, binary TIFF files from these documents the [`lb` tool][lb] can be
used like this:

~~~console
$ lb tiff-hul-26.md -o tiff-hul-26.tif
~~~

The following files are available:

- [tiff-hul-26.md](tiff-hul-26.md) - this file raises a [TIFF-HUL-26
  "StripOffsets not defined"][TIFF-HUL-26] error.

[TIFF]: https://archive.org/details/TIFF6
[JHOVE]: https://jhove.openpreservation.org/
[lb]: https://github.com/marhop/literate-binary
[Markdown]: https://commonmark.org/

[TIFF-HUL-26]: https://github.com/openpreserve/jhove/wiki/TIFF-hul-Messages#tiff-hul-26
