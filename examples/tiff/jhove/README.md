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

Example files for the following JHOVE errors are available:

- [TIFF-HUL-3 "Unknown data type"][TIFF-HUL-3]: [tiff-hul-3.md](tiff-hul-3.md)
- [TIFF-HUL-7 "Type mismatch for tag x; expecting y, saw z"][TIFF-HUL-7]:
  [tiff-hul-7.md](tiff-hul-7.md)
- [TIFF-HUL-8 "Type mismatch for tag x; expecting y1 or y2, saw z"][TIFF-HUL-8]:
  [tiff-hul-8.md](tiff-hul-8.md)
- [TIFF-HUL-26 "StripOffsets not defined"][TIFF-HUL-26]:
  [tiff-hul-26.md](tiff-hul-26.md)
- [TIFF-HUL-27 "StripByteCounts not defined"][TIFF-HUL-27]:
  [tiff-hul-27.md](tiff-hul-27.md)

[TIFF]: https://archive.org/details/TIFF6
[JHOVE]: https://jhove.openpreservation.org/
[lb]: https://github.com/marhop/literate-binary
[Markdown]: https://commonmark.org/

[TIFF-HUL-3]: https://github.com/openpreserve/jhove/wiki/TIFF-hul-Messages#tiff-hul-3
[TIFF-HUL-7]: https://github.com/openpreserve/jhove/wiki/TIFF-hul-Messages#tiff-hul-7
[TIFF-HUL-8]: https://github.com/openpreserve/jhove/wiki/TIFF-hul-Messages#tiff-hul-8
[TIFF-HUL-26]: https://github.com/openpreserve/jhove/wiki/TIFF-hul-Messages#tiff-hul-26
[TIFF-HUL-27]: https://github.com/openpreserve/jhove/wiki/TIFF-hul-Messages#tiff-hul-27
