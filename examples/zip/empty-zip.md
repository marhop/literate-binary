This document describes a ZIP file in ["literate binary"][lb] notation,
integrating handcrafted binary (expressed as hex code) with documentation
written in [Markdown][Markdown]. Two different representations can be derived
from this document:

1. A PDF/HTML/Latex ... file, using a Markdown converter like [Pandoc][Pandoc].
2. A binary ZIP file, using the [`lb` tool][lb].

[lb]: https://github.com/marhop/literate-binary
[Markdown]: https://commonmark.org/
[Pandoc]: https://pandoc.org/

# Empty ZIP File Example

According to the [file format specification][APPNOTE] a ZIP archive consists of
a series of records that describe the files contained in the archive. The global
structure looks like this:

* local file header + data for first file
* local file header + data for second file
* ...
* central directory
  * central directory header for first file
  * central directory header for second file
  * ...
  * end of central directory record

However, if a ZIP archive is empty (i.e., contains no files at all) most of the
elements in this structure make no sense, leaving only this:

* central directory
  * end of central directory record

So an empty ZIP archive consists of nothing but the end of central directory
record, which looks like this (see [[APPNOTE] section 4.3.16]):

 1. The end of central directory signature.

        504b 0506

 2. The number of this disk. ZIP files may be split on multiple disks (like
    multiple floppies).

        0000

 3. The number of the disk with the start of the central directory.

        0000

 4. The total number of entries in the central directory on this disk.

        0000

 5. The total number of entries in the central directory. This is the number of
    files in this ZIP archive, so for an empty archive this is obviously 0.

        0000

 6. The size of the central directory, not counting the end of central directory
    record itself. No entries in the central directory (other than the end of
    central directory record, which doesn't count), so again 0.

        0000 0000

 7. The offset of the central directory. This is used to locate the start of the
    central directory. Since there is nothing in this archive before the central
    directory it starts at offset 0 (BOF).

        0000 0000

 8. The length of the ZIP file comment. See field 9.

        0000

 9. The ZIP file comment. None in this example.

[APPNOTE]: https://pkware.cachefly.net/webdocs/casestudies/APPNOTE.TXT
