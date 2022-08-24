This document describes a TIFF file in ["literate binary"][lb] notation,
integrating handcrafted binary (expressed as hex code) with documentation
written in [Markdown][Markdown]. Two different representations can be derived
from this document:

1. A PDF/HTML/Latex ... file, using a Markdown converter like [Pandoc][Pandoc].
2. A binary TIFF file, using the [`lb` tool][lb].

[lb]: https://github.com/marhop/literate-binary
[Markdown]: https://commonmark.org/
[Pandoc]: https://pandoc.org/

# TIFF file example

This example file violates the Baseline [TIFF 6.0 specification][TIFF]. It
contains a duplicate pointer in the StripOffsets field.

## Image File Header

Byte order (big endian) + number 42 + offset of first IFD.

    "MM" 002a 00000008

## Image File Directory

Number of directory entries.

    0008

Sequence of 12-byte IFD entries. Structure of each entry: tag + type + count +
value/pointer.

    0100 0003 00000001 01000000 # ImageWidth, 256 pixels
    0101 0003 00000001 04000000 # ImageLength, 1024 pixels
    0106 0003 00000001 00000000 # PhotometricInterpretation, WhiteIsZero
    0111 0003 00000004 0000006e # StripOffsets, pointer
    0116 0003 00000001 01000000 # RowsPerStrip, 256
    0117 0003 00000004 00000076 # StripByteCounts, pointer
    011a 0005 00000001 0000007e # XResolution, pointer
    011b 0005 00000001 00000086 # YResolution, pointer

Pointer to next IFD or NULL.

    00000000

### Values that don't fit into four bytes

Referenced from IFD.

    008e 008e 008e 008e # StripOffsets, pointers
    2000 2000 2000 2000 # StripByteCounts, 8/8/8/8 KiB
    0000012c 00000001   # XResolution, 300/1
    0000012c 00000001   # YResolution, 300/1

**Note how the StripOffsets array points to the same offset (0x8e) four times!
This is illegal as per the TIFF specification:**

> No Duplicate Pointers. No data should be referenced from more than one place.
> TIFF readers and editors are under no obligation to detect this condition and
> handle it properly. This would not be a problem if TIFF files were read-only
> entities, but they are not. This warning covers both TIFF field value offsets
> and fields that are defined as offsets, such as StripOffsets.
>
> (TIFF 6.0, page 26)

## Image data

Just some black/white dummy image data. By referencing this four times from the
StripOffsets array the image data is repeated four times.

    (ff){2K} (00){4K} (ff){2K} # 256 rows, 256 columns, black/white

[TIFF]: https://archive.org/details/TIFF6
