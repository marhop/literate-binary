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
contains a duplicate pointer in the XResolution and YResolution fields.

## Image File Header

Byte order (big endian) + number 42 + offset of first IFD.

    "MM" 002a 00000008

## Image File Directory

Number of directory entries.

    0008

Sequence of 12-byte IFD entries. Structure of each entry: tag + type + count +
value/pointer.

    0100 0003 00000001 02000000 # ImageWidth, 512 pixels
    0101 0003 00000001 01400000 # ImageLength, 320 pixels
    0106 0003 00000001 00000000 # PhotometricInterpretation, WhiteIsZero
    0111 0003 00000003 0000006e # StripOffsets, pointer
    0116 0003 00000001 00800000 # RowsPerStrip, 128
    0117 0003 00000003 00000074 # StripByteCounts, pointer
    011a 0005 00000001 0000007a # XResolution, pointer
    011b 0005 00000001 0000007a # YResolution, pointer

**Note how both XResolution and YResolution point to the same offset (0x7a)!
This is illegal as per the TIFF specification:**

> No Duplicate Pointers. No data should be referenced from more than one place.
> TIFF readers and editors are under no obligation to detect this condition and
> handle it properly. This would not be a problem if TIFF files were read-only
> entities, but they are not.
>
> (TIFF 6.0, page 26)

Pointer to next IFD or NULL.

    00000000

### Values that don't fit into four bytes

Referenced from IFD.

    0082 2082 4082    # StripOffsets, pointers
    2000 2000 1000    # StripByteCounts, 8/8/4 KiB
    0000012c 00000001 # XResolution and YResolution, 300/1

Note how both XResolution and YResolution point to the same value (300/1)
because both horizontal and vertical resolution are set to 300 pixels per inch.
Sounds useful, isn't allowed though.

## Image data

Just some black/white dummy image data.

    (ff){8K}          # first strip, 128 black rows
    (00){4K} (ff){4K} # second strip, 64 white + 64 black rows
    (ff){4K}          # third strip, 64 black rows

[TIFF]: https://www.adobe.io/content/dam/udp/en/open/standards/tiff/TIFF6.pdf
