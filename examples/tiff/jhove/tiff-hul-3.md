This file is written in ["literate binary"][lb] notation, integrating binary
(expressed as hex code) with documentation written in [Markdown][Markdown]. To
derive an actual, binary TIFF file from this the [`lb` tool][lb] can be used.

# TIFF File Example

This file violates the [TIFF 6.0 specification][TIFF] because one of its IFD
entries has an unknown data type. It raises a [TIFF-HUL-3 "Unknown data
type"][TIFF-HUL-3] error when validated with [JHOVE][JHOVE].

## Image File Header

Byte order (big endian) + number 42 + offset of first IFD.

    "MM" 002a 00000008

## Image File Directory

Number of directory entries.

    000a

Sequence of 12-byte IFD entries. Structure of each entry: tag + type + count +
value/pointer.

    0100 0003 00000001 005a0000 # ImageWidth, 90 pixels
    0101 0003 00000001 005a0000 # ImageLength, 90 pixels
    0102 0003 00000003 00000086 # BitsPerSample, pointer
    0106 0003 00000001 00020000 # PhotometricInterpretation, RGB
    0111 0003 00000003 0000008c # StripOffsets, pointer
    0115 0003 00000001 00030000 # SamplesPerPixel, 3 components
    0116 0003 00000001 001e0000 # RowsPerStrip, 30
    0117 0003 00000003 00000092 # StripByteCounts, pointer
    011a 0005 00000001 00000098 # XResolution, pointer
    011b 029a 00000001 000000a0 # YResolution, pointer

**Note that the YResolution entry in this IFD has type 0x029a instead of 0x0005
(RATIONAL). This type is specified neither in the core TIFF specification nor in
any extension I am aware of, making the file invalid.** However, TIFF viewer
software might still be able to render the image if the respective entry's value
is not critical for rendering, or if the viewer software "knows" the correct
data type and can thus silently ignore the type given in the IFD entry (e.g., as
per the specification, YResolution always has type 0x0005).

Pointer to next IFD or NULL.

    00000000

### Values That Don't Fit Into Four Bytes

Referenced from IFD.

    0008 0008 0008    # BitsPerSample, one byte per RGB component
    00a8 204c 3ff0    # StripOffsets, pointers
    1fa4 1fa4 1fa4    # StripByteCounts, 3 × 8100 bytes
    0000012c 00000001 # XResolution, 300/1
    0000012c 00000001 # YResolution, 300/1

## Image Data

Just some red/green/blue dummy image data.

    (ff0000){2700} # first strip, 30 red rows
    (00ff00){2700} # second strip, 30 green rows
    (0000ff){2700} # third strip, 30 blue rows

[lb]: https://github.com/marhop/literate-binary
[TIFF]: https://archive.org/details/TIFF6
[JHOVE]: https://jhove.openpreservation.org/
[TIFF-HUL-3]: https://github.com/openpreserve/jhove/wiki/TIFF-hul-Messages#tiff-hul-3
