This file is written in ["literate binary"][lb] notation, integrating binary
(expressed as hex code) with documentation written in [Markdown][Markdown]. To
derive an actual, binary TIFF file from this the [`lb` tool][lb] can be used.

# TIFF File Example

This file violates the [TIFF 6.0 specification][TIFF] because its Image File
Directory has no StripOffsets entry. It raises a [TIFF-HUL-26 "StripOffsets not
defined"][TIFF-HUL-26] error when validated with [JHOVE][JHOVE].

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
    # 0111 0003 00000003 0000008c # NO StripOffsets
    010e 0002 00000006 0000008c # ImageDescription, pointer
    0115 0003 00000001 00030000 # SamplesPerPixel, 3 components
    0116 0003 00000001 001e0000 # RowsPerStrip, 30
    0117 0003 00000003 00000092 # StripByteCounts, pointer
    011a 0005 00000001 00000098 # XResolution, pointer
    011b 0005 00000001 000000a0 # YResolution, pointer

**Note that this IFD has no StripOffsets entry in spite of it being required by
the TIFF specification, making the file invalid.** (In fact, the StripOffsets
entry has - just for fun - been replaced by an ImageDescription entry that still
points to the array of strip offsets at position 0x8c below, see next section.)
TIFF viewer software will rarely be able to render this image because without
the StripOffsets information it cannot know where to look for the image data
which could, in general, be placed almost anywhere in the file.

Pointer to next IFD or NULL.

    00000000

### Values That Don't Fit Into Four Bytes

Referenced from IFD.

    0008 0008 0008    # BitsPerSample, one byte per RGB component
    00a8 204c 3ff0    # StripOffsets, pointers
    1fa4 1fa4 1fa4    # StripByteCounts, 3 × 8100 bytes
    0000012c 00000001 # XResolution, 300/1
    0000012c 00000001 # YResolution, 300/1

Note that the StripOffsets data is still there (in the second line), only the
respective IFD entry is missing. Instead, this data is now referenced from the
ImageDescription IFD entry which causes it to be interpreted as ASCII text
describing the image. This does not yield any useful information though since
most of the bytes are not valid ASCII characters; moreover, the required NULL
byte at the end of the "string" is missing. Depending on how zealous the TIFF
specification is applied here this may qualify as another error making the file
invalid. Most viewer software would however silently ignore this.

## Image Data

Just some red/green/blue dummy image data.

    (ff0000){2700} # first strip, 30 red rows
    (00ff00){2700} # second strip, 30 green rows
    (0000ff){2700} # third strip, 30 blue rows

[lb]: https://github.com/marhop/literate-binary
[Markdown]: https://commonmark.org/
[TIFF]: https://archive.org/details/TIFF6
[JHOVE]: https://jhove.openpreservation.org/
[TIFF-HUL-26]: https://github.com/openpreserve/jhove/wiki/TIFF-hul-Messages#tiff-hul-26
