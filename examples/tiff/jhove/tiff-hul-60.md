This file is written in ["literate binary"][lb] notation, integrating binary
(expressed as hex code) with documentation written in [Markdown][Markdown]. To
derive an actual, binary TIFF file from this the [`lb` tool][lb] can be used.

# TIFF File Example

This file violates the [TIFF 6.0 specification][TIFF] because it contains
duplicate pointers. Somewhat related, it raises a [TIFF-HUL-60 "More than 50
IFDs in chain, probably an infinite loop"][TIFF-HUL-60] error when validated
with [JHOVE][JHOVE] because there is, in fact, an infinite loop of IFDs.

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
    011b 0005 00000001 000000a0 # YResolution, pointer

Pointer to next IFD or NULL.

    00000008

**Note that the offset of the next IFD recorded here is the same as that of the
first IFD referenced in the file header above. In other words, this IFD points
to itself, creating an infinite loop.** This is obviously not a wise thing to do
and TIFF viewer software is not very likely to render this image. However, what
makes this file technically invalid with respect to the TIFF specification is
the fact that the same pointer to offset 0x00000008 is used twice (in the image
file header and at the end of the IFD), violating the "No Duplicate Pointers. No
data should be referenced from more than one place." rule on page 26 of the
[TIFF 6.0 specification][TIFF]. Further more, it should be pointed out that even
a very long chain of IFDs does not per se violate the TIFF specification ­ this
feature is commonly used for multi page TIFFs (e.g., scanned documents) ­ as
long as it does not include circular (and thus, duplicate) references.

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
[Markdown]: https://commonmark.org/
[TIFF]: https://archive.org/details/TIFF6
[JHOVE]: https://jhove.openpreservation.org/
[TIFF-HUL-60]: https://github.com/openpreserve/jhove/wiki/TIFF-hul-Messages#tiff-hul-60
