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

This example file conforms to the [TIFF 6.0 specification][TIFF]. It contains an
RGB full-color image. In an RGB image each pixel is represented by three
components forming an RGB triple.

## Image File Header

A TIFF file begins with an Image File Header consisting of three fields: the
byte order used in the file ("II" (0x4949) for little endian or "MM" (0x4d4d)
for big endian), an "arbitrary but carefully chosen number (42) that further
identifies the file as a TIFF file" (TIFF 6.0, page 13), and finally the offset
of the first Image File Directory (IFD).

    "MM" 002a 00000008

Following the header a TIFF file has no fixed overall structure. The header
points to the first IFD, and the IFD points to the image data, to IFD entry
values and potentially to other IFDs. The whole file structure is connected
solely by pointers (offsets). In particular, the components do not have to be in
any specific order (and could even be interspersed with junk data, but of course
no sensible TIFF writer would do such a thing). Let this be illustrated by an
ominous quote from the TIFF specification:

> Readers must follow the pointers wherever they may lead.
>
> (TIFF 6.0, page 13)

## Image File Directory

The Image File Directory (IFD) contains information about the image. This is
first and foremost technical metadata required to interpret the actual image
data but may also be other metadata like EXIF tags or XMP.

An IFD starts with a two-byte field that holds the number of directory entries.

    000a

This is followed by a sequence of 12-byte IFD entries where each entry has the
same structure made up of four components:

1. a tag (number) that identifies the field, two bytes
2. the field type (BYTE, ASCII, SHORT, ...), two bytes
3. the count/number of values of this type in the field (*not* the size in
   bytes), four bytes
4. either the field value itself (iff it fits into four bytes) or a pointer to
   (i.e., offset of) the field value (otherwise), four bytes

If the last component holds the value itself then the value is left-justified
within the four bytes, independent of endianness, so the value 0xf7 represented
as a big endian SHORT (16-bit number) looks like `00f70000`. The size of the
value can be determined by type and count of the field e.g., three values of
type SHORT occupy six bytes.

Several fields are required for a baseline TIFF RGB image; some of them have
default values though so they do not have to be included if the default value is
appropriate.

* ImageWidth, tag 256 = 0x0100, one value of type SHORT (0x03, could also be
  LONG). The width of the image in pixels; 90 = 0x5a columns. See how the 16-bit
  SHORT is left-justified within the value field?

      0100 0003 00000001 005a0000

* ImageLength, tag 257 = 0x0101, one value of type SHORT (could also be LONG).
  The length (height) of the image in pixels; 90 = 0x5a rows.

      0101 0003 00000001 005a0000

* BitsPerSample, tag 258 = 0x0102, a three-values array of type SHORT. The
  number of bits per sample i.e., per Red/Green/Blue component. Note that three
  SHORTs do not fit into four bytes so a pointer to the array is inserted
  instead. It points to offset 0x86 which is where the three SHORTs are stored
  (see next section).

      0102 0003 00000003 00000086

* Compression, tag 259 = 0x0103, one value of type SHORT. The default is 1 (no
  compression). Since the image data in this file is in fact uncompressed it is
  not necessary (but wouldn't hurt either) to include this field. It would look
  like this if included: `0103 0003 00000001 00010000`.

* PhotometricInterpretation, tag 262 = 0x0106, one value of type SHORT. The
  color space (or rather color model) of the image data; 2 = RGB.

      0106 0003 00000001 00020000

* StripOffsets, tag 273 = 0x0111, a three-values array of type SHORT (could also
  be LONG). The offsets of the strips containing the actual image data; the
  meaning of strips is explained in the section on image data below. Note that
  three SHORTs do not fit into four bytes so a pointer to the array at offset
  0x8c is inserted instead. Depending on the number of strips used in a file
  there may be less or much more than three offset values.

      0111 0003 00000003 0000008c

* SamplesPerPixel, tag 277 = 0x0115, one value of type SHORT. The number of
  components per pixel; 3 = 0x03 samples (Red, Green, Blue) per pixel.

      0115 0003 00000001 00030000

* RowsPerStrip, tag 278 = 0x0116, one value of type SHORT (could also be LONG).
  The number of rows (lines of pixels) stored in each strip; 30 = 0x1e rows per
  strip. Depending on the total number of rows the last strip may have fewer
  rows.

      0116 0003 00000001 001e0000

* StripByteCounts, tag 279 = 0x0117, a three-values array of type SHORT (could
  also be LONG). The number of bytes in each strip of image data; again, this is
  a pointer to the actual array value at offset 0x92.

      0117 0003 00000003 00000092

* XResolution, tag 282 = 0x011a, one value of type RATIONAL (0x05). The number
  of pixels per ResolutionUnit (see below) in the ImageWidth (typically,
  horizontal) direction. A RATIONAL number consists of the two components of a
  fraction, numerator and denominator, each represented by a four-byte LONG.
  Obviously, that does not fit into four bytes, so a pointer to the actual value
  at offset 0x98 is used.

      011a 0005 00000001 00000098

* YResolution, tag 283 = 0x011b, one value of type RATIONAL. The number of
  pixels per ResolutionUnit in the ImageLength (typically, vertical) direction,
  just like XResolution; a pointer to offset 0xa0.

      011b 0005 00000001 000000a0

* ResolutionUnit, tag 296 = 0x0128, one value of type SHORT. The default is 2
  (inch) which is used in this example, so like the Compression field above this
  is not needed. It would look like this: `0128 0003 00000001 00020000`.

There are two requirements in the TIFF specification regarding IFD entries that
are easily violated. First, the entries have to be sorted in ascending order by
tag number (TIFF 6.0, page 15). Second, the field values must begin on a word
boundary, so the pointers (offsets) to the values have to be even numbers (TIFF
6.0, page 15). This condition also applies to the IFD offset (TIFF 6.0, page
13). Violating these requirements usually doesn't cause problems for viewers or
other TIFF-processing software, but requirements are requirements, so file
format validation software will yell at you if they are not fulfilled.

An IFD ends with either a four-byte pointer to the next IFD or four NULL bytes
if it is the last (or only, as in this example) IFD.

    00000000

Why "next IFD"? A TIFF file may contain multiple IFDs that describe multiple
images, called subfiles by the TIFF specification (TIFF 6.0, page 16). This
feature is used e.g. for multi page TIFFs like scanned documents but also for
embedded thumbnail images.

### Values that don't fit into four bytes

As mentioned in the previous section the values of several fields do not fit
into four bytes and are thus replaced by pointers to the actual values. These
offsets may point more or less anywhere in the file. To keep things clear though
in this example they all point to this section i.e., the values directly follow
the IFD.

* BitsPerSample, a three-values array of type SHORT. Each sample (Red/Green/Blue
  component) is encoded using one byte.

      0008 0008 0008

  Note that this code block itself will start at offset 0x86 in a binary file
  created by the `lb` tool which is why the BitsPerSample field in the previous
  section points to that offset. (Just in case you wonder, offsets can be
  calculated "manually" by counting bytes in the code blocks.)

* StripOffsets, a three-values array of type SHORT. The image data is organized
  into three strips which start at the offsets recorded in this array.

      00a8 204c 3ff0

* StripByteCounts, a three-values array of type SHORT. This array contains the
  size (in bytes) of each strip. In this example all strips have the same size
  (8100 bytes, a little less than 8 KiB).

      1fa4 1fa4 1fa4

* XResolution and YResolution, one value of type RATIONAL each. Both horizontal
  and vertical resolution are set to 300 pixels per inch which is represented by
  the fraction 300/1 = 0x012c/0x01.

      0000012c 00000001
      0000012c 00000001

  By the way, this highlights another opportunity to violate the TIFF
  specification: If both XResolution and YResolution have the same value, then
  why not let both fields just point to the same offset, thus saving some bytes?
  Here's why:

  > No Duplicate Pointers. No data should be referenced from more than one
  > place. TIFF readers and editors are under no obligation to detect this
  > condition and handle it properly. This would not be a problem if TIFF files
  > were read-only entities, but they are not.
  >
  > (TIFF 6.0, page 26)

## Image data

The actual image data is a sequence of bytes that has to be interpreted
according to the information in the IFD above. In this example that means the
image data is uncompressed and encodes a 90 × 90 pixels image in such a way that
each pixel is represented by a three-bytes RGB triple.

The binary layout of the image data is where it gets interesting:

> Compressed or uncompressed image data can be stored almost anywhere in a TIFF
> file. TIFF also supports breaking an image into separate strips for increased
> editing flexibility and efficient I/O buffering.
>
> (TIFF 6.0, page 19)

That's why the StripOffsets, StripByteCounts and RowsPerStrip fields are needed.
They determine (in this order) where the strips are, how many bytes each strip
holds (after compression, if applicable) and how many rows of pixels each strip
represents (except for the last one which may have fewer rows). The first two
fields are arrays corresponding to the number of strips (in this example: three
strips, three offsets, three byte counts), the third field holds a single value.

The TIFF specification has some advice regarding the strip size:

> However, some readers may try to read an entire strip into memory at one time.
> If the entire image is one strip, the application may run out of memory.
> Recommendation: Set RowsPerStrip such that the size of each strip is about 8K
> bytes.
>
> (TIFF 6.0, page 27)

The 8 KiB guideline may not be just as relevant anymore as in 1992 when TIFF 6.0
was published, but this example follows it anyway. The image data is organized
into three strips of 30 rows each, adding up to the 90 pixels ImageLength.
Together with an ImageWidth of 90 pixels and three bytes per RGB pixel this
yields 90 × 30 × 3 = 8100 bytes per strip which is pretty close to the
recommended size of 8 KiB = 8192 bytes. (Why not use exactly 8 KiB? Because one
pixel is represented by three bytes, so the number of bytes in a strip will
always be a multiple of three, which 8192 is not.)

The byte patterns below also show how each strip of 8100 bytes consists of 2700
repetitions of a three-bytes RGB triple: `ff 00 00` for red, `00 ff 00` for
green and `00 00 ff` for blue.

    (ff0000){2700} # first strip, 30 red rows
    (00ff00){2700} # second strip, 30 green rows
    (0000ff){2700} # third strip, 30 blue rows

[TIFF]: https://www.adobe.io/content/dam/udp/en/open/standards/tiff/TIFF6.pdf
