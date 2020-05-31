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

This example file conforms to the [TIFF 6.0 specification][TIFF]. It contains a
palette-color image. In a palette-color image each pixel is represented by an
index into an RGB-lookup table, called color map or palette.

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

Several fields are required for a baseline TIFF palette-color image; some of
them have default values though so they do not have to be included if the
default value is appropriate.

* ImageWidth, tag 256 = 0x0100, one value of type SHORT (0x03, could also be
  LONG). The width of the image in pixels; 512 = 0x0200 columns. See how the
  16-bit SHORT is left-justified within the value field?

      0100 0003 00000001 02000000

* ImageLength, tag 257 = 0x0101, one value of type SHORT (could also be LONG).
  The length (height) of the image in pixels; 256 = 0x0100 rows.

      0101 0003 00000001 01000000

* BitsPerSample, tag 258 = 0x0102, one value of type SHORT. The number of bits
  per sample; one byte per pixel, allowing 256 distinct entries (i.e., RGB
  colors) to be indexed in the palette.

      0102 0003 00000001 00080000

* Compression, tag 259 = 0x0103, one value of type SHORT. The default is 1 (no
  compression). Since the image data in this file is in fact uncompressed it is
  not necessary (but wouldn't hurt either) to include this field. It would look
  like this if included: `0103 0003 00000001 00010000`.

* PhotometricInterpretation, tag 262 = 0x0106, one value of type SHORT. The
  color space (or rather color model in case this makes a difference, like RGB)
  of the image data; 3 = palette color.

      0106 0003 00000001 00030000

* StripOffsets, tag 273 = 0x0111, a 16-values array of type LONG (0x04, could
  also be SHORT). The offsets of the strips containing the actual image data;
  the meaning of strips is explained in the section on image data below. Note
  that 16 LONGs do not fit into four bytes so a pointer to the array is
  inserted instead. It points to offset 0x86 which is where the 16 LONGs are
  stored (see next section). Depending on the number of strips used in a file
  there may be less or much more than 16 offset values.

      0111 0004 00000010 00000086

* RowsPerStrip, tag 278 = 0x0116, one value of type SHORT (could also be LONG).
  The number of rows (lines of pixels) stored in each strip; 16 = 0x10 rows per
  strip. Depending on the total number of rows the last strip may have fewer
  rows.

      0116 0003 00000001 00100000

* StripByteCounts, tag 279 = 0x0117, a 16-values array of type SHORT (could also
  be LONG). The number of bytes in each strip of image data; again, this is a
  pointer to the actual array value at offset 0xc6.

      0117 0003 00000010 000000c6

* XResolution, tag 282 = 0x011a, one value of type RATIONAL (0x05). The number
  of pixels per ResolutionUnit (see below) in the ImageWidth (typically,
  horizontal) direction. A RATIONAL number consists of the two components of a
  fraction, numerator and denominator, each represented by a four-byte LONG.
  Obviously, that does not fit into four bytes, so a pointer to the actual value
  at offset 0xe6 is used.

      011a 0005 00000001 000000e6

* YResolution, tag 283 = 0x011b, one value of type RATIONAL. The number of
  pixels per ResolutionUnit in the ImageLength (typically, vertical) direction,
  just like XResolution; a pointer to offset 0xee.

      011b 0005 00000001 000000ee

* ResolutionUnit, tag 296 = 0x0128, one value of type SHORT. The default is 2
  (inch) which is used in this example, so like the Compression field above this
  is not needed. It would look like this: `0128 0003 00000001 00020000`.

* ColorMap, tag 320 = 0x0140, 256 × 3 = 768 values of type SHORT. A lookup
  table containing RGB triples, also known as color map or palette; this is a
  pointer to offset 0xf6.

      0140 0003 00000300 000000f6

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

* StripOffsets, a 16-values array of type LONG. The image data is organized
  into 16 strips which start at the offsets recorded in this array.

      000006f6 000026f6 000046f6 000066f6
      000086f6 0000a6f6 0000c6f6 0000e6f6
      000106f6 000126f6 000146f6 000166f6
      000186f6 0001a6f6 0001c6f6 0001e6f6

  Note that this code block itself will start at offset 0x86 in a binary file
  created by the `lb` tool which is why the StripOffsets field in the previous
  section points to that offset. (Just in case you wonder, offsets can be
  calculated "manually" by counting bytes in the code blocks.)

* StripByteCounts, a 16-values array of type SHORT. This array contains the size
  (in bytes) of each strip. In this example all strips have the same size (8
  KiB).

      2000 2000 2000 2000 2000 2000 2000 2000
      2000 2000 2000 2000 2000 2000 2000 2000

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

* ColorMap, a lookup table containing RGB triples. According to BitsPerSample
  this table holds 256 entries. Each entry consists of three SHORT values (Red,
  Green, Blue), adding up to 256 × 3 × 2 = 1536 bytes. Note that the RGB table
  is not ordered line by line but column by column:

  > In a TIFF ColorMap, all the Red values come first, followed by the Green
  > values, then the Blue values. In the ColorMap, black is represented by 0,0,0
  > and white is represented by 65535, 65535, 65535.
  >
  > (TIFF 6.0, page 23)

  So in this example the first entry at index 0 is (0, 0, 0), the second is (0,
  1, 0) and so on.

  The palette is crucial for the rendering of the image. In this example all Red
  and Blue components are set to 0 so the image must contain nothing but shades
  of green (plus black). Modifying the values in the palette (like switching the
  Red and Green columns) while leaving the image data below unchanged could
  result in wildly differing colors.

      # first column, Red components
      0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000
      0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000
      0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000
      0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000
      0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000
      0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000
      0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000
      0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000
      0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000
      0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000
      0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000
      0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000
      0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000
      0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000
      0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000
      0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000
      0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000
      0000

      # second column, Green components
      0000 0001 0002 0003 0004 0005 0006 0007 0008 0009 000a 000b 000c 000d 000e
      000f 0010 0011 0012 0013 0014 0015 0016 0017 0018 0019 001a 001b 001c 001d
      001e 001f 0020 0021 0022 0023 0024 0025 0026 0027 0028 0029 002a 002b 002c
      002d 002e 002f 0030 0031 0032 0033 0034 0035 0036 0037 0038 0039 003a 003b
      003c 003d 003e 003f 0040 0041 0042 0043 0044 0045 0046 0047 0048 0049 004a
      004b 004c 004d 004e 004f 0050 0051 0052 0053 0054 0055 0056 0057 0058 0059
      005a 005b 005c 005d 005e 005f 0060 0061 0062 0063 0064 0065 0066 0067 0068
      0069 006a 006b 006c 006d 006e 006f 0070 0071 0072 0073 0074 0075 0076 0077
      0078 0079 007a 007b 007c 007d 007e 007f 0080 0081 0082 0083 0084 0085 0086
      0087 0088 0089 008a 008b 008c 008d 008e 008f 0090 0091 0092 0093 0094 0095
      0096 0097 0098 0099 009a 009b 009c 009d 009e 009f 00a0 00a1 00a2 00a3 00a4
      00a5 00a6 00a7 00a8 00a9 00aa 00ab 00ac 00ad 00ae 00af 00b0 00b1 00b2 00b3
      00b4 00b5 00b6 00b7 00b8 00b9 00ba 00bb 00bc 00bd 00be 00bf 00c0 00c1 00c2
      00c3 00c4 00c5 00c6 00c7 00c8 00c9 00ca 00cb 00cc 00cd 00ce 00cf 00d0 00d1
      00d2 00d3 00d4 00d5 00d6 00d7 00d8 00d9 00da 00db 00dc 00dd 00de 00df 00e0
      00e1 00e2 00e3 00e4 00e5 00e6 00e7 00e8 00e9 00ea 00eb 00ec 00ed 00ee 00ef
      00f0 00f1 00f2 00f3 00f4 00f5 00f6 00f7 00f8 00f9 00fa 00fb 00fc 00fd 00fe
      00ff

      # third column, Blue components
      0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000
      0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000
      0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000
      0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000
      0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000
      0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000
      0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000
      0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000
      0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000
      0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000
      0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000
      0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000
      0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000
      0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000
      0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000
      0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000
      0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000
      0000

## Image data

The actual image data is a sequence of bytes that has to be interpreted
according to the information in the IFD above. In this example that means the
image data is uncompressed and encodes a 512 × 256 pixels image in such a way
that each byte carries color information for one pixel in from of an index into
the palette (ColorMap).

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
fields are arrays corresponding to the number of strips (in this example: 16
strips, 16 offsets, 16 byte counts), the third field holds a single value.

The TIFF specification has some advice regarding the strip size:

> However, some readers may try to read an entire strip into memory at one time.
> If the entire image is one strip, the application may run out of memory.
> Recommendation: Set RowsPerStrip such that the size of each strip is about 8K
> bytes.
>
> (TIFF 6.0, page 27)

The 8 KiB guideline may not be just as relevant anymore as in 1992 when TIFF 6.0
was published, but this example follows it anyway. The image data is organized
into 16 strips of 16 rows each, adding up to the 256 pixels ImageLength.
Together with an ImageWidth of 512 pixels and one byte per pixel this yields 512
× 16 = 8192 bytes = 8 KiB for each strip.

The byte patterns below also show how each of the 256 rows in the image is
colored in another shade of green by referencing another palette entry, from
index 0 to index 255.

    # strip 1
    (00){512} (01){512} (02){512} (03){512}
    (04){512} (05){512} (06){512} (07){512}
    (08){512} (09){512} (0a){512} (0b){512}
    (0c){512} (0d){512} (0e){512} (0f){512}

    # strip 2
    (10){512} (11){512} (12){512} (13){512}
    (14){512} (15){512} (16){512} (17){512}
    (18){512} (19){512} (1a){512} (1b){512}
    (1c){512} (1d){512} (1e){512} (1f){512}

    # strip 3
    (20){512} (21){512} (22){512} (23){512}
    (24){512} (25){512} (26){512} (27){512}
    (28){512} (29){512} (2a){512} (2b){512}
    (2c){512} (2d){512} (2e){512} (2f){512}

    # strip 4
    (30){512} (31){512} (32){512} (33){512}
    (34){512} (35){512} (36){512} (37){512}
    (38){512} (39){512} (3a){512} (3b){512}
    (3c){512} (3d){512} (3e){512} (3f){512}

    # strip 5
    (40){512} (41){512} (42){512} (43){512}
    (44){512} (45){512} (46){512} (47){512}
    (48){512} (49){512} (4a){512} (4b){512}
    (4c){512} (4d){512} (4e){512} (4f){512}

    # strip 6
    (50){512} (51){512} (52){512} (53){512}
    (54){512} (55){512} (56){512} (57){512}
    (58){512} (59){512} (5a){512} (5b){512}
    (5c){512} (5d){512} (5e){512} (5f){512}

    # strip 7
    (60){512} (61){512} (62){512} (63){512}
    (64){512} (65){512} (66){512} (67){512}
    (68){512} (69){512} (6a){512} (6b){512}
    (6c){512} (6d){512} (6e){512} (6f){512}

    # strip 8
    (70){512} (71){512} (72){512} (73){512}
    (74){512} (75){512} (76){512} (77){512}
    (78){512} (79){512} (7a){512} (7b){512}
    (7c){512} (7d){512} (7e){512} (7f){512}

    # strip 9
    (80){512} (81){512} (82){512} (83){512}
    (84){512} (85){512} (86){512} (87){512}
    (88){512} (89){512} (8a){512} (8b){512}
    (8c){512} (8d){512} (8e){512} (8f){512}

    # strip 10
    (90){512} (91){512} (92){512} (93){512}
    (94){512} (95){512} (96){512} (97){512}
    (98){512} (99){512} (9a){512} (9b){512}
    (9c){512} (9d){512} (9e){512} (9f){512}

    # strip 11
    (a0){512} (a1){512} (a2){512} (a3){512}
    (a4){512} (a5){512} (a6){512} (a7){512}
    (a8){512} (a9){512} (aa){512} (ab){512}
    (ac){512} (ad){512} (ae){512} (af){512}

    # strip 12
    (b0){512} (b1){512} (b2){512} (b3){512}
    (b4){512} (b5){512} (b6){512} (b7){512}
    (b8){512} (b9){512} (ba){512} (bb){512}
    (bc){512} (bd){512} (be){512} (bf){512}

    # strip 13
    (c0){512} (c1){512} (c2){512} (c3){512}
    (c4){512} (c5){512} (c6){512} (c7){512}
    (c8){512} (c9){512} (ca){512} (cb){512}
    (cc){512} (cd){512} (ce){512} (cf){512}

    # strip 14
    (d0){512} (d1){512} (d2){512} (d3){512}
    (d4){512} (d5){512} (d6){512} (d7){512}
    (d8){512} (d9){512} (da){512} (db){512}
    (dc){512} (dd){512} (de){512} (df){512}

    # strip 15
    (e0){512} (e1){512} (e2){512} (e3){512}
    (e4){512} (e5){512} (e6){512} (e7){512}
    (e8){512} (e9){512} (ea){512} (eb){512}
    (ec){512} (ed){512} (ee){512} (ef){512}

    # strip 16
    (f0){512} (f1){512} (f2){512} (f3){512}
    (f4){512} (f5){512} (f6){512} (f7){512}
    (f8){512} (f9){512} (fa){512} (fb){512}
    (fc){512} (fd){512} (fe){512} (ff){512}

[TIFF]: https://www.adobe.io/content/dam/udp/en/open/standards/tiff/TIFF6.pdf
