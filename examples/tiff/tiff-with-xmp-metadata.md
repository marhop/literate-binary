This document describes a TIFF file in ["literate binary"][lb] notation,
integrating handcrafted binary (expressed as hex code) with documentation
written in [Markdown][Markdown]. Two different representations can be derived
from this document:

1. A PDF/HTML/Latex ... file, using a Markdown converter like [Pandoc][Pandoc].
2. A binary TIFF file, using the [`lb` tool][lb].

[lb]: https://github.com/marhop/literate-binary
[Markdown]: https://commonmark.org/
[Pandoc]: https://pandoc.org/

# TIFF File Example With XMP Metadata

This example file conforms to the [TIFF 6.0 specification][TIFF]. It contains an
RGB full-color image with added XMP metadata. [XMP (Extensible Metadata
Platform)][XMP] metadata is based on a generic data model that allows to use any
set of metadata properties. The XMP specification contains pre-defined
properties like the well-known Dublin Core terms, but custom properties may be
used as well. XMP metadata can be embedded in several file formats, amongst them
TIFF.

This is not an introduction to the TIFF file format itself but an example for
XMP metadata embedded in a TIFF file. Basic knowledge of the TIFF file format is
assumed.

## Image File Header

Byte order (big endian) + number 42 + offset of first IFD.

    "MM" 002a 00000008

## Image File Directory

Number of directory entries.

    000b

Sequence of 12-byte IFD entries. Structure of each entry: tag + type + count +
value/pointer.

    0100 0003 00000001 005a0000 # ImageWidth, 90 pixels
    0101 0003 00000001 005a0000 # ImageLength, 90 pixels
    0102 0003 00000003 00000092 # BitsPerSample, pointer
    0106 0003 00000001 00020000 # PhotometricInterpretation, RGB
    0111 0003 00000003 00000098 # StripOffsets, pointer
    0115 0003 00000001 00030000 # SamplesPerPixel, 3 components
    0116 0003 00000001 001e0000 # RowsPerStrip, 30
    0117 0003 00000003 0000009e # StripByteCounts, pointer
    011a 0005 00000001 000000a4 # XResolution, pointer
    011b 0005 00000001 000000ac # YResolution, pointer

In addition to the usual IFD entries an entry is included that points to the XMP
metadata. According to the *[Adobe XMP Specification][XMP] Part 3, Storage in
Files*, page 19, this entry is denoted by tag 700 = 0x02bc and has a field type
of either UNDEFINED (7) or BYTE (1). It points to the offset of the XMP packet
which may be located pretty much anywhere in the file; in this example it can be
found right after the image data, at the very end of the file. Note that the
count value includes the bytes consumed by whitespace in the XML data,
particularly indenting and newline characters. In a real-world setting one would
probably minimize the use of such whitespace.

    02bc 0007 00000228 00005fa0 # XMP, pointer

Pointer to next IFD or NULL.

    00000000

### Values That Don't Fit Into Four Bytes

Referenced from IFD. Note that the XMP metadata could just as well be put here.

    0008 0008 0008    # BitsPerSample, one byte per RGB component
    00b4 2058 3ffc    # StripOffsets, pointers
    1fa4 1fa4 1fa4    # StripByteCounts, 3 × 8100 bytes
    0000012c 00000001 # XResolution, 300/1
    0000012c 00000001 # YResolution, 300/1

## Image Data

Just some red/green/blue dummy image data.

    (ff0000){2700} # first strip, 30 red rows
    (00ff00){2700} # second strip, 30 green rows
    (0000ff){2700} # third strip, 30 blue rows

## XMP Metadata

The XMP metadata is serialized as [RDF/XML][RDF] as prescribed by the *[Adobe
XMP Specification][XMP] Part 1, Data model, Serialization, and Core Properties*.
As usual with XML this is a little verbose; the metadata properties describing
the TIFF file are surrounded by an outer `rdf:RDF` and an inner
`rdf:Description` element. In this example there are three properties:
`xmp:MetadataDate` with a simple (date-formatted) string value, `dc:subject`
with a set of keywords, and `dc:creator` with a one-element sequence of strings.
All three properties are defined in the XMP specification, but properties from
other, even custom namespaces could be used just as well.

    '<rdf:RDF
      xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
      xmlns:xmp="http://ns.adobe.com/xap/1.0/"
      xmlns:dc="http://purl.org/dc/elements/1.1/">
      <rdf:Description rdf:about="">
        <xmp:MetadataDate>2025-02-11</xmp:MetadataDate>
        <dc:subject>
          <rdf:Bag>
            <rdf:li>TIFF</rdf:li>
            <rdf:li>XMP</rdf:li>
            <rdf:li>Literate Binary</rdf:li>
          </rdf:Bag>
        </dc:subject>
        <dc:creator>
          <rdf:Seq>
            <rdf:li>Martin Hoppenheit</rdf:li>
          </rdf:Seq>
        </dc:creator>
      </rdf:Description>
    </rdf:RDF>'

## Note: Reconciliation With Native Metadata

XMP can be embedded in a lot of different file formats, but many of these
formats also support native metadata of their own. To preserve consistency
between different approaches to metadata the *[Adobe XMP Specification][XMP]
Part 3, Storage in Files* recommends to reconcile XMP with native metadata. This
involves keeping certain metadata properties in sync with their native
counterparts. For TIFF, the relevant native metadata are TIFF metadata tags,
Exif metadata, IPTC metadata, and metadata in Photoshop Image Resources.

However, according to the *[Metadata Working Group Guidelines for Handling Image
Metadata, version 2.0][MWG]* it is acceptable (or even recommended) to write
only XMP metadata in newly created files; see page 25 for a statement regarding
Exif, page 28 for IPTC-IIM, and page 23 for IPTC-Core. In this spirit,
reconciliation with native metadata seems to be required only when updating
files where such metadata is already present, or when backwards compatibility is
important. Since including native metadata quickly makes things complicated, in
this example file – which has been made from scratch anyway – metadata has been
restricted to XMP only.

If reconciliation with native metadata is important these pointers might be
helpful (see *[Adobe XMP Specification][XMP] Part 3, Storage in Files*, page
63):

- TIFF metadata tags are stored as IFD entries.
- IPTC metadata items are stored in a sequence of custom data structures called
  DataSets which is referenced from IFD0 by means of tag 33723 = 0x83bb.
- Photoshop Image Resources (PSIR) may contain duplicate IPTC metadata, again in
  a custom data structure which is referenced from IFD0 via tag 34377 = 0x8649.
- Exif metadata tags are stored as entries in a subsidiary IFD which is
  referenced from IFD0 using tag 34665 = 0x8769.

[TIFF]: https://archive.org/details/TIFF6
[XMP]: https://www.adobe.com/devnet/xmp.html
[RDF]: https://en.wikipedia.org/wiki/RDF/XML
[MWG]: https://web.archive.org/web/20120131102845/http://www.metadataworkinggroup.org/pdf/mwg_guidance.pdf
