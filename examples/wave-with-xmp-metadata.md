This document describes a WAVE file with embedded XMP metadata in ["literate
binary"][lb] notation, integrating handcrafted binary (expressed as hex code)
and documentation written in Markdown syntax. Two different representations can
be derived from this document:

 1. A PDF/HTML/Latex ... file, using a Markdown converter like [Pandoc][Pandoc].
 2. A binary WAVE file, using the [`lb` tool][lb].

[lb]: https://github.com/marhop/literate-binary
[Pandoc]: https://pandoc.org/

# WAVE file example with XMP metadata

There are several ways in which metadata can be embedded in a WAVE file. The
original [WAVE specification][RIFF] already defined a special chunk for a fixed
set of metadata items (the INFO LIST chunk), others were added with the
[Broadcast WAVE][BWF] extension. [XMP (Extensible Metadata Platform)][XMP]
metadata is based on a generic data model that allows to use any set of metadata
properties. The XMP specification contains pre-defined properties like the
well-known Dublin Core terms, but custom properties may be used as well. XMP
metadata can be embedded in several file formats, amongst them WAVE.

This is not an introduction to the WAVE file format itself but an example for
XMP metadata embedded in a WAVE file. Basic knowledge of the WAVE file format is
assumed.

[RIFF]: http://www-mmsp.ece.mcgill.ca/Documents/AudioFormats/WAVE/Docs/riffmci.pdf
[BWF]: https://tech.ebu.ch/docs/tech/tech3285.pdf
[XMP]: https://www.adobe.com/devnet/xmp.html

## RIFF, Format and Data chunks

Each WAVE file has at least three sections (called chunks); the RIFF chunk, the
format chunk and the data chunk. Together they make a complete WAVE file.

The RIFF chunk identifies the file format (RIFF/WAVE) and the size of the
remaining data (including the string "WAVE" but excluding the chunk ID "RIFF"
and the size field itself -- should be equal to file size minus 8).

    "RIFF" d6021000 "WAVE"

The format chunk holds technical metadata necessary to decode the byte stream in
the data chunk below. This includes information about the encoding method (or
WAVE format category), the number of channels, the sampling rate, the average
number of bytes per second (number of channels × sampling rate × bytes per
sample), the block alignment (number of channels × bytes per sample), and
finally the bit depth (or bits per sample).

    "fmt "   # chunk ID
    10000000 # chunk size
    0100     # WAVE format category, PCM
    0200     # number of channels, 2
    44ac0000 # sampling rate, 44100 Hz
    10b10200 # average number of bytes per second, 176400
    0400     # block alignment, 4
    1000     # bits per sample, 16

The data chunk contains the actual audio data. In this example it holds just
random bytes, leading to random noise.

    "data" 00001000 .{1M}

Note the special syntax used here: The single dot `.` represents one random byte
and the quantifier `{1M}` represents 1 × 2²⁰ repetitions, so `.{1M}` represents
1 MiB of random data.

## XMP chunk

Since RIFF-based file formats like WAVE can easily be extended by introducing
additional chunks it is pretty straightforward to just define a new chunk for
XMP metadata. That way, the metadata can be embedded without interfering with
the rest of the WAVE file structure, and any program that does not need or
understand the XMP metadata can simply skip the XMP chunk. From the *[Adobe XMP
Specification][XMP] Part 3, Storage in Files*, page 24:

> The XMP in AVI and WAV is in a chunk with the ID "_PMX", encoded as UTF-8.
> Note that the ID is backwards, due to a bug in the initial implementation
> concerning processor byte order. The XMP chunk is immediately within the
> outermost "RIFF" chunk. There is no ordering constraint of the XMP relative to
> other chunks.

(Isn't it soothing that other people are struggling with endianness as well?)

    "_PMX" 7a020000

    '<rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
        xmlns:xmp="http://ns.adobe.com/xap/1.0/"
        xmlns:dc="http://purl.org/dc/elements/1.1/">
        <rdf:Description rdf:about="">
            <xmp:MetadataDate>1970-01-01</xmp:MetadataDate>
            <dc:subject>
                <rdf:Bag>
                    <rdf:li>WAVE</rdf:li>
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

The XMP metadata inside the chunk is serialized as [RDF/XML][RDF] as prescribed
by the *[Adobe XMP Specification][XMP] Part 1, Data model, Serialization, and
Core Properties*. As usual with XML this is a little verbose; the metadata
properties describing the WAVE file are surrounded by an outer rdf:RDF and an
inner rdf:Description element. In this example there are three properties:
xmp:MetadataDate with a simple (date-formatted) string value, dc:subject with a
set of keywords, and dc:creator with a one-element sequence of strings. All
three properties are defined in the XMP specification, but properties from
other, even custom namespaces could be used just as well.

Note that the chunk size includes the bytes consumed by whitespace, particularly
indenting and newline characters. In a real-world setting one would probably
minimize the use of such whitespace.

[RDF]: https://en.wikipedia.org/wiki/RDF/XML

## INFO LIST chunk

XMP can be embedded in a lot of different file formats, but many of these
formats also support native metadata of their own. To preserve consistency
between different approaches to metadata the *[Adobe XMP Specification][XMP]
Part 3, Storage in Files* recommends to reconcile XMP with native metadata. This
involves keeping certain metadata properties in sync with their native
counterparts. The relevant properties and their mappings to native metadata
items are listed in the XMP specification for several file formats including
WAVE.

Considering the metadata used in the above XMP chunk the dc:subject XMP property
has to be mapped to the IKEY entry in an INFO LIST chunk, while no mappings are
defined for the xmp:MetadataDate and dc:creator properties (see *[Adobe XMP
Specification][XMP] Part 3, Storage in Files*, page 56).

The INFO LIST chunk is a special chunk for native WAVE metadata that is defined
in the [WAVE specification][RIFF], page 23. Similar to the RIFF chunk, its chunk
data consists of the list type "INFO" followed by a sequence of nested chunks
that constitute the list items. The chunks allowed in an INFO LIST chunk are
defined in the WAVE specification. In this example, only one is required to
account for the metadata mapping: an IKEY chunk which contains a NULL-terminated
string consisting of semicolon-separated keywords.

    "LIST" 28000000 "INFO"
    "IKEY" 1b000000 "WAVE; XMP; Literate Binary" 00 00

Note that there are *two* trailing NULL bytes at the end of the IKEY chunk. They
are there for two different reasons: The first terminates the string content of
the IKEY chunk (see [WAVE specification][RIFF], page 23). The second is a pad
byte that has to be appended to the actual chunk data whenever the chunk size is
odd (see [WAVE specification][RIFF], page 11).
