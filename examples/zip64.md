This document describes a ZIP64 file in ["literate binary"][lb] notation,
integrating handcrafted binary (expressed as hex code) with documentation
written in [Markdown][Markdown]. Two different representations can be derived
from this document:

 1. A PDF/HTML/Latex ... file, using a Markdown converter like [Pandoc][Pandoc].
 2. A binary ZIP64 file, using the [`lb` tool][lb].

[lb]: https://github.com/marhop/literate-binary
[Markdown]: https://commonmark.org/
[Pandoc]: https://pandoc.org/

# ZIP64 File Example

The ZIP64 extension of the ZIP file format is used for files bigger than 4 GB.
The basic idea is this: In a regular ZIP file, if some value does not fit into
its usual field (like a file size bigger than 2³² bytes), this field is set to a
special value (0xffffffff for 32 bit fields) and an additional record is created
that holds a corresponding field of bigger size. These records are the *ZIP64
extended information extra field* for local file headers and central directory
headers and the *ZIP64 end of central directory record* and *ZIP64 end of
central directory record locator* for the end of central directory record.

As an aside, apart from constructing it manually, the easiest way to create a
minimal ZIP64 file is piping content into the zip command.

~~~ {.nobin}
$ echo 'hello world' | zip zip64.zip -
~~~

This enables the ZIP64 extension even if it is not required by the size of the
zipped content. From the man page (Zip 3.0 (July 5th 2008), by Info-ZIP on
Debian 9): "ZIP64 is also used for archives streamed from standard input as the
size of such archives are not known in advance"

## Local File Header

Basically the same as a local file header in a regular ZIP file, but with two
important details. First, the minimum supported ZIP specification version needed
to extract this file is 0x2d = 4.5 because this version introduced the ZIP64
extension. [[APPNOTE] section 4.4.3.2] Second, the compressed size and
uncompressed size fields may be set to 0xffffffff if the respective value does
not fit into 32 bits: "If an archive is in ZIP64 format and the value in this
field is 0xFFFFFFFF, the size will be in the corresponding 8 byte ZIP64 extended
information extra field." [[APPNOTE] sections 4.4.8 and 4.4.9]

    504b 0304   # local file header signature
    2d00        # version needed to extract
    0000        # general purpose bit flag
    0000        # compresion method
    4eb7        # last mod file time
    294d        # last mod file date
    2d3b 08af   # crc-32
    ffff ffff   # compressed size - too big for 32 bits
    ffff ffff   # uncompressed size - too big for 32 bits
    0900        # file name length
    1400        # extra field length
    "hello.txt" # file name

The ZIP64 extended information extra field contains the compressed and
uncompressed sizes as 64 bit numbers. It should be created if one of these
values does not fit into 32 bits. [[APPNOTE] section 4.5.3]

    0100                # header ID "ZIP64 extended information extra field"
    1000                # data size
    0c00 0000 0000 0000 # uncompressed size
    0c00 0000 0000 0000 # compressed size

Obviously, a real world ZIP file would not need the ZIP64 extensions used in
this example because none of the size values actually requires 64 bits and so
they could just as well be written to the regular local file header fields.
Well, it's an example.

## File Data

This is just the same as in a regular ZIP file. In this example, the file data
consists of the uncompressed ASCII string "hello world", followed by a line
feed.

    "hello world" 0a

In a real world ZIP64 file there would of course be much more data (more than 4
GB, to be precise) and it would most likely be compressed. Compression would
possibly reduce it to less than 4 GB again, but because of the uncompressed size
field in the local file header and central directory header the ZIP64 extension
would still be necessary.

## Central Directory Header

Basically the same as a central directory header in a regular ZIP file. As in
the local file header, some fields need special attention because of the ZIP64
extension.

    504b 0102   # central file header signature
    1e03        # version made by
    2d00        # version needed to extract
    0000        # general purpose bit flag
    0000        # compression method
    4eb7        # last mod file time
    294d        # last mod file date
    2d3b 08af   # crc-32
    ffff ffff   # compressed size - too big for 32 bits
    ffff ffff   # uncompressed size - too big for 32 bits
    0900        # file name length
    2000        # extra field length
    0000        # file comment length
    ffff        # disk number start - too big for 32 bits
    0100        # internal file attributes
    0000 a481   # external file attributes
    ffff ffff   # offset of local header - too big for 32 bits
    "hello.txt" # file name

As in the local file header, there is a ZIP64 extended information extra field.
In addition to the compressed size and uncompressed size values the extra field
in the central directory header may also include larger variants of the number
of the disk on which the file starts and of the offset of the corresponding
local file header. Just like the size values none of those actually needs this
extension, so there is no reason to include them in the extra field -- except to
serve as an example.

    0100                # header ID "ZIP64 extended information extra field"
    1c00                # data size
    0c00 0000 0000 0000 # uncompressed size
    0c00 0000 0000 0000 # compressed size
    0000 0000 0000 0000 # offset of local header
    0000 0000           # disk number start

## ZIP64 End Of Central Directory Record

The ZIP64 end of central directory record (and consequently, the ZIP64 end of
central directory locator in the next section) should be created if one of the
fields in the "regular" end of central directory record (see below) is too small
for its data. [[APPNOTE] section 4.4.1.4] This record contains fields from the
end of central directory record, but in a 64 bit variant. Thus, it serves the
same purpose as the ZIP64 extended information extra field does in the the local
file header and central directory header. This record is specified in [[APPNOTE]
section 4.3.14].

 1. The ZIP64 end of central directory signature.

        504b 0606

 2. The size of the ZIP64 end of central directory record, not counting the
    first two fields. [[APPNOTE] section 4.3.14.1]

        2c00 0000 0000 0000

 3. The ZIP specification version supported by the software used to create this
    file.

        1e03

 4. The minimum supported ZIP specification version needed to extract this file.

        2d00

 5. The number of this disk.

        0000 0000

 6. The number of the disk with the start of the central directory.

        0000 0000

 7. The total number of entries in the central directory on this disk.

        0100 0000 0000 0000

 8. The total number of entries in the central directory.

        0100 0000 0000 0000

 9. The size of the central directory.

        5700 0000 0000 0000

10. The offset of the central directory.

        4700 0000 0000 0000

11. The ZIP64 extensible data sector. This can be used to store additional
    information, similar to the extra field in the local file header. However,
    it is reserved for use by the creator of the ZIP specification, PKWARE. None
    in this example. [[APPNOTE] section 4.4.27]

## ZIP64 End Of Central Directory Locator

The ZIP64 end of central directory locator is used to locate the ZIP64 end of
central directory record. This record is specified in [[APPNOTE] section
4.3.15].

 1. The ZIP64 end of central directory locator signature.

        504b 0607

 2. The number of the disk with the start of the ZIP64 end of central directory.

        0000 0000

 3. The offset of the ZIP64 end of central directory record.

        9e00 0000 0000 0000

 4. The total number of disks.

        0100 0000

## End Of Central Directory Record

Basically the same as in a regular ZIP file, but similar to the local file
headers and central directory headers several fields may be set to 0xffff or
0xffffffff, meaning the larger variants in the ZIP64 end of central directory
record should be used.

    504b 0506 # end of central directory signature
    ffff      # number of this disk
    ffff      # number of the disk with the start of the central directory
    ffff      # total number of entries in the central directory on this disk
    ffff      # total number of entries in the central directory
    ffff ffff # size of the central directory
    ffff ffff # offset of start of central directory
    0000      # ZIP file comment length

Again, none of these fields actually needs the ZIP64 extension. Since the values
would just as well fit into the central directory record, some tools in fact
write them both to the ZIP64 end of central directory record and to the
"regular" end of central directory record instead of putting 0xffff or
0xffffffff here.

[APPNOTE]: https://pkware.cachefly.net/webdocs/casestudies/APPNOTE.TXT
