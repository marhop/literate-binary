# ZIP File Example

The ZIP format specification is called APPNOTE.TXT. The current version (6.3.4
at the time of writing) can be found [here][APPNOTE]. A ZIP archive consists of
a series of records that describe the files contained in the archive. The global
structure looks like this:

  * first file
      * local file header
      * file data
  * second file
      * local file header
      * file data
  * ...
  * central directory
      * central directory header for first file
      * central directory header for second file
      * ...
      * end of central directory record

There are several additional, optional records; see [[APPNOTE] section 4.3.6]
for the complete list. Each record starts with the bytes `504b` (ASCII "PK") and
another two bytes identifying the record type. [[APPNOTE] section 4.2]

All multi-byte numeric fields use little endian byte order.

## First File

### Local File Header

The local file header holds metadata about a file contained in the ZIP archive.
This record is specified in [[APPNOTE] section 4.3.7].

 1. The local file header signature. This byte pattern identifies the record
    type.

        504b 0304

 2. The minimum supported ZIP specification version needed to extract this file.
    This is used to ensure a tool supports certain features, like version 2.0
    for Deflate compression or version 4.5 for the ZIP64 extension. Here it's
    just the default value 1.0 which is obtained by dividing 0x000a by 10.
    [[APPNOTE] section 4.4.3]

        0a00

 3. The general purpose bit flag. By setting different bits additional,
    pre-defined information about the file can be conveyed like whether it is
    encrypted or which options were applied to the compression process. Here no
    bit is set, so there's nothing special about this file. [[APPNOTE] section
    4.4.4]

        0000

 4. The compression method. A number denoting the algorithm used to compress
    this file. 0 means no compression ("stored" in the words of the ZIP
    specification); the commonly used compression method Deflate has number 8.
    [[APPNOTE] section 4.4.5]

        0000

 5. The last file modification time. This is encoded "in standard MS-DOS
    format", whatever this may be. [[APPNOTE] section 4.4.6]

        4cb7

 6. The last file modification date. Also in "standard MS-DOS format".
    [[APPNOTE] section 4.4.6]

        294d

 7. The [CRC-32] checksum of the (uncompressed) file. Useful for integrity
    checks. [[APPNOTE] section 4.4.7]

        2d3b 08af

 8. The size of the compressed file. [[APPNOTE] section 4.4.8]

        0c00 0000

 9. The size of the uncompressed file. Note that uncompressed and compressed
    size are the same in this example because the file is not compressed.
    [[APPNOTE] section 4.4.9]

        0c00 0000

10. The length of the file name. See field 12. [[APPNOTE] section 4.4.10]

        0900

11. The length of the extra field. See field 13. [[APPNOTE] section 4.4.11]

        0000

12. The file name. "hello.txt" [[APPNOTE] section 4.4.17]

        6865 6c6c 6f2e 7478 74

13. The extra field. This can be used to store a series of extensible data
    fields for different use cases like ZIP64 extended information or
    application specific data. Each extensible data field consists of a 2 bytes
    header ID, another 2 bytes that hold the data size and finally the field
    specific data. This example has no extra field. [[APPNOTE] section 4.4.28]

### File Data

The actual file data immediately follows the local file header (unless an
optional encryption header is inserted between local file header and file data).
It may be compressed using different algorithms, or not compressed at all.
Different compression methods may be used for different files. In this example,
the file data consists of the uncompressed ASCII string "hello world", followed
by a line feed.

    6865 6c6c 6f20 776f 726c 640a 

This record is specified in [[APPNOTE] section 4.3.8].

The file data may be followed by an optional data descriptor, but this is
necessary only in very special cases.

## Second File

### Local File Header

The local file header for the second file is very similar to the first.

    504b 0304                   # local file header signature
    0a00                        # version needed to extract
    0000                        # general purpose bit flag
    0000                        # compression method
    4eb7                        # last mod file time
    294d                        # last mod file date
    0db3 bd19                   # crc-32
    0e00 0000                   # compressed size
    0e00 0000                   # uncompressed size
    0b00                        # file name length
    0000                        # extra field length
    676f 6f64 6279 652e 7478 74 # file name "goodbye.txt"

### File Data

The file data consists of the ASCII string "goodbye world", followed by a line
feed.

    676f 6f64 6279 6520 776f 726c 640a

## Central Directory

The central directory is located at the end of the ZIP file. It identifies all
files contained in the archive by means of a central directory header and is
finished by the end of central directory record.

### Central Directory Header For First File

The metadata in the central directory header is largely, but not completely
redundant to the corresponding local file header.

This record is specified in [[APPNOTE] section 4.3.12].

 1. The central file header signature.

        504b 0102

 2. The ZIP specification version supported by the software used to create this
    file (0x1e = 3.0) and a number denoting the host system on which the
    external file attributes (see field 16) are compatible (0x03 = Unix).
    [[APPNOTE] section 4.4.2]

        1e03

 3. The minimum supported ZIP specification version needed to extract this file.

        0a00

 4. The general purpose bit flag.

        0000

 5. The compression method.

        0000

 6. The last file modification time.

        4cb7

 7. The last file modification date.

        294d

 8. The CRC-32 checksum of the (uncompressed) file.

        2d3b 08af

 9. The size of the compressed file.

        0c00 0000

10. The size of the uncompressed file.

        0c00 0000

11. The length of the file name.

        0900

12. The length of the extra field.

        0000

13. The length of the file comment. See field 20. [[APPNOTE] section 4.4.12]

        0000

14. The number of the disk on which this file begins. ZIP files may be split on
    multiple disks (like multiple floppies), but this example fits on one.
    [[APPNOTE] section 4.4.13]

        0000

15. The internal file attributes. By setting different bits additional,
    pre-defined information about the file can be conveyed. Most bits are not
    defined by the specification, but if the first bit is set like in this
    example (remember, little endian!) this indicates that the file is
    considered to be a text file. [[APPNOTE] section 4.4.14]

        0100

16. The external file attributes. These are host-system dependent file
    attributes. The host system is determined by field 2, which says Unix, so
    0x81a4 encodes Unix permissions that read as 100644 in octal notation.
    [[APPNOTE] section 4.4.15]

        0000 a481

17. The offset of the corresponding local file header. This is used to locate
    the file in the ZIP archive. Thus, the central directory enables random
    access to a single file without reading the whole archive. [[APPNOTE]
    section 4.4.16]

        0000 0000

18. The file name.

        6865 6c6c 6f2e 7478 74

19. The extra field. None in this example.

20. The file comment. None in this example. [[APPNOTE] section 4.4.18]

### Central Directory Header For Second File

Same as for the first file.

    504b 0102                   # central file header signature
    1e03                        # version made by
    0a00                        # version needed to extract
    0000                        # general purpose bit flag
    0000                        # compression method
    4eb7                        # last mod file time
    294d                        # last mod file date
    0db3 bd19                   # crc-32
    0e00 0000                   # compressed size
    0e00 0000                   # uncompressed size
    0b00                        # file name length
    0000                        # extra field length
    0000                        # file comment start
    0000                        # disk number start
    0100                        # internal file attributes
    0000 a481                   # external file attributes
    3300 0000                   # offset of local header
    676f 6f64 6279 652e 7478 74 # file name "goodbye.txt"

### End Of Central Directory Record

The end of central directory record is the entry point of a ZIP archive.

This record is specified in [[APPNOTE] section 4.3.16].

 1. The end of central directory signature.

        504b 0506

 2. The number of this disk. ZIP files may be split on multiple disks (like
    multiple floppies), but this example fits on one. [[APPNOTE] section 4.4.19]

        0000

 3. The number of the disk with the start of the central directory. [[APPNOTE]
    section 4.4.20]

        0000

 4. The total number of entries in the central directory on this disk.
    [[APPNOTE] section 4.4.21]

        0200

 5. The total number of entries in the central directory. That is the number of
    files in this ZIP archive. [[APPNOTE] section 4.4.22]

        0200

 6. The size of the central directory. 0x70 = 112 bytes. [[APPNOTE] section
    4.4.23]

        7000 0000

 7. The offset of the central directory. This is used to locate the start of the
    central directory. [[APPNOTE] section 4.4.24]

        6a00 0000

 8. The length of the ZIP file comment. See field 9. [[APPNOTE] section 4.4.25]

        0000

 9. The ZIP file comment. None in this example. [[APPNOTE] section 4.4.26]

[APPNOTE]: https://pkware.cachefly.net/webdocs/casestudies/APPNOTE.TXT
[CRC-32]: https://en.wikipedia.org/wiki/Cyclic_redundancy_check
