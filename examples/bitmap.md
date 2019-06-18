This file describes a simple BMP, or Bitmap Image File in hexadecimal notation.
It is an example for a ["literate binary"][lb] file that integrates handcrafted
binary and documentation. Two different representations can be derived from this
file (see end of file for usage examples):

 1. A PDF/HTML/Latex ... file, using a Markdown converter like Pandoc.
 2. A binary BMP file, using [`lb`][lb]. This takes the hex code from all
    Markdown code blocks and transforms it into binary that can be opened in any
    suitable image viewer.

# BMP File Example

## File Headers

A BMP file starts with two headers, the BMP, or bitmap header and the DIB, or
device independent bitmap header (sometimes called bitmap information header).
Numeric values are encoded in little endian byte order.

 1. The first header starts with the magic number `424d` (ASCII "BM") that helps
    to identify the file type, followed by the total size of the file in bytes,
    four bytes reserved for application specific use, and finally the offset of
    the actual image data in the file.

        424d     # magic number
        86580200 # file size
        00000000 # application specific (unused)
        36000000 # offset of image data

    Note that the preceding code block will be part of a binary file created by
    `lb`. Whitespace including linebreaks and comments will be ignored.

 2. Information about the image encoded in this file, such as dimensions and
    color information, can be found in the second header.

        28000000 # header size
        e2000000 # image width
        e2000000 # image height
        0100     # number of color planes
        1800     # number of bits per pixel
        00000000 # compression type of image data (0 = uncompressed)
        50580200 # size of image data (including padding)
        130b0000 # horizontal print resolution, pixels/metre
        130b0000 # vertical print resolution, pixels/metre
        00000000 # number of colors in palette
        00000000 # important colors (0 = all)

    This code block will also be part of a binary file created by `lb`.

Explaining the meaning of all fields in the second header is beyond the scope of
this example because it is not meant to be an introduction to the BMP format but
merely to show how a literate binary file looks like. Some hints, however:

  * Width and height are given in pixels, so this is a 226 * 226 pixels image
    (0xe2 = 226).
  * Most of the color stuff is either always the same (like number of color
    planes) or irrelevant in the present case (like number of colors in the
    palette because this file has no palette).
  * The number of bits per pixel (0x18 = 24) and the compression type
    (uncompressed) indicate what the image data in the next section should look
    like: three bytes per pixel, without any compression.

## Image Data

The actual image data consists of an array of pixels with each pixel being
represented by a three byte blue/green/red value -- think RGB in little endian.
Pixels are in bottom left to top right order. They are interspersed with
padding, usually NULL bytes, to achieve a four byte alignment for each line of
pixels. (That means the number of bytes that encode one line of pixels should be
divisible by four -- if it's not, NULL bytes are appended until it is.)

Encoding a 226 * 226 pixels image in this way requires 226 lines of 226 pixels.
Each line is represented by 226 * 3 + 2 = 680 bytes (two bytes padding because
226 * 3 = 678 is not divisible by four), adding up to a total of 226 * 680 =
153680 bytes. Both writing and reading such a large amount of bytes as plain hex
values would be rather inconvenient. Luckily though, literate binary supports a
macro syntax that facilitates the compact description of repetitive or random
byte patterns. Examples of this macro syntax can be seen at work in the
following code blocks.

    # one red square + one square of random black/grey/white pixels
    ((0000ff){113} (000000|808080|ffffff){113} 0000){113}

    # one square of shades of green + one square of randomly colored pixels
    ((00(80-ff)00){113} (...){113} 0000){113}

These two code blocks will also be part of a binary file created by `lb`, and
special care will be taken of any hex macros during the conversion:

  * The repetition macro `(0000ff){113}` denotes 113 repetitions of the byte
    sequence `0000ff`, resulting in 113 red pixels. Macros may be nested, making
    it easy e.g. to create 113 lines of 113 red and 113 green pixels each,
    resulting in a red and a green square next to each other.
  * The alternative macro `(000000|808080|ffffff)` denotes a random choice of
    one of the given options, resulting in either a black or a grey or a white
    pixel. Naturally, "random" means that this macro will produce a different
    pixel each time it is processed by `lb`, so combining this with a repetition
    macro like in `(000000|808080|ffffff){113}` will result in 113 random black
    or grey or white pixels.
  * The range macro `(80-ff)` behaves like the alternative `(80|81|...|fe|ff)`,
    so the expression `00(80-ff)00` results in a pixel colored in a random shade
    of green.
  * The special range macro `.` (a single dot) is an abbreviation for the range
    `(00-ff)` which denotes one random byte, so the expression `...` results in
    a randomly colored pixel.

# Usage Example

The following two commands convert this Markdown file to PDF and to BMP:

~~~ {.nobin}
$ pandoc bitmap.md --output documentation.pdf
$ lb bitmap.md --output binary.bmp
~~~

Note that the preceding code block will *not* be part of a binary file created
by `lb`. It will be ignored because of the `.nobin` class.

[lb]: https://github.com/marhop/literate-binary
