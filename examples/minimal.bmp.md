This file describes a simple BMP, or Bitmap Image File in hexadecimal notation.
It is an example for a ["literate binary"][lb] file that integrates handcrafted
binary and documentation. Two different representations can be derived from this
file:

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
    eight bytes reserved for application specific use, and finally the offset of
    the actual image data in the file.

        424d     # magic number
        86580200 # file size
        00000000 # application specific (unused)
        36000000 # offset of image data

    Note that the preceding code block will be part of a binary file created by
    [`lb`][lb]. Whitespace including linebreaks and comments will be ignored.

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

    This code block will also be part of a binary file created by [`lb`][lb].

Explaining the meaning of all fields in the second header is beyond the scope of
this example because it is not meant to be an introduction to the BMP format but
merely to show how a literate binary file could look like. Some hints, however:

  * Width and height are given in pixels, so this is a 226x226 pixels image
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

    (
    (0000ff){113} # 113 red pixels
    (ffffff){113} # 113 white pixels
    0000          # padding
    ){113}        # 113 lines of red and white pixels

    # the same but with blue and green pixels
    ( (ff0000){113} (00ff00){113} 0000 ){113}

These two code blocks will also be part of a binary file created by [`lb`][lb].
Note the use of hex macros: Where `0000ff` denotes three bytes that encode a
single red pixel, `(0000ff){113}` denotes a sequence of 339 bytes (or 113
repetitions of this three bytes sequence) that encode 113 red pixels. Macros may
be nested, making it easy to create 113 lines of 113 red and 113 white pixels,
resulting in a red and a white square next to each other.

[lb]: https://github.com/marhop/literate-binary
