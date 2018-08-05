This file describes a simple BMP, or Bitmap Image File in hexadecimal notation.
It can be "compiled" in two ways:

 1. Into a PDF/HTML/Latex ... file, using a Markdown converter like Pandoc.
 2. Into a binary BMP file, using [lb].

[lb]: https://github.com/marhop/literate-binary

# BMP File Example

## File Headers

A BMP file starts with two headers, the BMP, or bitmap header and the DIB, or
device independent bitmap header (sometimes called bitmap information header).

 1. The first header starts with the magic number `424d` (ASCII "BM") that helps
    to identify the file type, followed by the total size of the file in bytes,
    eight bytes reserved for application specific use, and finally the offset of
    the actual image data in the file.

        424d     # magic number
        46000000 # file size
        00000000 # application specific (unused)
        36000000 # offset of image data

 2. Information about the image encoded in this file, such as dimensions and
    color information, can be found in the second header.

        28000000 # header size
        02000000 # image width
        02000000 # image height
        0100     # number of color planes
        1800     # number of bits per pixel
        00000000 # compression type of image data (0 = uncompressed)
        10000000 # size of image data (including padding)
        130b0000 # horizontal print resolution, pixels/metre
        130b0000 # vertical print resolution, pixels/metre
        00000000 # number of colors in palette
        00000000 # important colors (0 = all)

Explaining the meaning of all fields in the second header is beyond the scope of
this example because it is not meant to be an introduction to the BMP format but
merely to show how a literate binary file could look like. Some hints, however:

  * Width and height are given in pixels, so this is a 2x2 pixels image.
  * Most of the color stuff is either always the same (like number of color
    planes) or irrelevant in the present case (like number of colors in the
    palette because this file has no palette).
  * The number of bits per pixel (0x18 = 24) and the compression type
    (uncompressed) indicate what the image data in the next section should look
    like: 3 bytes per pixel, without any compression.

## Image Data

The actual image data consists of an array of pixels with each pixel being
represented by a three byte blue/green/red value -- think RGB in little endian.
Pixels are in bottom left to top right order, interspersed with padding (usually
NULL bytes) to achieve a 4 byte alignment for each line of pixels. 

    0000ff # red pixel
    ffffff # white pixel
    0000   # padding
    ff0000 # blue pixel
    00ff00 # green pixel
    0000   # padding

