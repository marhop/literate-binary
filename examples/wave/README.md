The documents in this directory describe WAVE files in ["literate binary"][lb]
notation, integrating handcrafted binary (expressed as hex code) with
documentation written in [Markdown][Markdown]. Two different representations can
be derived from these documents:

 1. PDF/HTML/Latex ... files, using a Markdown converter like [Pandoc][Pandoc].
 2. Binary WAVE files, using the [`lb` tool][lb].

[lb]: https://github.com/marhop/literate-binary
[Markdown]: https://commonmark.org/
[Pandoc]: https://pandoc.org/

The following files are available:

  * [wave.md](wave.md) - a basic WAVE file, including an introduction to the
    file format (start here!)
  * [broadcast-wave.md](broadcast-wave.md) - a Broadcast Wave Format (BWF) file
  * [multiple-channel-wave.md](multiple-channel-wave.md) - a WAVE file with
    multiple audio channels, using the WAVE_FORMAT_EXTENSIBLE extension
  * [wave-with-xmp-metadata.md](wave-with-xmp-metadata.md) - a WAVE file with
    added XMP metadata
