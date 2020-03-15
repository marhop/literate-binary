This document describes a WAVE file with multiple audio channels, also known as
WAVE_FORMAT_EXTENSIBLE, in ["literate binary"][lb] notation, integrating
handcrafted binary (expressed as hex code) with documentation written in
[Markdown][Markdown]. Two different representations can be derived from this
document:

 1. A PDF/HTML/Latex ... file, using a Markdown converter like [Pandoc][Pandoc].
 2. A binary WAVE file, using the [`lb` tool][lb].

[lb]: https://github.com/marhop/literate-binary
[Markdown]: https://commonmark.org/
[Pandoc]: https://pandoc.org/

# Multiple channel WAVE file example

Although the WAVE file format supports up to 65535 audio channels, the original
[format specification][RIFF] did not define a mapping of channels to speaker
positions for more than two channels (i.e., stereo). This was remedied by an
[extension of the WAVE format][MULTIWAV] introduced by Microsoft with Windows
2000 which is commonly referred to as WAVE_FORMAT_EXTENSIBLE or
WAVEFORMATEXTENSIBLE. It adds a few fields to the format chunk, particularly a
speaker mapping for up to 18 channels.

This example file is not an introduction to the WAVE file format itself but to
the peculiarities of multi channel WAVE files. Basic knowledge of the WAVE file
format is assumed.

## RIFF chunk

Each WAVE file has at least three sections (called chunks); the RIFF chunk, the
format chunk and the data chunk. Together they make a complete WAVE file.

The RIFF chunk identifies the file format (RIFF/WAVE) and the size of the
remaining data (including the string "WAVE" but excluding the chunk ID "RIFF"
and the size field itself -- should be equal to file size minus 8).

    "RIFF" c8881500 "WAVE"

## Format chunk and extensions

The format chunk holds technical metadata necessary to decode the byte stream in
the data chunk below. This includes information about the encoding method (or
WAVE format category), the number of channels, the sampling rate, the average
number of bytes per second (number of channels × sampling rate × bytes per
sample), the block alignment (number of channels × bytes per sample), and
finally the bit depth (or bits per sample).

    "fmt "   # chunk ID
    28000000 # chunk size including extra fields below
    feff     # WAVE format category, WAVE_FORMAT_EXTENSIBLE
    0400     # number of channels, 4
    44ac0000 # sampling rate, 44100 Hz
    20620500 # average number of bytes per second, 352800
    0800     # block alignment, 8
    1000     # bits per sample, 16

So far, apart from the format category this is the classic format chunk for PCM
WAVE files as defined in the [original specification][RIFF]. To account for the
problem of mapping multiple audio channels to speaker positions as well as to
resolve some ambiguities concerning the bit depth, [the WAVE_FORMAT_EXTENSIBLE
format category][MULTIWAV] appends a set of additional fields to the format
chunk:

 1. The size of the additional fields (not including the size field itself), 22.

        1600

 2. The number of valid bits per sample, 16.

        1000

    This is very often the same as the bits per sample field above. So what is
    the difference? For example, consider 12 bit-samples. Storing one
    byte-aligned sample obviously requires two bytes, or 16 bits. This leads to
    an ambiguity because the bits per sample field could now be set to either 12
    or 16. Using both fields together resolves this ambiguity: As per the
    [WAVE_FORMAT_EXTENSIBLE specification][MULTIWAV], bits per sample would be
    16 and valid bits per sample would be 12.

 3. The mapping of audio channels to speaker positions. This is a bit field
    called the "channel mask". Each bit is assigned to a pre-defined speaker
    position; setting a bit to `1` marks the respective speaker active. The
    channels present in the audio data below are mapped to active speakers in
    sequential order: first channel to first active speaker, second channel to
    second active speaker and so on. In this example the four audio channels use
    the front left, front right, back left and back right speakers which
    requires setting the first, second, fifth and sixth bit, respectively
    (counting from the least significant bit): `... 0011 0011`. This corresponds
    to the following little endian byte sequence:

        33000000

    The complete channel mask mapping can be found in the
    [WAVE_FORMAT_EXTENSIBLE specification][MULTIWAV].

 4. The subformat GUID. Usually the format category above determines how the
    audio data in the data chunk is encoded (like the common format category 1,
    aka WAVE_FORMAT_PCM, for pulse code modulation). However,
    WAVE_FORMAT_EXTENSIBLE does not prescribe a particular encoding method.
    Instead it allows to specify the format of the data chunk content by means
    of a GUID. The subformat GUID for PCM (i.e., the immediate counterpart of
    format category 1) as defined in the WAVE_FORMAT_EXTENSIBLE specification is
    this:

        0100000000001000800000aa00389b71

Note that while these fields are specified by the WAVE_FORMAT_EXTENSIBLE format
category, the fact they can be added to the format chunk at all is due to an
extension called WAVEFORMATEX introduced in an earlier [revision of the WAVE
specification][RIFFNEW]. The wording may be a little confusing. To be precise,
WAVEFORMATEX is a data structure in the Windows API that defines an extensible
format chunk (i.e., a format chunk with arbitrary additional fields), while
WAVE_FORMAT_EXTENSIBLE is a WAVE format category that makes use of this generic
extensibility to add some concrete fields to the format chunk.

## Fact chunk

The fact chunk consists of just one value, the number of samples (per channel)
in the data chunk.

    "fact" 04000000 10b10200

The specification of this chunk is problematic in two ways; should it be
included at all and how should its value be calculated? This is best explained
by two quotes from [this WAVE format overview][mcgill]:

> The [Rev. 3 documentation][RIFFNEW] states that the Fact chunk "is required
> for all new WAVE formats", but "is not required" for the standard
> WAVE_FORMAT_PCM file. [...] There is a question as to whether the fact chunk
> should be used for (including those with PCM) WAVE_FORMAT_EXTENSIBLE files.

> There is an ambiguity as to the meaning of "number of samples" for
> multichannel data. The implication in the Rev. 3 documentation is that it
> should be interpreted to be "number of samples per channel". The statement in
> the Rev. 3 documentation is: "The nSamplesPerSec field from the wave format
> header is used in conjunction with the dwSampleLength field to determine the
> length of the data in seconds." With no mention of the number of channels in
> this computation, this implies that dwSampleLength is the number of samples
> per channel.

## Data chunk

The data chunk contains the actual audio data. To highlight the use of multiple
channels in this example it consists of a constant beep (a 441 Hz sine tone)
playing on each channel in turn for one second.

As usual, samples for multiple channels are interleaved. This makes the mapping
of channels to speaker positions visible in the byte patterns below: As
specified by the channel mask in the format chunk, the active speakers are (in
this order) front left, front right, back left and back right. So a block of
samples like `0b10 0000 0000 0000` maps the 16 bit-sample `0b10` to the front
left speaker and `0000` to each of the other active speakers, while `0000 0000
fc17 0000` maps `fc17` to the back left speaker. Now, if the whole sequence of
sine values is mapped to one speaker while the other three speakers receive
nothing but constant values like `0000` this results in one speaker playing a
beep and the other three remaining silent.

    "data" 80881500

    # one second, 441 Hz sine tone, front left
    ( 0000 0000 0000 0000 0908 0000 0000 0000
      0b10 0000 0000 0000 fc17 0000 0000 0000
      d51f 0000 0000 0000 8e27 0000 0000 0000
      1e2f 0000 0000 0000 8036 0000 0000 0000
      aa3d 0000 0000 0000 9544 0000 0000 0000
      3c4b 0000 0000 0000 9651 0000 0000 0000
      9f57 0000 0000 0000 4e5d 0000 0000 0000
      9f62 0000 0000 0000 8d67 0000 0000 0000
      126c 0000 0000 0000 2a70 0000 0000 0000
      d073 0000 0000 0000 0277 0000 0000 0000
      bb79 0000 0000 0000 fa7b 0000 0000 0000
      bb7d 0000 0000 0000 fd7e 0000 0000 0000
      be7f 0000 0000 0000 ff7f 0000 0000 0000
      be7f 0000 0000 0000 fd7e 0000 0000 0000
      bb7d 0000 0000 0000 fa7b 0000 0000 0000
      bb79 0000 0000 0000 0277 0000 0000 0000
      d073 0000 0000 0000 2a70 0000 0000 0000
      126c 0000 0000 0000 8d67 0000 0000 0000
      9f62 0000 0000 0000 4e5d 0000 0000 0000
      9f57 0000 0000 0000 9651 0000 0000 0000
      3c4b 0000 0000 0000 9544 0000 0000 0000
      aa3d 0000 0000 0000 8036 0000 0000 0000
      1e2f 0000 0000 0000 8e27 0000 0000 0000
      d51f 0000 0000 0000 fc17 0000 0000 0000
      0b10 0000 0000 0000 0908 0000 0000 0000
      0000 0000 0000 0000 f7f7 0000 0000 0000
      f5ef 0000 0000 0000 04e8 0000 0000 0000
      2be0 0000 0000 0000 72d8 0000 0000 0000
      e2d0 0000 0000 0000 80c9 0000 0000 0000
      56c2 0000 0000 0000 6bbb 0000 0000 0000
      c4b4 0000 0000 0000 6aae 0000 0000 0000
      61a8 0000 0000 0000 b2a2 0000 0000 0000
      619d 0000 0000 0000 7398 0000 0000 0000
      ee93 0000 0000 0000 d68f 0000 0000 0000
      308c 0000 0000 0000 fe88 0000 0000 0000
      4586 0000 0000 0000 0684 0000 0000 0000
      4582 0000 0000 0000 0381 0000 0000 0000
      4280 0000 0000 0000 0180 0000 0000 0000
      4280 0000 0000 0000 0381 0000 0000 0000
      4582 0000 0000 0000 0684 0000 0000 0000
      4586 0000 0000 0000 fe88 0000 0000 0000
      308c 0000 0000 0000 d68f 0000 0000 0000
      ee93 0000 0000 0000 7398 0000 0000 0000
      619d 0000 0000 0000 b2a2 0000 0000 0000
      61a8 0000 0000 0000 6aae 0000 0000 0000
      c4b4 0000 0000 0000 6bbb 0000 0000 0000
      56c2 0000 0000 0000 80c9 0000 0000 0000
      e2d0 0000 0000 0000 72d8 0000 0000 0000
      2be0 0000 0000 0000 04e8 0000 0000 0000
      f5ef 0000 0000 0000 f7f7 0000 0000 0000
    ){441}

    # one second, 441 Hz sine tone, back left
    ( 0000 0000 0000 0000 0000 0000 0908 0000
      0000 0000 0b10 0000 0000 0000 fc17 0000
      0000 0000 d51f 0000 0000 0000 8e27 0000
      0000 0000 1e2f 0000 0000 0000 8036 0000
      0000 0000 aa3d 0000 0000 0000 9544 0000
      0000 0000 3c4b 0000 0000 0000 9651 0000
      0000 0000 9f57 0000 0000 0000 4e5d 0000
      0000 0000 9f62 0000 0000 0000 8d67 0000
      0000 0000 126c 0000 0000 0000 2a70 0000
      0000 0000 d073 0000 0000 0000 0277 0000
      0000 0000 bb79 0000 0000 0000 fa7b 0000
      0000 0000 bb7d 0000 0000 0000 fd7e 0000
      0000 0000 be7f 0000 0000 0000 ff7f 0000
      0000 0000 be7f 0000 0000 0000 fd7e 0000
      0000 0000 bb7d 0000 0000 0000 fa7b 0000
      0000 0000 bb79 0000 0000 0000 0277 0000
      0000 0000 d073 0000 0000 0000 2a70 0000
      0000 0000 126c 0000 0000 0000 8d67 0000
      0000 0000 9f62 0000 0000 0000 4e5d 0000
      0000 0000 9f57 0000 0000 0000 9651 0000
      0000 0000 3c4b 0000 0000 0000 9544 0000
      0000 0000 aa3d 0000 0000 0000 8036 0000
      0000 0000 1e2f 0000 0000 0000 8e27 0000
      0000 0000 d51f 0000 0000 0000 fc17 0000
      0000 0000 0b10 0000 0000 0000 0908 0000
      0000 0000 0000 0000 0000 0000 f7f7 0000
      0000 0000 f5ef 0000 0000 0000 04e8 0000
      0000 0000 2be0 0000 0000 0000 72d8 0000
      0000 0000 e2d0 0000 0000 0000 80c9 0000
      0000 0000 56c2 0000 0000 0000 6bbb 0000
      0000 0000 c4b4 0000 0000 0000 6aae 0000
      0000 0000 61a8 0000 0000 0000 b2a2 0000
      0000 0000 619d 0000 0000 0000 7398 0000
      0000 0000 ee93 0000 0000 0000 d68f 0000
      0000 0000 308c 0000 0000 0000 fe88 0000
      0000 0000 4586 0000 0000 0000 0684 0000
      0000 0000 4582 0000 0000 0000 0381 0000
      0000 0000 4280 0000 0000 0000 0180 0000
      0000 0000 4280 0000 0000 0000 0381 0000
      0000 0000 4582 0000 0000 0000 0684 0000
      0000 0000 4586 0000 0000 0000 fe88 0000
      0000 0000 308c 0000 0000 0000 d68f 0000
      0000 0000 ee93 0000 0000 0000 7398 0000
      0000 0000 619d 0000 0000 0000 b2a2 0000
      0000 0000 61a8 0000 0000 0000 6aae 0000
      0000 0000 c4b4 0000 0000 0000 6bbb 0000
      0000 0000 56c2 0000 0000 0000 80c9 0000
      0000 0000 e2d0 0000 0000 0000 72d8 0000
      0000 0000 2be0 0000 0000 0000 04e8 0000
      0000 0000 f5ef 0000 0000 0000 f7f7 0000
    ){441}

    # one second, 441 Hz sine tone, back right
    ( 0000 0000 0000 0000 0000 0000 0000 0908
      0000 0000 0000 0b10 0000 0000 0000 fc17
      0000 0000 0000 d51f 0000 0000 0000 8e27
      0000 0000 0000 1e2f 0000 0000 0000 8036
      0000 0000 0000 aa3d 0000 0000 0000 9544
      0000 0000 0000 3c4b 0000 0000 0000 9651
      0000 0000 0000 9f57 0000 0000 0000 4e5d
      0000 0000 0000 9f62 0000 0000 0000 8d67
      0000 0000 0000 126c 0000 0000 0000 2a70
      0000 0000 0000 d073 0000 0000 0000 0277
      0000 0000 0000 bb79 0000 0000 0000 fa7b
      0000 0000 0000 bb7d 0000 0000 0000 fd7e
      0000 0000 0000 be7f 0000 0000 0000 ff7f
      0000 0000 0000 be7f 0000 0000 0000 fd7e
      0000 0000 0000 bb7d 0000 0000 0000 fa7b
      0000 0000 0000 bb79 0000 0000 0000 0277
      0000 0000 0000 d073 0000 0000 0000 2a70
      0000 0000 0000 126c 0000 0000 0000 8d67
      0000 0000 0000 9f62 0000 0000 0000 4e5d
      0000 0000 0000 9f57 0000 0000 0000 9651
      0000 0000 0000 3c4b 0000 0000 0000 9544
      0000 0000 0000 aa3d 0000 0000 0000 8036
      0000 0000 0000 1e2f 0000 0000 0000 8e27
      0000 0000 0000 d51f 0000 0000 0000 fc17
      0000 0000 0000 0b10 0000 0000 0000 0908
      0000 0000 0000 0000 0000 0000 0000 f7f7
      0000 0000 0000 f5ef 0000 0000 0000 04e8
      0000 0000 0000 2be0 0000 0000 0000 72d8
      0000 0000 0000 e2d0 0000 0000 0000 80c9
      0000 0000 0000 56c2 0000 0000 0000 6bbb
      0000 0000 0000 c4b4 0000 0000 0000 6aae
      0000 0000 0000 61a8 0000 0000 0000 b2a2
      0000 0000 0000 619d 0000 0000 0000 7398
      0000 0000 0000 ee93 0000 0000 0000 d68f
      0000 0000 0000 308c 0000 0000 0000 fe88
      0000 0000 0000 4586 0000 0000 0000 0684
      0000 0000 0000 4582 0000 0000 0000 0381
      0000 0000 0000 4280 0000 0000 0000 0180
      0000 0000 0000 4280 0000 0000 0000 0381
      0000 0000 0000 4582 0000 0000 0000 0684
      0000 0000 0000 4586 0000 0000 0000 fe88
      0000 0000 0000 308c 0000 0000 0000 d68f
      0000 0000 0000 ee93 0000 0000 0000 7398
      0000 0000 0000 619d 0000 0000 0000 b2a2
      0000 0000 0000 61a8 0000 0000 0000 6aae
      0000 0000 0000 c4b4 0000 0000 0000 6bbb
      0000 0000 0000 56c2 0000 0000 0000 80c9
      0000 0000 0000 e2d0 0000 0000 0000 72d8
      0000 0000 0000 2be0 0000 0000 0000 04e8
      0000 0000 0000 f5ef 0000 0000 0000 f7f7
    ){441} 

    # one second, 441 Hz sine tone, front right
    ( 0000 0000 0000 0000 0000 0908 0000 0000
      0000 0b10 0000 0000 0000 fc17 0000 0000
      0000 d51f 0000 0000 0000 8e27 0000 0000
      0000 1e2f 0000 0000 0000 8036 0000 0000
      0000 aa3d 0000 0000 0000 9544 0000 0000
      0000 3c4b 0000 0000 0000 9651 0000 0000
      0000 9f57 0000 0000 0000 4e5d 0000 0000
      0000 9f62 0000 0000 0000 8d67 0000 0000
      0000 126c 0000 0000 0000 2a70 0000 0000
      0000 d073 0000 0000 0000 0277 0000 0000
      0000 bb79 0000 0000 0000 fa7b 0000 0000
      0000 bb7d 0000 0000 0000 fd7e 0000 0000
      0000 be7f 0000 0000 0000 ff7f 0000 0000
      0000 be7f 0000 0000 0000 fd7e 0000 0000
      0000 bb7d 0000 0000 0000 fa7b 0000 0000
      0000 bb79 0000 0000 0000 0277 0000 0000
      0000 d073 0000 0000 0000 2a70 0000 0000
      0000 126c 0000 0000 0000 8d67 0000 0000
      0000 9f62 0000 0000 0000 4e5d 0000 0000
      0000 9f57 0000 0000 0000 9651 0000 0000
      0000 3c4b 0000 0000 0000 9544 0000 0000
      0000 aa3d 0000 0000 0000 8036 0000 0000
      0000 1e2f 0000 0000 0000 8e27 0000 0000
      0000 d51f 0000 0000 0000 fc17 0000 0000
      0000 0b10 0000 0000 0000 0908 0000 0000
      0000 0000 0000 0000 0000 f7f7 0000 0000
      0000 f5ef 0000 0000 0000 04e8 0000 0000
      0000 2be0 0000 0000 0000 72d8 0000 0000
      0000 e2d0 0000 0000 0000 80c9 0000 0000
      0000 56c2 0000 0000 0000 6bbb 0000 0000
      0000 c4b4 0000 0000 0000 6aae 0000 0000
      0000 61a8 0000 0000 0000 b2a2 0000 0000
      0000 619d 0000 0000 0000 7398 0000 0000
      0000 ee93 0000 0000 0000 d68f 0000 0000
      0000 308c 0000 0000 0000 fe88 0000 0000
      0000 4586 0000 0000 0000 0684 0000 0000
      0000 4582 0000 0000 0000 0381 0000 0000
      0000 4280 0000 0000 0000 0180 0000 0000
      0000 4280 0000 0000 0000 0381 0000 0000
      0000 4582 0000 0000 0000 0684 0000 0000
      0000 4586 0000 0000 0000 fe88 0000 0000
      0000 308c 0000 0000 0000 d68f 0000 0000
      0000 ee93 0000 0000 0000 7398 0000 0000
      0000 619d 0000 0000 0000 b2a2 0000 0000
      0000 61a8 0000 0000 0000 6aae 0000 0000
      0000 c4b4 0000 0000 0000 6bbb 0000 0000
      0000 56c2 0000 0000 0000 80c9 0000 0000
      0000 e2d0 0000 0000 0000 72d8 0000 0000
      0000 2be0 0000 0000 0000 04e8 0000 0000
      0000 f5ef 0000 0000 0000 f7f7 0000 0000
    ){441}

[mcgill]: http://www-mmsp.ece.mcgill.ca/Documents/AudioFormats/WAVE/WAVE.html
[RIFF]: http://www-mmsp.ece.mcgill.ca/Documents/AudioFormats/WAVE/Docs/riffmci.pdf
[RIFFNEW]: http://www-mmsp.ece.mcgill.ca/Documents/AudioFormats/WAVE/Docs/RIFFNEW.pdf
[MULTIWAV]: http://www-mmsp.ece.mcgill.ca/Documents/AudioFormats/WAVE/Docs/multichaudP.pdf
