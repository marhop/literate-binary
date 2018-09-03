# WAVE file example

A WAVE file consists of a series of sections called chunks. The general
structure of a chunk, the concept of building arbitrary files from chunks and
the WAVE format itself as a specific instance of this concept are defined in the
Resource Interchange File Format, or RIFF, specification. [Here is a PDF
copy][RIFF]; the WAVE format description starts on page 56.

Three chunks are necessary to build a WAVE file; the RIFF chunk, the format
chunk and the data chunk. All chunks share the same basic structure:

  1. The *chunk ID*, a four bytes ASCII string.
  2. The *chunk size*, a four bytes number identifying the size of the chunk
     data.
  3. The *chunk data*, whose content is specific to the chunk type.

Numeric fields are in little endian byte order.

[RIFF]: http://www-mmsp.ece.mcgill.ca/Documents/AudioFormats/WAVE/Docs/riffmci.pdf

## RIFF chunk

A chunk of this type is common to all RIFF based formats. Its chunk ID is
"RIFF", and its chunk data consists of an ASCII string denoting the RIFF format
instance ("WAVE" in this case), followed by the remaining chunks described
below.

    52494646 # chunk ID "RIFF"
    24f40000 # chunk size
    57415645 # chunk data "WAVE"

Note that since the chunk data comprises all the rest of the file the chunk size
equals file size minus 8.

## Format chunk

The format chunk is specific to WAVE files. Its chunk ID is "fmt " (note the
trailing whitespace).

    666d7420 # chunk ID "fmt "
    10000000 # chunk size

The chunk data is composed of several fields with a size of two or four bytes
each that describe the format of the payload in the data chunk below.

 1. The WAVE format category that determines how the audio data in the data
    chunk is encoded. Pulse code modulation (PCM) has format category 1.

        0100

 2. The number of channels in the audio data. Two for stereo sound.

        0200

 3. The sampling rate. 8000 (0x1f40) samples per second.

        401f0000

    Other common sampling rates include 44100 (CD quality), 48000, and 192000
    samples per second.

 4. The average number of bytes per second. The RIFF specification defines this
    as channels × bits per second × bytes per sample, but obviously it should be
    samples per second (i.e., sampling rate) instead of bits per second. See
    field 6 below for the number of bytes per sample.

    2 × 8000 × 1 = 16000 = 0x3e80

        803e0000

 5. The block alignment, that is, how many bytes have to be read for playback of
    each sample: number of channels × bytes per sample.

    2 × 1 = 2

        0200

 6. The number of bits per sample, also known as bit depth. One byte for each
    sample of audio data.

        0800

    Other bit depths like 16 bits (CD quality) are possible, but there is an
    inconsistency in the RIFF specification: If one to eight bits are used this
    value is interpreted as an unsigned integer, but with more than eight bits
    it is interpreted as a signed integer ...

The two most important pieces of information for interpretation of the data
chunk are fields 3 and 6, sampling rate and bit depth. These are the central
parameters of pulse code modulation.

## Data chunk

This chunk contains the actual audio data which is represented using pulse code
modulation.

Sound, from a physics perspective, is a vibration that can be visualized by a
graph called waveform. (Josh Comeau has written a *really* great [tutorial about
this][waveforms].) Consequently, digital representation of sound is a matter of
encoding a waveform in binary. With [pulse code modulation][PCM], this requires
two steps called [sampling] and [quantization]. First, the waveform is sampled
at regular, uniform intervals, resulting in a list of values. The length of this
list depends on the sampling rate; a sampling rate of 8000 samples per second
obviously yields 8000 values per second of sound per channel. Second, the
samples are mapped (or rounded, if you will) to the available range of numbers.
This range is determined by the bit depth; with a bit depth of 8 bits per
sample, the samples can be mapped to 2⁸ = 256 different numbers.

The chunk data of a PCM WAVE data chunk consists of a simple series of quantized
waveform samples. Samples for multiple channels are interleaved, so two lists of
samples (c11, c12, c13, ...) and (c21, c22, c23, ...) representing two audio
channels become (c11, c21, c12, c22, c13, c23, ...) in the chunk data. In this
example, both stereo channels play the same sound so each pair of samples
consists of the same two bytes. (Yes, that's like mono with doubled memory
usage. It's just an example!)

    64617461 # chunk ID "data"
    00fa0000 # chunk size

    # 80 stereo samples repeated 100 times = 8000 samples = 1 second at 100 Hz
    (7f7f 8989 9393 9d9d a6a6 b0b0 b9b9 c1c1 caca d1d1 d9d9 e0e0 e6e6 ebeb f0f0
    f4f4 f8f8 fafa fcfc fefe fefe fefe fcfc fafa f8f8 f4f4 f0f0 ebeb e6e6 e0e0
    d9d9 d1d1 caca c1c1 b9b9 b0b0 a6a6 9d9d 9393 8989 7f7f 7575 6b6b 6161 5858
    4e4e 4545 3d3d 3434 2d2d 2525 1e1e 1818 1313 0e0e 0a0a 0606 0404 0202 0000
    0000 0000 0202 0404 0606 0a0a 0e0e 1313 1818 1e1e 2525 2d2d 3434 3d3d 4545
    4e4e 5858 6161 6b6b 7575){100}

    # 40 stereo samples repeated 200 times = 8000 samples = 1 second at 200 Hz
    (7f7f 9393 a6a6 b9b9 caca d9d9 e6e6 f0f0 f8f8 fcfc fefe fcfc f8f8 f0f0 e6e6
    d9d9 caca b9b9 a6a6 9393 7f7f 6b6b 5858 4545 3434 2525 1818 0e0e 0606 0202
    0000 0202 0606 0e0e 1818 2525 3434 4545 5858 6b6b){200}

    # 20 stereo samples repeated 400 times = 8000 samples = 1 second at 400 Hz
    (7f7f a6a6 caca e6e6 f8f8 fefe f8f8 e6e6 caca a6a6 7f7f 5858 3434 1818 0606
    0000 0606 1818 3434 5858){400}

    # 10 stereo samples repeated 800 times = 8000 samples = 1 second at 800 Hz
    (7f7f caca f8f8 f8f8 caca 7f7f 3434 0606 0606 3434){800}

Of course, real-world waveform samples are not generated manually but by systems
called [ADC]. But for the sake of example, this file contains four genuine
handmade beeps. The remaining paragraphs explain how the samples in the
preceding code block were obtained.

In one of its simplest forms a waveform visualizing sound takes the shape of the
sine function which is periodic, meaning it repeats its values in regular
intervals and thus can be repeated over and over to produce a constant tone. The
pitch of this tone is determined by the number of cycles of the sine function in
a given unit of time; faster repetitions produce a higher pitch. The number of
repetitions per second, the frequency, is measured in Hertz (Hz), so an 800 Hz
tone has a higher pitch than a 200 Hz tone. (Do not confuse this frequency with
the sampling rate which is also given in Hertz sometimes!)

Consider the tone that corresponds to a 200 Hz sine waveform. The waveform has
200 cycles per second, so using a sampling rate of 8000 samples per second one
second of this tone would be represented by 200 repetitions of a pattern
consisting of 8000/200 = 40 samples. (Contrast this with a 400 Hz sine waveform
that needs 400 repetitions of 8000/400 = 20 samples.) Since the sine function is
periodic with period 2π the 40 samples representing one cylce are sin(0),
sin((1/40) × 2π), sin((2/40) × 2π), ..., sin((39/40) × 2π). Quantizing these
values to a bit depth of 8 bits yields the bytes in the code block above.

Here are some values for the 200 Hz (40 samples) example, where sample(x) =
sin((x/40) × 2π) and quantized(y) = 127 + round(127 × y):

x  | sample              | quantized | hex
---|---------------------|-----------|----
0  | 0.0                 | 127       | 7f
1  | 0.15643446504023087 | 147       | 93
2  | 0.3090169943749474  | 166       | a6
   | ...                 | ...       | ...
39 | -0.1564344650402311 | 107       | 6b

And finally, here is the Haskell program that was used to generate the hex
patterns in the code block above (something equivalent could be done in any
other programming language, of course):

~~~ {.nobin}
import Text.Printf

main = putStr $ patterns [80, 40, 20, 10]

patterns = unlines . map (unwords . map fmt . samples)

-- | Create x samples of one cycle of the sine function, quantized to integer
-- values in the 8 bit range (0..256).
samples x = map (quant . sample . (/ x)) [0 .. (x - 1)]
  where
    sample x = sin $ x * 2 * pi
    quant x = 127 + round (127 * x)

-- | Format number as hex string. Twice, because redundant stereo.
fmt :: Integer -> String
fmt x = printf "%02x%02x" x x
~~~

[waveforms]: https://pudding.cool/2018/02/waveforms/
[PCM]: https://en.wikipedia.org/wiki/Pulse-code_modulation
[sampling]: https://en.wikipedia.org/wiki/Sampling_(signal_processing)
[quantization]: https://en.wikipedia.org/wiki/Quantization_(signal_processing)
[ADC]: https://en.wikipedia.org/wiki/Analog-to-digital_converter
