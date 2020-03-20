This document describes a WAVE file in ["literate binary"][lb] notation,
integrating handcrafted binary (expressed as hex code) with documentation
written in [Markdown][Markdown]. Two different representations can be derived
from this document:

 1. A PDF/HTML/Latex ... file, using a Markdown converter like [Pandoc][Pandoc].
 2. A binary WAVE file, using the [`lb` tool][lb].

[lb]: https://github.com/marhop/literate-binary
[Markdown]: https://commonmark.org/
[Pandoc]: https://pandoc.org/

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

    "RIFF"   # chunk ID
    44620500 # chunk size
    "WAVE"   # chunk data

Note that since the chunk data comprises all the rest of the file the chunk size
equals file size minus 8.

## Format chunk

The format chunk is specific to WAVE files. Its chunk ID is "fmt " (note the
trailing whitespace).

    "fmt "   # chunk ID
    10000000 # chunk size

The chunk data is composed of several fields with a size of two or four bytes
each that describe the format of the payload in the data chunk below.

 1. The WAVE format category that determines how the audio data in the data
    chunk is encoded. Pulse code modulation (PCM) has format category 1.

        0100

 2. The number of channels in the audio data. Two for stereo sound.

        0200

 3. The sampling rate. 44100 (0xac44) samples per second (44.1 kHz) are defined
    as "CD quality".

        44ac0000

    Other common sampling rates include 48000 and 192000 samples per second.

 4. The average number of bytes per second. The RIFF specification defines this
    as channels × bits per second × bytes per sample, but obviously it should be
    samples per second (i.e., sampling rate) instead of bits per second. See
    field 6 below for the number of bytes per sample.

    2 × 44100 × 2 = 176400 = 0x2b110

        10b10200

 5. The block alignment, that is, how many bytes have to be read for playback of
    each sample: number of channels × bytes per sample.

    2 × 2 = 4

        0400

 6. The number of bits per sample, also known as bit depth. Two bytes (16 bits)
    for each sample of audio data are defined as "CD quality".

        1000

    Other bit depths like 8 or 24 bits are possible, but there is an
    inconsistency in the RIFF specification: If one to eight bits are used the
    samples in the data chunk below are interpreted as unsigned integers, but
    with more than eight bits they are interpreted as signed integers ...

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
list depends on the sampling rate; a sampling rate of 44100 samples per second
obviously yields 44100 values per second of sound per channel. Second, the
samples are mapped (or rounded, if you will) to the available range of numbers.
This range is determined by the bit depth; with a bit depth of 16 bits per
sample, the samples can be mapped to 2¹⁶ = 65536 different numbers.

The chunk data of a PCM WAVE data chunk consists of a simple series of quantized
waveform samples. Samples for multiple channels are interleaved, so two lists of
samples (a₀, a₁, a₂, ...) and (b₀, b₁, b₂, ...) representing two audio channels
become (a₀, b₀, a₁, b₁, a₂, b₂, ...) in the chunk data.

    "data"   # chunk ID
    20620500 # chunk size

    # one second, 441 Hz sine tone, left channel
    ( 0000 0000 0908 0000 0b10 0000 fc17 0000 d51f 0000
      8e27 0000 1e2f 0000 8036 0000 aa3d 0000 9544 0000
      3c4b 0000 9651 0000 9f57 0000 4e5d 0000 9f62 0000
      8d67 0000 126c 0000 2a70 0000 d073 0000 0277 0000
      bb79 0000 fa7b 0000 bb7d 0000 fd7e 0000 be7f 0000
      ff7f 0000 be7f 0000 fd7e 0000 bb7d 0000 fa7b 0000
      bb79 0000 0277 0000 d073 0000 2a70 0000 126c 0000
      8d67 0000 9f62 0000 4e5d 0000 9f57 0000 9651 0000
      3c4b 0000 9544 0000 aa3d 0000 8036 0000 1e2f 0000
      8e27 0000 d51f 0000 fc17 0000 0b10 0000 0908 0000
      0000 0000 f7f7 0000 f5ef 0000 04e8 0000 2be0 0000
      72d8 0000 e2d0 0000 80c9 0000 56c2 0000 6bbb 0000
      c4b4 0000 6aae 0000 61a8 0000 b2a2 0000 619d 0000
      7398 0000 ee93 0000 d68f 0000 308c 0000 fe88 0000
      4586 0000 0684 0000 4582 0000 0381 0000 4280 0000
      0180 0000 4280 0000 0381 0000 4582 0000 0684 0000
      4586 0000 fe88 0000 308c 0000 d68f 0000 ee93 0000
      7398 0000 619d 0000 b2a2 0000 61a8 0000 6aae 0000
      c4b4 0000 6bbb 0000 56c2 0000 80c9 0000 e2d0 0000
      72d8 0000 2be0 0000 04e8 0000 f5ef 0000 f7f7 0000
    ){441}

    # one second, 441 Hz sine tone, right channel
    ( 0000 0000 0000 0908 0000 0b10 0000 fc17 0000 d51f
      0000 8e27 0000 1e2f 0000 8036 0000 aa3d 0000 9544
      0000 3c4b 0000 9651 0000 9f57 0000 4e5d 0000 9f62
      0000 8d67 0000 126c 0000 2a70 0000 d073 0000 0277
      0000 bb79 0000 fa7b 0000 bb7d 0000 fd7e 0000 be7f
      0000 ff7f 0000 be7f 0000 fd7e 0000 bb7d 0000 fa7b
      0000 bb79 0000 0277 0000 d073 0000 2a70 0000 126c
      0000 8d67 0000 9f62 0000 4e5d 0000 9f57 0000 9651
      0000 3c4b 0000 9544 0000 aa3d 0000 8036 0000 1e2f
      0000 8e27 0000 d51f 0000 fc17 0000 0b10 0000 0908
      0000 0000 0000 f7f7 0000 f5ef 0000 04e8 0000 2be0
      0000 72d8 0000 e2d0 0000 80c9 0000 56c2 0000 6bbb
      0000 c4b4 0000 6aae 0000 61a8 0000 b2a2 0000 619d
      0000 7398 0000 ee93 0000 d68f 0000 308c 0000 fe88
      0000 4586 0000 0684 0000 4582 0000 0381 0000 4280
      0000 0180 0000 4280 0000 0381 0000 4582 0000 0684
      0000 4586 0000 fe88 0000 308c 0000 d68f 0000 ee93
      0000 7398 0000 619d 0000 b2a2 0000 61a8 0000 6aae
      0000 c4b4 0000 6bbb 0000 56c2 0000 80c9 0000 e2d0
      0000 72d8 0000 2be0 0000 04e8 0000 f5ef 0000 f7f7
    ){441}

Of course, real-world waveform samples are not generated manually but by systems
called [ADC]. But for the sake of example, this file contains a genuine handmade
beep, playing for one second on each of the two stereo channels. The remaining
paragraphs explain how the samples in the preceding code block were obtained.

In one of its simplest forms a waveform visualizing sound takes the shape of the
sine function which is periodic, meaning it repeats its values in regular
intervals and thus can be repeated over and over to produce a constant tone. The
pitch of this tone is determined by the number of cycles of the sine function in
a given unit of time; faster repetitions produce a higher pitch. The number of
repetitions per second, the frequency, is measured in Hertz (Hz), so an 800 Hz
tone has a higher pitch than a 200 Hz tone. (Do not confuse this frequency with
the sampling rate which is also given in Hertz sometimes!)

Consider [a standard 440 Hz sine tone][A440]. Even better, consider a 441 Hz
sine tone because in this example the math works out better for 441 Hz. The
waveform corresponding to that tone has 441 cycles per second, so using a
sampling rate of 44100 samples per second one second of this tone is represented
by 441 repetitions of a pattern consisting of 44100/441 = 100 samples. Since the
sine function is periodic with period 2π the 100 samples representing one cycle
are sin(0), sin(1/100 × 2π), sin(2/100 × 2π), ..., sin(99/100 × 2π). Quantizing
these values to the ±32767 range induced by a bit depth of 16 bits (actually the
lower bound is -32768, but never mind) and encoding the numbers in little endian
[two's complement][twos-compl] representation yields the bytes in the code block
above.

Here are some example values, where sample(x) = sin(x/100 × 2π), quantized(y) =
round(y × 32767) and hex is the little endian two's complement representation of
that number.

 x |               sample | quantized |  hex
---|----------------------|-----------|-----
 0 |  0.0                 |         0 | 0000
 1 |  0.06279051952931337 |      2057 | 0908
 2 |  0.12533323356430426 |      4107 | 0b10
99 | -0.06279051952931326 |     -2057 | f7f7

So according to this table the first few samples of a 441 Hz sine tone sampled
at 44.1 kHz, 16 bit are `0000`, `0908`, `0b10`, ... But looking at the code
block above there are a lot of additional NULL bytes between the sine values
like in `0000 0000 0908 0000 0b10 0000`. Remember the samples for the two stereo
channels are interleaved, meaning this sequence represents the left channel
`0000 0908 0b10` and the right channel `0000 0000 0000`. So while the left
channel plays the 441 Hz sine tone the right channel plays sound corresponding
to a waveform consisting of a sequence of constant 0 values. This of course is
not very wavy at all, and it produces just -- silence.

And finally, just for the record, here is the Haskell program that was used to
generate the hex patterns in the code block above (something equivalent could be
done in any programming language, of course):

~~~ {.nobin}
import Data.List.Split (chunksOf)
import Text.Printf

main :: IO ()
main = putStrLn . unwords $ map le16hex samples

samples :: [Int]
samples = [round (sin (x / 100 * 2 * pi) * 32767) | x <- [0 .. 99]]

le16hex :: Int -> String
le16hex = concat . reverse . chunksOf 2 . printf "%04hx"
~~~

[waveforms]: https://pudding.cool/2018/02/waveforms/
[PCM]: https://en.wikipedia.org/wiki/Pulse-code_modulation
[sampling]: https://en.wikipedia.org/wiki/Sampling_(signal_processing)
[quantization]: https://en.wikipedia.org/wiki/Quantization_(signal_processing)
[ADC]: https://en.wikipedia.org/wiki/Analog-to-digital_converter
[A440]: https://en.wikipedia.org/wiki/A440_(pitch_standard)
[twos-compl]: https://en.wikipedia.org/wiki/Two%27s_complement

# Further reading

  * [A detailed description of the WAVE file format][gem].
  * [A concise overview of the WAVE file format][mcgill], with copies of the
    relevant format specifications.

[mcgill]: http://www-mmsp.ece.mcgill.ca/Documents/AudioFormats/WAVE/WAVE.html
[gem]: https://wavefilegem.com/how_wave_files_work.html
