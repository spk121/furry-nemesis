furry-nemesis
=============

Hi.  This program renders a MIDI file into a stereo WAV file. Run
`./midichip.scm song.mid` to write `song.wav`.

Rendering is reasonably quick: a one-minute, four-track song takes
about a second.  Every voice is mixed straight into one preallocated
floating-point buffer for the whole song (about 21 MB per stereo
minute), which is converted to 16-bit PCM once at the end.  The first
run is slower because Guile compiles the modules and caches them.

## Command-line usage

`midichip.scm` is a command-line front end with GNU-style long options:

    ./midichip.scm song.mid                 # writes song.wav
    ./midichip.scm -o out.wav -r 22050 song.mid
    ./midichip.scm --no-drums -b 12 song.mid
    ./midichip.scm -n --dump-notes song.mid # analyse only, no audio

| Option | Meaning |
|---|---|
| `-i`, `--input=FILE` | MIDI file to read (default `tmp.midi`; may also be a positional argument) |
| `-o`, `--output=FILE` | WAV file to write (default: input name with `.wav`) |
| `-r`, `--sample-rate=HZ` | output sample rate (default 44100) |
| `-m`, `--mono` | write a mono WAV that ignores pan (the default is stereo) |
| `-b`, `--bend-range=SEMI` | pitch wheel range in semitones (default 2) |
| `-t`, `--tempo=BPM` | tempo used when the file has no tempo events (default 120) |
| `--no-drums` / `--drums-only` | skip, or render only, channel 10 percussion |
| `--dump-events`, `--dump-tempo`, `--dump-notes` | pretty-print intermediate data |
| `--trace` | print each MIDI event as it is parsed |
| `-n`, `--dry-run` | parse and analyse without rendering |
| `-v`, `--verbose` / `-q`, `--quiet` | more or less progress output |
| `-h`, `--help` / `-V`, `--version` | help and version |

With no input argument, `midichip.scm` reads `tmp.midi` and writes `tmp.wav`.

## MIDI 1.1 Support

The parser handles the whole Standard MIDI File 1.1 format: formats
0, 1 and 2, metrical and SMPTE timing, running status, every meta event,
multi-packet SysEx and F7 escapes, unknown chunks, and truncated files.

Playback goes through a 16-channel MIDI receiver model. It applies:

- the pitch wheel continuously, including mid-note bends
- RPN pitch bend sensitivity and fine/coarse tuning
- volume, expression and master volume
- pan and balance, in stereo output
- sustain, sostenuto and soft pedals
- modulation wheel and aftertouch (as vibrato)
- portamento
- All Notes Off and All Sound Off
- Reset All Controllers, Mono/Poly mode, and General MIDI reset

See [MIDI_1.1_COMPLIANCE.md](MIDI_1.1_COMPLIANCE.md) for the details and
for what a chiptune renderer can't reproduce, such as reverb.

Run the tests with:

    guile test-midi-1.1.scm
