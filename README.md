furry-nemesis
=============

Hi.  This program renders a file named "tmp.midi", which you must provide,
into a file named "tmp.wav".

It is really slow.  Sorry about that.  I'm using a primitive method to
generate the wav files, keeping too much data in memory.  I'll make it
more efficient when I get some spare time.

## Command-line usage

`main.scm` is a command-line front end with GNU-style long options:

    ./main.scm song.mid                 # writes song.wav
    ./main.scm -o out.wav -r 22050 song.mid
    ./main.scm --no-drums -b 12 song.mid
    ./main.scm -n --dump-notes song.mid # analyse only, no audio

| Option | Meaning |
|---|---|
| `-i`, `--input=FILE` | MIDI file to read (default `tmp.midi`; may also be a positional argument) |
| `-o`, `--output=FILE` | WAV file to write (default: input name with `.wav`) |
| `-r`, `--sample-rate=HZ` | output sample rate (default 44100) |
| `-b`, `--bend-range=SEMI` | pitch wheel range in semitones (default 2) |
| `-t`, `--tempo=BPM` | tempo used when the file has no tempo events (default 120) |
| `--no-drums` / `--drums-only` | skip, or render only, channel 10 percussion |
| `--dump-events`, `--dump-tempo`, `--dump-notes` | pretty-print intermediate data |
| `--trace` | print each MIDI event as it is parsed |
| `-n`, `--dry-run` | parse and analyse without rendering |
| `-v`, `--verbose` / `-q`, `--quiet` | more or less progress output |
| `-h`, `--help` / `-V`, `--version` | help and version |

`potluck2014.scm` still renders `tmp.midi` to `tmp.wav` with no options.

## MIDI 1.1 Support

This renderer now supports the MIDI 1.1 specification, including:

- **Pitch wheel control**: Pitch bend events are properly applied to notes with
  a standard ±2 semitone range.
- **Tempo changes**: Multiple tempo changes within a song are correctly handled.
- **Extended meta events**: Support for all MIDI 1.1 meta events including:
  - Text events (text, copyright, track name, instrument name, lyrics, markers, cue points)
  - Program/Device name events
  - MIDI Channel Prefix
  - Sequencer-specific events
- **Complete controller support**: All channel mode messages and control changes
- **System Exclusive messages**: Proper parsing and handling of SysEx messages

