furry-nemesis
=============

Hi.  This program renders a file named "tmp.midi", which you must provide,
into a file named "tmp.wav".

It is really slow.  Sorry about that.  I'm using a primitive method to
generate the wav files, keeping too much data in memory.  I'll make it
more efficient when I get some spare time.

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

