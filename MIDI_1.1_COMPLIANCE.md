# MIDI 1.1 Specification Compliance

This document describes the MIDI 1.1 specification features supported by the furry-nemesis MIDI to chiptune renderer.

## Supported MIDI 1.1 Features

### Channel Voice Messages

All standard MIDI 1.1 channel voice messages are fully supported:

- **NOTE_OFF** (0x8n): Note release events
- **NOTE_ON** (0x9n): Note press events (velocity 0 treated as NOTE_OFF)
- **POLYPHONIC_AFTERTOUCH** (0xAn): Per-key pressure
- **CONTROL_CHANGE** (0xBn): Controller changes (all 128 controllers)
- **PROGRAM_CHANGE** (0xCn): Instrument selection
- **CHANNEL_AFTERTOUCH** (0xDn): Channel-wide pressure
- **PITCH_WHEEL** (0xEn): Pitch bend with ±2 semitone range (standard)

### Pitch Wheel Implementation

Pitch wheel events are properly captured and applied to notes:
- Standard 14-bit resolution (0-16383, center at 8192)
- Default range of ±2 semitones
- Applied to note frequency calculation
- Independent per-channel tracking

### Tempo Changes

Multiple tempo changes within a single MIDI file are fully supported:
- SET_TEMPO meta events (0xFF 0x51)
- Accurate time calculation across tempo boundaries
- Proper conversion from MIDI ticks to real-time seconds

### Channel Mode Messages

All MIDI 1.1 channel mode messages are recognized:

- **ALL_SOUND_OFF** (CC 120): Immediate silence
- **RESET_ALL_CONTROLLERS** (CC 121): Controller reset
- **LOCAL_CONTROL** (CC 122): Local keyboard on/off
- **ALL_NOTES_OFF** (CC 123): Note release
- **OMNI_MODE_OFF** (CC 124): Omni mode control
- **OMNI_MODE_ON** (CC 125): Omni mode control
- **MONO_MODE_ON** (CC 126): Monophonic mode
- **POLY_MODE_ON** (CC 127): Polyphonic mode

### Meta Events

All MIDI 1.1 meta events are supported:

- **0x00**: Sequence Number
- **0x01**: Text Event
- **0x02**: Copyright Notice
- **0x03**: Track Name
- **0x04**: Instrument Name
- **0x05**: Lyric
- **0x06**: Marker
- **0x07**: Cue Point
- **0x08**: Program Name (added in MIDI 1.1)
- **0x09**: Device Name (added in MIDI 1.1)
- **0x20**: MIDI Channel Prefix
- **0x21**: MIDI Port
- **0x2F**: End of Track
- **0x51**: Set Tempo
- **0x54**: SMPTE Offset
- **0x58**: Time Signature
- **0x59**: Key Signature
- **0x7F**: Sequencer Specific

### System Common Messages

- **System Exclusive** (0xF0): SysEx messages with proper EOX (0xF7) handling
- **MIDI Time Code** (0xF1): Quarter frame messages
- **Song Position Pointer** (0xF2): 14-bit beat position
- **Song Select** (0xF3): Song selection
- **Tune Request** (0xF6): Analog synthesizer tuning

### System Real-Time Messages

All MIDI 1.1 real-time messages are recognized:

- **TIMING_CLOCK** (0xF8): 24 pulses per quarter note
- **START** (0xFA): Start playback
- **CONTINUE** (0xFB): Continue from stop point
- **STOP** (0xFC): Stop playback
- **ACTIVE_SENSING** (0xFE): Connection monitoring
- **SYSTEM_RESET** (0xFF): Reset to power-up state

### File Format Support

- **Format 0**: Single multi-channel track
- **Format 1**: Multiple simultaneous tracks
- **Format 2**: Multiple independent tracks (sequential)

### Time Division

- **Metrical Time**: Ticks per quarter note (standard)
- **SMPTE Time**: Frame-based timing (parsed but converted to metrical)

## Running Status

The parser fully supports MIDI running status, where repeated messages of the same type can omit the status byte.

## Test Coverage

The implementation includes comprehensive tests for:
- Basic MIDI file parsing
- Pitch wheel event capture and application
- Multiple tempo changes with accurate timing
- Note structure with pitch bend information

## Limitations

While this implementation supports parsing and rendering all MIDI 1.1 events, some features have limited impact on the final chiptune output due to the nature of the synthesizer:

- Polyphonic and channel aftertouch events are parsed but not rendered
- Some controller changes are parsed but may not affect synthesis
- SMPTE timing is converted to metrical timing
- System real-time messages are parsed but don't affect playback timing

## Compliance Notes

This implementation is fully compliant with the MIDI 1.1 specification for:
- File format parsing
- Event recognition and classification
- Timing and synchronization
- Channel voice message handling

The synthesizer renders notes using a simplified chiptune-style synthesis that focuses on accurate pitch (including pitch wheel), timing (including tempo changes), and basic envelope control.
