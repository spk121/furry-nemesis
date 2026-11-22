# MIDI 1.0 / Standard MIDI File 1.1 Compliance

This document describes how furry-nemesis implements the MIDI 1.0
Detailed Specification and the Standard MIDI Files 1.0 specification
(as revised for "MIDI 1.1"), and where a chiptune renderer necessarily
departs from a full synthesizer. `test-midi-1.1.scm` checks the
behaviour described here.

The code is split into three layers:

| Module | Role |
| --- | --- |
| `mlg/midi.scm` | Standard MIDI File parser: bytes to a time-sorted event list |
| `mlg/notelist.scm` | A 16-channel MIDI receiver model: events to notes with controller curves |
| `mlg/waveform.scm` | Chiptune synthesizer: notes to 16-bit stereo (or mono) PCM |

## File format (parser)

- **Chunks**: `MThd` and `MTrk` are read using their declared lengths.
  Unknown chunk types are skipped, as the spec requires. A header
  longer than 6 bytes is accepted and the extra bytes are ignored.
  RIFF-wrapped `.rmi` files are unwrapped.
- **Formats 0, 1 and 2.** Formats 0 and 1 are merged into one
  time-ordered stream. Format 2 tracks are independent sequences, so
  they are played one after another.
- **Time division.** Both kinds are supported:
  - Metrical division (ticks per quarter note).
  - SMPTE division: 24, 25, 29.97 (30 drop-frame) or 30 fps, with
    ticks per frame. SMPTE files are timed in absolute seconds and
    ignore tempo events.
- **Variable-length quantities**: used for delta times and for the
  lengths of meta events and SysEx events.
- **Running status**: applies only to channel messages. Meta events and
  SysEx events never become the running status. A running status in
  effect before a meta or SysEx event carries on after it, which many
  real-world files rely on.
- **Robustness.** A truncated track keeps every complete event read
  before the cut. A missing End of Track is tolerated, and anything
  after End of Track is ignored.

### Meta events

| Type | Event | Notes |
| --- | --- | --- |
| 00 | Sequence Number | defaults to the track index when the data is omitted |
| 01–0F | Text, Copyright, Track Name, Instrument Name, Lyric, Marker, Cue Point, Program Name, Device Name, reserved text | |
| 20 | MIDI Channel Prefix | tags following meta and SysEx events with the channel |
| 21 | MIDI Port | |
| 2F | End of Track | |
| 51 | Set Tempo | drives tick-to-seconds conversion |
| 54 | SMPTE Offset | frame-rate bits decoded from the hours byte |
| 58 | Time Signature | any power-of-two denominator |
| 59 | Key Signature | |
| 7F | Sequencer-Specific | |
| other | kept as `META` | ignored by playback, as required |

### SysEx and escapes

- `F0 <len> <data>` is a SysEx message. When its data does not end in
  `F7`, later `F7 <len> <data>` packets continue it (`SOX_CONTINUATION`).
- Any other `F7` packet is an escape. System common and real-time
  messages inside one are decoded (for example `TIMING_CLOCK`), and
  anything else is kept as `SYSEX_ESCAPE`.
- System common and real-time status bytes are decoded if they appear
  bare in a track, and they don't disturb running status.

## Channel messages (receiver model)

Voices are allocated per **channel + key**, as on a real receiver, so
format 0 files and note-offs sent from another track work correctly.
The table shows how each message affects the sound.

| Message | Effect |
| --- | --- |
| Note On / Note Off | Note On with velocity 0 counts as Note Off. Re-striking a sounding key ends the earlier note. Notes still on at the end of the song end there. |
| Note-off velocity | Fast releases shorten the release envelope and slow ones lengthen it. 0 means no release velocity was sent. |
| Polyphonic Key Pressure | Adds vibrato to that key only. |
| Channel Pressure | Adds vibrato to the whole channel. |
| Program Change | Selects the instrument family for later notes (General MIDI families of 8). |
| Pitch Wheel | 14-bit and applied continuously, including changes in the middle of a note and during its release. |

### Controllers

| CC | Controller | Effect |
| --- | --- | --- |
| 0 / 32 | Bank Select | stored, no audible effect (one bank) |
| 1 / 33 | Modulation | vibrato depth (5.5 Hz, up to ±0.5 semitone) |
| 5 / 37 | Portamento Time | glide time, up to 3 s |
| 6 / 38 | Data Entry MSB / LSB | sets the selected RPN |
| 7 / 39 | Channel Volume | gain, using the standard 40·log10 curve |
| 8 / 40 | Balance | attenuates the opposite speaker; centre (64) leaves both at full level |
| 10 / 42 | Pan | constant-power stereo placement; centre is −3 dB in each speaker |
| 11 / 43 | Expression | gain, using the standard 40·log10 curve |
| 64 | Sustain (damper) | holds released notes until the pedal is lifted |
| 65 | Portamento On/Off | glides from the channel's previous key |
| 66 | Sostenuto | holds only the notes down when it was pressed |
| 67 | Soft Pedal | ×0.7 gain |
| 84 | Portamento Control | the next note glides from the given key |
| 96 / 97 | Data Increment / Decrement | steps the selected RPN |
| 98 / 99 | NRPN LSB / MSB | selects an NRPN; data entry for NRPNs is ignored |
| 100 / 101 | RPN LSB / MSB | selects an RPN; 127/127 is the null RPN |

Controllers 0–31 have 14-bit resolution. Writing a controller's MSB
clears its LSB, as the spec requires. Other controllers are stored.

### Registered Parameters

| RPN | Parameter | Notes |
| --- | --- | --- |
| 0 | Pitch Bend Sensitivity | MSB is semitones, LSB is cents. The initial value comes from `--bend-range` (default 2). |
| 1 | Fine Tuning | ±1 semitone, 14-bit |
| 2 | Coarse Tuning | ±64 semitones |

### Channel mode messages

| CC | Message | Behaviour |
| --- | --- | --- |
| 120 | All Sound Off | silences the channel immediately, cutting release tails and ignoring pedals |
| 121 | Reset All Controllers | resets the pitch wheel, modulation, expression, pedals, portamento, aftertouch and RPN/NRPN selection. Volume, pan, balance, program, bank and RPN values are left alone, per RP-015. |
| 122 | Local Control | recognised; it has no meaning for file playback |
| 123 | All Notes Off | releases every key, but notes held by a pedal keep sounding |
| 124 / 125 | Omni Off / On | turns all notes off; every channel is always rendered |
| 126 | Mono On | turns all notes off and puts channels N…N+M−1 in Mono mode (M = 0 means through channel 16), so each plays one note at a time |
| 127 | Poly On | turns all notes off and restores polyphony |

Mode messages are decoded whatever their data byte says, so unusual
values never stop a render.

### Universal System Exclusive

- **General MIDI System On/Off** (`7E dev 09 0n`): resets every channel
  to its power-up state.
- **Master Volume** (`7F dev 04 01 lsb msb`): global gain.
- **System Reset** (`FF` in an escape): resets every channel.

## Timing

The tempo before the first Set Tempo event is 120 BPM, or the value of
`--tempo`. Tempo changes are integrated exactly, and each tick is
looked up in the tempo map by binary search.

## Synthesis and its limits

Each note is rendered with an attack, decay, sustain and release
envelope and a square or sine-ish wave, in the original chiptune style.
Pitch, gain, vibrato and stereo position follow the curves the
receiver recorded, so a pan sweep moves a note while it sounds. Output is
interleaved stereo by default; `--mono` renders one channel and ignores pan
and balance. Pitch, gain and vibrato changes are applied the same way, and
portamento glides linearly in pitch. Percussion on channel 10 uses the
General MIDI key map (keys 35–81 mapped to noise instruments). It
follows volume, expression and pan but not pitch bend, as on GM drum kits.
The mix saturates at the 16-bit limit instead of overflowing.

Some things are recorded but can't be heard in this renderer:

- **Effects 1–5 depths (reverb, chorus and so on)**: there is no effects
  processor.
- **Bank select**: there is a single bank.
- **Omni mode and Local Control**: these are about how a live
  instrument listens to its MIDI input, which doesn't apply when
  rendering a file.
- **Timing clock, song position, song select and MTC quarter frames**:
  these synchronise live devices. Playback timing always comes from
  delta times and the tempo map.
- **Other SysEx messages**: device-specific messages are parsed and
  kept in the event list, but the renderer doesn't act on them.
