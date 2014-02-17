#!/usr/bin/guile
!#
(add-to-load-path (dirname (current-filename)))
(use-modules (mlg midi)
             (mlg waveform)
             (mlg notelist)
             (srfi srfi-1)
             (rnrs io ports)
             (rnrs bytevectors)
             (ice-9 pretty-print))

(display "Parsing MIDI file")
(newline)
(let* ([midiEvents (ParseMidiFile "tmp.midi")]
       [ticksPerQuarter (third (car midiEvents))])
  ;; (pretty-print ticksPerQuarter)
  ;; (pretty-print midiEvents)
  (display "Extracting tempo information from the MIDI events.")
  (newline)
  (let ([tempoMap (MidiEvents->TempoMap midiEvents)])
    ;; (pretty-print tempoMap)
    (newline)
    (display "Extracting note information from the MIDI events.")
    (newline)
    (let ([noteList (MidiEvents->NoteList midiEvents)])
      ;; (pretty-print noteList)
      (display "Compute real times")
      (newline)
      (NormalizeNoteListTimes! noteList ticksPerQuarter (list (car tempoMap)))
      (pretty-print noteList)
      (display "Begin rendering to WAV...")
      (let ([wav (NoteList->PCM noteList)])
        (let ([port (open-file "tmp.wav" "wb")])
          (put-u8 port #x52)            ; R
          (put-u8 port #x49)            ; I
          (put-u8 port #x46)            ; F
          (put-u8 port #x46)            ; F
          (let ([len (+ 36 (bytevector-length wav))])
            (put-u8 port (logand #xff len))
            (put-u8 port (logand #xff (ash len -8)))
            (put-u8 port (logand #xff (ash len -16)))
            (put-u8 port (logand #xff (ash len -24))))
          (put-u8 port #x57)            ; W
          (put-u8 port #x41)            ; A
          (put-u8 port #x56)            ; V
          (put-u8 port #x45)            ; E
          (put-u8 port #x66)            ; f
          (put-u8 port #x6D)            ; m
          (put-u8 port #x74)            ; t
          (put-u8 port #x20)            ; <space>
          (put-u8 port #x10)            ; subchunk size is 16
          (put-u8 port #x00)
          (put-u8 port #x00)
          (put-u8 port #x00)
          (put-u8 port #x01)            ; audio format is 1 = PCM
          (put-u8 port #x00)
          (put-u8 port #x01)            ; num channels is 1
          (put-u8 port #x00)
          (put-u8 port #x44)            ; sample rate is 44100
          (put-u8 port #xAC)
          (put-u8 port #x00)
          (put-u8 port #x00)
          (put-u8 port #x88)            ; byte rate is 44100 * 2
          (put-u8 port #x58)
          (put-u8 port #x01)
          (put-u8 port #x00)
          (put-u8 port #x02)            ; block align is 2
          (put-u8 port #x00)            
          (put-u8 port #x10)            ; bits per sample is 16
          (put-u8 port #x00)
          (put-u8 port #x64)            ; d
          (put-u8 port #x61)            ; a
          (put-u8 port #x74)            ; t
          (put-u8 port #x61)            ; a
          (let ([len (bytevector-length wav)])
            (put-u8 port (logand #xff len))
            (put-u8 port (logand #xff (ash len -8)))
            (put-u8 port (logand #xff (ash len -16)))
            (put-u8 port (logand #xff (ash len -24))))
          (put-bytevector port wav)
          (close port))))))
