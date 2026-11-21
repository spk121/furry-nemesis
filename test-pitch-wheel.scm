#!/usr/bin/guile
!#
;;; Test script to verify MIDI 1.1 pitch wheel functionality

(add-to-load-path (dirname (current-filename)))
(use-modules (mlg midi)
             (mlg notelist)
             (ice-9 binary-ports)
             (ice-9 optargs)
             (rnrs bytevectors)
             (rnrs io ports))

(display "Testing Pitch Wheel Implementation\n")
(display "===================================\n\n")

;; Create a MIDI file with pitch wheel events
(display "Test 1: Create MIDI file with pitch wheel and notes\n")
(let* ([bv (make-bytevector 200 0)]
       [idx 0])
  
  ;; Helper to write bytes
  (define (write-bytes! . bytes)
    (for-each (lambda (b)
                (bytevector-u8-set! bv idx b)
                (set! idx (+ idx 1)))
              bytes))
  
  ;; Write MIDI header "MThd"
  (write-bytes! #x4D #x54 #x68 #x64)
  (write-bytes! 0 0 0 6)  ; length = 6
  (write-bytes! 0 0)      ; format = 0
  (write-bytes! 0 1)      ; tracks = 1
  (write-bytes! 0 96)     ; division = 96 ticks per quarter
  
  ;; Write track header "MTrk"
  (write-bytes! #x4D #x54 #x72 #x6B)
  (let ([track-len-pos idx])
    (write-bytes! 0 0 0 0)  ; placeholder for length
    
    ;; SET_TEMPO event (120 BPM)
    (write-bytes! 0 #xFF #x51 #x03 #x07 #xA1 #x20)
    
    ;; PITCH_WHEEL event on channel 0 - bend up
    ;; Status byte E0 (pitch wheel, channel 0)
    ;; Value = 10000 (bend up) = LSB: 16, MSB: 78
    (write-bytes! 0)       ; delta time = 0
    (write-bytes! #xE0)    ; pitch wheel, channel 0
    (write-bytes! #x10 #x4E)  ; value = 10000 (bend up)
    
    ;; NOTE_ON event (middle C)
    (write-bytes! 0)       ; delta time = 0
    (write-bytes! #x90)    ; note on, channel 0
    (write-bytes! 60)      ; key = middle C
    (write-bytes! 100)     ; velocity = 100
    
    ;; NOTE_OFF event
    (write-bytes! 96)      ; delta time = 96 (one quarter note)
    (write-bytes! #x80)    ; note off, channel 0
    (write-bytes! 60)      ; key = middle C
    (write-bytes! 64)      ; velocity = 64
    
    ;; PITCH_WHEEL event - reset to center
    (write-bytes! 0)       ; delta time = 0
    (write-bytes! #xE0)    ; pitch wheel, channel 0
    (write-bytes! #x00 #x40)  ; value = 8192 (center)
    
    ;; NOTE_ON event (another note without bend)
    (write-bytes! 0)       ; delta time = 0
    (write-bytes! #x90)    ; note on, channel 0
    (write-bytes! 64)      ; key = E
    (write-bytes! 100)     ; velocity = 100
    
    ;; NOTE_OFF event
    (write-bytes! 96)      ; delta time = 96
    (write-bytes! #x80)    ; note off, channel 0
    (write-bytes! 64)      ; key = E
    (write-bytes! 64)      ; velocity = 64
    
    ;; END_OF_TRACK event
    (write-bytes! 0 #xFF #x2F #x00)
    
    ;; Update track length
    (let ([track-len (- idx track-len-pos 4)])
      (bytevector-u8-set! bv track-len-pos (ash track-len -24))
      (bytevector-u8-set! bv (+ track-len-pos 1) (logand #xFF (ash track-len -16)))
      (bytevector-u8-set! bv (+ track-len-pos 2) (logand #xFF (ash track-len -8)))
      (bytevector-u8-set! bv (+ track-len-pos 3) (logand #xFF track-len))))
  
  (let* ([final-bv (make-bytevector idx)]
         [_ (bytevector-copy! bv 0 final-bv 0 idx)]
         [in-port (open-bytevector-input-port final-bv)])
    (display "  - MIDI file created: ")
    (display idx)
    (display " bytes\n")
    (display "  - Parsing MIDI file...\n")
    
    (catch #t
      (lambda ()
        (let* ([events (ParseMidi in-port)]
               [ticksPerQuarter (let-keywords (cdar events) #t ([ticksPerQuarter 96]) ticksPerQuarter)]
               [tempoMap (MidiEvents->TempoMap events)]
               [noteList (MidiEvents->NoteList events)])
          
          (display "  - Parse successful!\n")
          (display "  - Number of events: ")
          (display (length events))
          (newline)
          (display "  - Number of notes: ")
          (display (length noteList))
          (newline)
          
          ;; Check pitch wheel events
          (let ([pitch-wheel-events (filter (lambda (e) (eq? (car e) 'PITCH_WHEEL)) events)])
            (display "  - Pitch wheel events found: ")
            (display (length pitch-wheel-events))
            (newline))
          
          ;; Check note pitch bends
          (display "\n  - Checking note pitch bends:\n")
          (let loop ([notes noteList]
                     [i 1])
            (when (not (null? notes))
              (let ([note (car notes)])
                (display "    Note ")
                (display i)
                (display ": key=")
                (display (note-key note))
                (display ", pitch-bend=")
                (display (note-pitch-bend note))
                (display " (")
                (cond
                 [(< (note-pitch-bend note) 8192) (display "bend down")]
                 [(> (note-pitch-bend note) 8192) (display "bend up")]
                 [else (display "no bend")])
                (display ")")
                (newline)
                (loop (cdr notes) (+ i 1)))))
          
          ;; Verify first note has bend up, second note has no bend
          (if (and (= (length noteList) 2)
                   (> (note-pitch-bend (car noteList)) 8192)
                   (= (note-pitch-bend (cadr noteList)) 8192))
              (display "\n  - PASS: Pitch wheel correctly applied to notes\n")
              (display "\n  - FAIL: Pitch wheel not correctly applied\n"))))
      (lambda (key . args)
        (display "  - Parse failed: ")
        (display key)
        (display " ")
        (display args)
        (newline)
        (display "  - FAIL\n")))))

(display "\n===================================\n")
(display "Testing complete!\n")
