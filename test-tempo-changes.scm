#!/usr/bin/guile
!#
;;; Test script to verify MIDI 1.1 tempo change functionality

(add-to-load-path (dirname (current-filename)))
(use-modules (mlg midi)
             (mlg notelist)
             (ice-9 binary-ports)
             (ice-9 optargs)
             (rnrs bytevectors)
             (rnrs io ports))

(display "Testing Tempo Change Implementation\n")
(display "====================================\n\n")

;; Create a MIDI file with multiple tempo changes
(display "Test 1: Create MIDI file with tempo changes\n")
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
    
    ;; SET_TEMPO event #1 (120 BPM = 500000 µs/quarter)
    (write-bytes! 0 #xFF #x51 #x03 #x07 #xA1 #x20)
    
    ;; NOTE_ON at tick 0
    (write-bytes! 0 #x90 60 100)
    
    ;; NOTE_OFF at tick 96 (1 quarter note later)
    (write-bytes! 96 #x80 60 64)
    
    ;; SET_TEMPO event #2 (60 BPM = 1000000 µs/quarter) at tick 96
    (write-bytes! 0 #xFF #x51 #x03 #x0F #x42 #x40)
    
    ;; NOTE_ON at tick 96 (tempo change takes effect)
    (write-bytes! 0 #x90 64 100)
    
    ;; NOTE_OFF at tick 192 (1 quarter note later, but at new tempo)
    (write-bytes! 96 #x80 64 64)
    
    ;; SET_TEMPO event #3 (240 BPM = 250000 µs/quarter) at tick 192
    (write-bytes! 0 #xFF #x51 #x03 #x03 #xD0 #x90)
    
    ;; NOTE_ON at tick 192
    (write-bytes! 0 #x90 67 100)
    
    ;; NOTE_OFF at tick 288
    (write-bytes! 96 #x80 67 64)
    
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
          (display "  - Number of tempo changes: ")
          (display (length tempoMap))
          (newline)
          
          ;; Display tempo map
          (display "\n  - Tempo map:\n")
          (for-each (lambda (tempo-entry)
                      (display "    Tick ")
                      (display (car tempo-entry))
                      (display ": ")
                      (display (cdr tempo-entry))
                      (display " µs/quarter (")
                      (display (exact->inexact (/ 60000000 (cdr tempo-entry))))
                      (display " BPM)")
                      (newline))
                    tempoMap)
          
          ;; Normalize note times
          (display "\n  - Normalizing note times with tempo changes...\n")
          (NormalizeNoteListTimes! noteList ticksPerQuarter tempoMap)
          
          ;; Display notes with their real times
          (display "\n  - Notes with real times:\n")
          (for-each (lambda (note)
                      (display "    Key ")
                      (display (note-key note))
                      (display ": start=")
                      (display (exact->inexact (note-time note)))
                      (display "s, duration=")
                      (display (exact->inexact (note-duration note)))
                      (display "s")
                      (newline))
                    noteList)
          
          ;; Verify timing
          ;; First note (120 BPM): 1 quarter = 0.5s
          ;; Second note (60 BPM): starts at 0.5s, duration = 1.0s
          ;; Third note (240 BPM): starts at 1.5s, duration = 0.25s
          (if (and (= (length noteList) 3)
                   (< (abs (- (note-time (car noteList)) 0.0)) 0.01)
                   (< (abs (- (note-duration (car noteList)) 0.5)) 0.01)
                   (< (abs (- (note-time (cadr noteList)) 0.5)) 0.01)
                   (< (abs (- (note-duration (cadr noteList)) 1.0)) 0.01)
                   (< (abs (- (note-time (caddr noteList)) 1.5)) 0.01)
                   (< (abs (- (note-duration (caddr noteList)) 0.25)) 0.01))
              (display "\n  - PASS: Tempo changes correctly applied\n")
              (display "\n  - FAIL: Tempo changes not correctly applied\n"))))
      (lambda (key . args)
        (display "  - Parse failed: ")
        (display key)
        (display " ")
        (display args)
        (newline)
        (display "  - FAIL\n")))))

(display "\n====================================\n")
(display "Testing complete!\n")
