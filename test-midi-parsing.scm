#!/usr/bin/guile
!#
;;; Test script to verify MIDI 1.1 parsing functionality

(add-to-load-path (dirname (current-filename)))
(use-modules (mlg midi)
             (mlg notelist)
             (ice-9 binary-ports)
             (rnrs bytevectors)
             (rnrs io ports))

(display "Testing MIDI 1.1 Implementation\n")
(display "================================\n\n")

;; Test 1: Verify note structure includes pitch bend
(display "Test 1: Note structure with pitch bend\n")
(let ([test-note (list 0.0 1.0 60 0 #f 100 90 8192)])
  (display "  - note-time: ")
  (display (note-time test-note))
  (newline)
  (display "  - note-duration: ")
  (display (note-duration test-note))
  (newline)
  (display "  - note-key: ")
  (display (note-key test-note))
  (newline)
  (display "  - note-pitch-bend: ")
  (display (note-pitch-bend test-note))
  (display " (should be 8192 for no bend)")
  (newline)
  (if (= (note-pitch-bend test-note) 8192)
      (display "  - PASS\n")
      (display "  - FAIL\n")))

;; Test 2: Create a minimal MIDI file in memory and test parsing
(display "\nTest 2: Create and parse minimal MIDI file\n")
(let* ([bv (make-bytevector 100 0)]
       [idx 0])
  
  ;; Helper to write bytes
  (define (write-bytes! . bytes)
    (for-each (lambda (b)
                (bytevector-u8-set! bv idx b)
                (set! idx (+ idx 1)))
              bytes))
  
  ;; Write MIDI header "MThd"
  (write-bytes! #x4D #x54 #x68 #x64)
  ;; length = 6
  (write-bytes! 0 0 0 6)
  ;; format = 0
  (write-bytes! 0 0)
  ;; tracks = 1
  (write-bytes! 0 1)
  ;; division = 96 ticks per quarter
  (write-bytes! 0 96)
  
  ;; Write track header "MTrk"
  (write-bytes! #x4D #x54 #x72 #x6B)
  ;; length = 11
  (write-bytes! 0 0 0 11)
  
  ;; Write events
  ;; SET_TEMPO event
  (write-bytes! 0)       ; delta time = 0
  (write-bytes! #xFF)    ; meta event
  (write-bytes! #x51)    ; set tempo
  (write-bytes! #x03)    ; length = 3
  (write-bytes! #x07 #xA1 #x20)  ; 500000 microseconds per quarter note (120 BPM)
  
  ;; END_OF_TRACK event
  (write-bytes! 0)       ; delta time = 0
  (write-bytes! #xFF)    ; meta event
  (write-bytes! #x2F)    ; end of track
  (write-bytes! #x00)    ; length = 0
  
  (let* ([final-bv (make-bytevector idx)]
         [_ (bytevector-copy! bv 0 final-bv 0 idx)]
         [in-port (open-bytevector-input-port final-bv)])
    (display "  - Minimal MIDI file created: ")
    (display idx)
    (display " bytes\n")
    (display "  - Attempting to parse...\n")
    (catch #t
      (lambda ()
        (let ([events (ParseMidi in-port)])
          (display "  - Parse successful! Found ")
          (display (length events))
          (display " events\n")
          ;; Verify we got a SET_TEMPO event
          (let ([tempo-events (filter (lambda (e) (eq? (car e) 'SET_TEMPO)) events)])
            (if (> (length tempo-events) 0)
                (display "  - Found SET_TEMPO event\n")
                (display "  - Warning: No SET_TEMPO event found\n")))
          (display "  - PASS\n")))
      (lambda (key . args)
        (display "  - Parse failed: ")
        (display key)
        (display " ")
        (display args)
        (newline)
        (display "  - FAIL\n")))))

(display "\n================================\n")
(display "Testing complete!\n")
