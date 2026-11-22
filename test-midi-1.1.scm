#!/usr/bin/guile
!#
;;; Tests for MIDI 1.0 / SMF 1.1 conformance of the parser, the
;;; receiver model in (mlg notelist) and the synthesizer.
;;;
;;; Run with:  guile --no-auto-compile test-midi-1.1.scm
;;; Exits with status 0 when every check passes.

(add-to-load-path (dirname (current-filename)))
(use-modules (mlg midi)
             (mlg notelist)
             (mlg waveform)
             (srfi srfi-1)
             (ice-9 optargs)
             (rnrs bytevectors)
             (rnrs io ports))

;;; ------------------------------------------------------------------
;;; Tiny test harness

(define failures 0)
(define checks 0)

(define (check name ok)
  (set! checks (1+ checks))
  (unless ok (set! failures (1+ failures)))
  (format #t "  ~a ~a~%" (if ok "PASS" "FAIL") name))

(define (approx= a b) (< (abs (- a b)) 1e-3))

(define-syntax-rule (section title body ...)
  (begin
    (format #t "~%~a~%" title)
    (catch #t
      (lambda () body ...)
      (lambda (key . args)
        (check (format #f "~a raised ~a ~s" title key args) #f)))))

;;; ------------------------------------------------------------------
;;; SMF builder

(define (varlen n)
  (let loop ([n (ash n -7)] [acc (list (logand n #x7F))])
    (if (zero? n)
        acc
        (loop (ash n -7) (cons (logior #x80 (logand n #x7F)) acc)))))

(define (u32 n) (list (logand #xFF (ash n -24)) (logand #xFF (ash n -16))
                      (logand #xFF (ash n -8)) (logand #xFF n)))
(define (u16 n) (list (logand #xFF (ash n -8)) (logand #xFF n)))

(define (chunk type body)
  (append (map char->integer (string->list type)) (u32 (length body)) body))

(define (track . events)
  ;; Each event is (delta byte ...).  End of Track is appended.
  (chunk "MTrk"
         (append (append-map (lambda (e) (append (varlen (car e)) (cdr e))) events)
                 '(0 #xFF #x2F 0))))

(define* (smf tracks #:key (format 1) (division 96) (extra-chunks '()))
  (u8-list->bytevector
   (append (chunk "MThd" (append (u16 format) (u16 (length tracks)) (u16 division)))
           (concatenate extra-chunks)
           (concatenate tracks))))

(define (parse bv) (ParseMidi (open-bytevector-input-port bv)))

(define (notes-of events)
  (let ([notes (MidiEvents->NoteList events)])
    (NormalizeNoteListTimes! notes (third (car events)) (MidiEvents->TempoMap events))
    notes))

(define (events-of type events)
  (filter (lambda (e) (eq? (car e) type)) events))

(define (kw ev key) (cadr (memq key ev)))

(define (curve-values curve) (map cdr curve))

(define tempo-120 '(0 #xFF #x51 3 #x07 #xA1 #x20))
(define (on ch key vel) (list 0 (logior #x90 ch) key vel))
(define (off ch key) (list 0 (logior #x80 ch) key 0))
(define (cc ch n v) (list 0 (logior #xB0 ch) n v))
(define (at delta ev) (cons delta (cdr ev)))   ; set an event's delta time

;;; ------------------------------------------------------------------
;;; Parser

(section "Running status"
  ;; Running status continues across a meta event.
  (let* ([ev (parse (smf (list (track '(0 #x90 60 100)
                                      '(0 62 100)       ; running status
                                      '(0 #xFF #x01 1 65) ; text meta
                                      '(96 60 0)        ; still running
                                      '(0 62 0)))))])
    (check "NOTE_ON via running status"
           (= 2 (length (events-of 'NOTE_ON ev))))
    (check "running status survives meta events"
           (= 2 (length (events-of 'NOTE_OFF ev))))))

(section "Meta events with long variable-length data"
  (let* ([text (make-list 200 66)]
         [ev (parse (smf (list (track (append '(0 #xFF #x03) (varlen 200) text)
                                      '(0 #xFF #x58 4 3 7 24 8)))))])
    (check "200-byte track name"
           (= 200 (string-length (kw (car (events-of 'TRACK_NAME ev)) #:text))))
    (check "time signature denominator 2^7"
           (= 128 (kw (car (events-of 'TIME_SIGNATURE ev)) #:denominator)))))

(section "System Exclusive"
  (let* ([ev (parse (smf (list (track '(0 #xF0 3 #x43 #x12 #x00) ; open packet
                                      '(0 #xF7 2 #x01 #xF7)     ; continuation
                                      '(0 #xF7 1 #xF8)          ; escaped clock
                                      '(0 #xF0 3 #x7E #x00 #xF7)))))])
    (check "multi-packet SysEx start" (not (kw (car (events-of 'SOX ev)) #:complete)))
    (check "SysEx continuation completes"
           (kw (car (events-of 'SOX_CONTINUATION ev)) #:complete))
    (check "F7 escape decodes a real-time message"
           (= 1 (length (events-of 'TIMING_CLOCK ev))))
    (check "complete SysEx strips F7"
           (equal? '(#x7E #x00) (kw (cadr (events-of 'SOX ev)) #:message)))))

(section "Chunks and robustness"
  (let* ([alien (chunk "XFIH" '(1 2 3 4 5))]
         [ev (parse (smf (list (track (on 0 60 100) (at 96 (off 0 60))))
                         #:extra-chunks (list alien)))])
    (check "unknown chunks are skipped" (= 1 (length (events-of 'NOTE_ON ev)))))
  ;; A track whose chunk is cut short and has no End of Track.
  (let* ([bv (u8-list->bytevector
              (append (chunk "MThd" (append (u16 0) (u16 1) (u16 96)))
                      (map char->integer (string->list "MTrk"))
                      (u32 100)
                      '(0 #x90 60 100 96 #x80 60)))]
         [ev (parse bv)])
    (check "truncated track keeps complete events"
           (= 1 (length (events-of 'NOTE_ON ev)))))
  ;; Channel mode messages with unusual values do not raise errors.
  (let ([ev (parse (smf (list (track (cc 0 122 64) (cc 0 124 5) (cc 0 126 2)))))])
    (check "lenient channel mode decoding"
           (and (= 1 (length (events-of 'LOCAL_CONTROL_ON ev)))
                (= 1 (length (events-of 'OMNI_MODE_OFF ev)))))))

(section "SMPTE time division"
  ;; -25 fps, 40 ticks per frame = 1000 ticks per second.
  (let* ([div (logior (ash (- 256 25) 8) 40)]
         [ev (parse (smf (list (track (on 0 60 100) (at 1000 (off 0 60))))
                         #:division div))]
         [n (car (notes-of ev))])
    (check "header reports SMPTE" (eq? 'SMPTE (MidiHeaderRef ev #:division)))
    (check "1000 ticks = 1 second" (approx= 1.0 (note-duration n))))
  (let ([ev (parse (smf (list (track)) #:division (logior (ash (- 256 29) 8) 100)))])
    (check "29.97 drop-frame rate"
           (approx= 29.97 (exact->inexact (MidiHeaderRef ev #:framesPerSecond))))))

(section "Format 2 plays tracks in sequence"
  (let* ([ev (parse (smf (list (track tempo-120 (on 0 60 100) (at 96 (off 0 60)))
                               (track (on 0 64 100) (at 96 (off 0 64))))
                         #:format 2))]
         [notes (notes-of ev)])
    (check "second pattern starts after the first"
           (approx= 0.5 (note-time (cadr notes))))))

(section "Tempo"
  ;; No tempo event until tick 96: the first beat is at 120 BPM.
  (let* ([ev (parse (smf (list (track (on 0 60 100)
                                      (at 96 (off 0 60))
                                      '(0 #xFF #x51 3 #x0F #x42 #x40) ; 60 BPM
                                      (on 0 62 100)
                                      (at 96 (off 0 62))))))]
         [notes (notes-of ev)])
    (check "default 120 BPM before first SET_TEMPO"
           (approx= 0.5 (note-duration (car notes))))
    (check "tempo change applies afterwards"
           (approx= 1.0 (note-duration (cadr notes))))))

;;; ------------------------------------------------------------------
;;; Receiver model

(section "Voices are per channel, not per track"
  ;; Format 0: same key on two channels in one track.
  (let ([notes (notes-of (parse (smf (list (track tempo-120
                                                  (on 0 60 100) (on 1 60 100)
                                                  (at 96 (off 0 60))
                                                  (at 96 (off 1 60)))))))])
    (check "two notes" (= 2 (length notes)))
    (check "independent durations"
           (equal? '(0.5 1.0) (sort (map note-duration notes) <))))
  ;; Note-off in another track ends the note on the same channel.
  (let ([notes (notes-of (parse (smf (list (track tempo-120 (on 0 60 100))
                                           (track (at 96 (off 0 60)))))))])
    (check "cross-track note-off" (approx= 0.5 (note-duration (car notes))))))

(section "Pitch wheel and Registered Parameters"
  (let* ([notes (notes-of
                 (parse (smf (list (track tempo-120
                                          ;; RPN 0 = 12 semitones
                                          (cc 0 101 0) (cc 0 100 0) (cc 0 6 12)
                                          (cc 0 101 127) (cc 0 100 127)
                                          (on 0 60 100)
                                          '(48 #xE0 #x7F #x7F) ; full bend up
                                          (at 48 (off 0 60))
                                          ;; coarse tune +3 via RPN 2
                                          (cc 0 101 0) (cc 0 100 2) (cc 0 6 67)
                                          '(0 #xE0 0 64)
                                          (on 0 60 100)
                                          (at 96 (off 0 60))
                                          ;; fine tune +50 cents via RPN 1
                                          (cc 0 100 1) (cc 0 6 96) (cc 0 38 0)
                                          (cc 0 100 2) (cc 0 6 64)
                                          (on 0 60 100)
                                          (at 96 (off 0 60)))))))]
         [bend (note-pitch-curve (first notes))])
    (check "bend starts at centre" (approx= 0.0 (cdar bend)))
    (check "mid-note bend recorded with RPN 0 range of 12"
           (approx= (* 12 (/ 8191 8192.0)) (cdadr bend)))
    (check "bend change time is relative to note start"
           (approx= 0.25 (caadr bend)))
    (check "coarse tuning" (approx= 3.0 (cdar (note-pitch-curve (second notes)))))
    (check "fine tuning" (approx= 0.5 (cdar (note-pitch-curve (third notes)))))))

(section "Data increment / decrement"
  (let ([n (car (notes-of
                 (parse (smf (list (track tempo-120
                                          (cc 0 101 0) (cc 0 100 0) (cc 0 6 2)
                                          (cc 0 96 0) (cc 0 96 0) (cc 0 97 0)
                                          '(0 #xE0 #x7F #x7F)
                                          (on 0 60 100) (at 96 (off 0 60))))))))])
    (check "RPN 0 incremented by one cent"
           (approx= (* 2.01 (/ 8191 8192.0)) (cdar (note-pitch-curve n))))))

(section "NRPN data entry does not touch RPNs"
  (let ([n (car (notes-of
                 (parse (smf (list (track tempo-120
                                          (cc 0 99 1) (cc 0 98 8) (cc 0 6 24)
                                          '(0 #xE0 #x7F #x7F)
                                          (on 0 60 100) (at 96 (off 0 60))))))))])
    (check "bend range still 2" (approx= (* 2 (/ 8191 8192.0)) (cdar (note-pitch-curve n))))))

(section "Volume, expression, soft pedal, master volume"
  (let* ([notes (notes-of
                 (parse (smf (list (track tempo-120
                                          (cc 0 7 127)
                                          (on 0 60 100)
                                          (at 24 (cc 0 11 64))
                                          (at 24 (cc 0 67 127))
                                          '(24 #xF0 7 #x7F #x7F #x04 #x01 #x7F #x3F #xF7)
                                          (at 24 (off 0 60)))))))]
         [g (curve-values (note-gain-curve (car notes)))])
    (check "full volume = gain 1" (approx= 1.0 (first g)))
    (check "expression 64" (approx= (expt (/ 64 127.0) 2) (second g)))
    (check "soft pedal" (approx= (* 0.7 (expt (/ 64 127.0) 2)) (third g)))
    (check "master volume sysex" (approx= (* 0.5 0.7 (expt (/ 64 127.0) 2)) (fourth g)))))

(section "Modulation and aftertouch"
  (let* ([n (car (notes-of
                  (parse (smf (list (track tempo-120
                                           (on 0 60 100)
                                           (at 24 (cc 0 1 127))
                                           (at 24 (cc 0 1 0))
                                           '(24 #xD0 64)
                                           '(24 #xA0 60 127)
                                           (at 24 (off 0 60))))))))]
         [m (curve-values (note-mod-curve n))])
    (check "mod wheel, channel and key pressure"
           (equal? m (list 0.0 1.0 0.0 (/ 64 127.0) 1.0)))))

(section "Sustain and sostenuto pedals"
  (let ([notes (notes-of
                (parse (smf (list (track tempo-120
                                         (cc 0 64 127)
                                         (on 0 60 100) (at 96 (off 0 60))
                                         (at 96 (cc 0 64 0))
                                         ;; sostenuto holds only 62
                                         (on 0 62 100)
                                         (at 0 (cc 0 66 127))
                                         (on 0 64 100)
                                         (at 96 (off 0 62))
                                         (at 0 (off 0 64))
                                         (at 96 (cc 0 66 0))
                                         ;; All Notes Off respects sustain
                                         (cc 0 64 127)
                                         (on 0 67 100)
                                         (at 96 (cc 0 123 0))
                                         (at 96 (cc 0 64 0)))))))])
    (define (dur key) (note-duration (find (lambda (n) (= (note-key n) key)) notes)))
    (check "sustain extends note to pedal release" (approx= 1.0 (dur 60)))
    (check "sostenuto holds note down when pressed" (approx= 1.0 (dur 62)))
    (check "sostenuto ignores later notes" (approx= 0.5 (dur 64)))
    (check "All Notes Off waits for the sustain pedal" (approx= 1.0 (dur 67)))))

(section "All Sound Off and Reset All Controllers"
  (let* ([notes (notes-of
                 (parse (smf (list (track tempo-120
                                          (cc 0 64 127)
                                          '(0 #xE0 0 0)
                                          (on 0 60 100)
                                          (at 24 (off 0 60))  ; held by pedal
                                          (at 24 (cc 0 121 0))
                                          (at 48 (cc 0 120 0)))))))]
         [n (car notes)])
    (check "Reset All Controllers centres the pitch wheel"
           (approx= 0.0 (cdr (last (note-pitch-curve n)))))
    (check "Reset All Controllers releases the sustain pedal"
           (approx= 0.25 (note-duration n)))
    (check "All Sound Off cuts the release tail" (approx= 0.5 (note-stop n)))))

(section "Re-striking, Mono mode and end of song"
  (let ([notes (notes-of
                (parse (smf (list (track tempo-120
                                         (on 0 60 100) (at 48 (on 0 60 90))
                                         (at 48 (off 0 60))
                                         (cc 0 126 1)
                                         (on 0 62 100) (at 48 (on 0 64 100))
                                         (at 48 (off 0 64))
                                         (cc 0 127 0)
                                         (on 0 65 100) (on 0 67 100)
                                         (at 96 (off 0 65))
                                         (on 1 72 100))))))])
    (define (durs key) (map note-duration (filter (lambda (n) (= (note-key n) key)) notes)))
    (check "re-struck key ends the earlier note" (equal? '(0.25 0.25) (durs 60)))
    (check "mono mode cuts the previous note" (equal? '(0.25) (durs 62)))
    (check "poly mode allows chords" (approx= 0.5 (car (durs 65))))
    (check "notes still on at end of song are kept" (= 1 (length (durs 72))))))

(section "Portamento"
  (let* ([notes (notes-of
                 (parse (smf (list (track tempo-120
                                          (cc 0 5 127) (cc 0 65 127)
                                          (on 0 60 100) (at 48 (off 0 60))
                                          (on 0 67 100) (at 48 (off 0 67))
                                          (cc 0 65 0)
                                          (cc 0 84 48)
                                          (on 0 50 100) (at 48 (off 0 50)))))))])
    (check "first note does not glide" (not (note-glide (first notes))))
    (check "glide from previous key" (equal? -7 (car (note-glide (second notes)))))
    (check "glide time from CC 5" (approx= 3.0 (cdr (note-glide (second notes)))))
    (check "portamento control (CC 84) sets the source key"
           (equal? -2 (car (note-glide (third notes)))))))

(section "General MIDI System On resets channels"
  (let ([n (cadr (notes-of
                  (parse (smf (list (track tempo-120
                                           (cc 0 7 10)
                                           '(0 #xC0 40)
                                           (on 0 60 100) (at 48 (off 0 60))
                                           '(0 #xF0 5 #x7E #x7F #x09 #x01 #xF7)
                                           (on 0 60 100) (at 48 (off 0 60))))))))])
    (check "program reset" (= 0 (note-patch n)))
    (check "volume reset" (approx= (expt (/ 100 127.0) 2) (cdar (note-gain-curve n))))))

;;; ------------------------------------------------------------------
;;; Synthesizer

(section "Rendering"
  (parameterize ([sample-rate 8000])
    (let* ([chord (append-map (lambda (k) (list (on 0 k 127))) (iota 24 48))]
           [notes (notes-of
                   (parse (smf (list (apply track
                                            tempo-120
                                            (cc 0 7 127)
                                            (append chord
                                                    (list '(48 #xE0 #x7F #x7F)
                                                          (cc 0 1 127)
                                                          (at 48 (cc 0 123 0)))))
                                     (track (on 9 36 127) (at 48 (off 9 36)))))))]
           [pcm (NoteList->PCM notes)])
      (check "24-voice chord renders without overflow" (> (bytevector-length pcm) 0))
      (check "output peaks at the 16-bit limit when saturated"
             (let loop ([i 0] [peak 0])
               (if (>= i (bytevector-length pcm))
                   (<= peak 32767)
                   (loop (+ i 2) (max peak (abs (bytevector-s16-ref pcm i (endianness little)))))))))))

(format #t "~%~a/~a checks passed~%" (- checks failures) checks)
(exit (if (zero? failures) 0 1))
