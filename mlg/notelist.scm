;;; (mlg notelist) -- turn MIDI events into a list of renderable notes
;;;
;;; MidiEvents->NoteList runs a model of a MIDI 1.0 receiver with 16
;;; channels over the event list.  It tracks every channel's program,
;;; controllers, pitch wheel, Registered Parameters, aftertouch, pedals
;;; and modes, and produces one entry per sounded note.  A note
;;; carries step-function "curves" that record how pitch, gain and
;;; vibrato depth changed while it sounded (including its release
;;; tail), so mid-note pitch bends, volume swells and so on are kept.
;;;
;;; Note layout (a list, so it pretty-prints readably):
;;;   0 time            start (ticks, or seconds after normalising)
;;;   1 duration        key-down time, including sustain-pedal holds
;;;   2 key             MIDI key number
;;;   3 patch           program number at note-on
;;;   4 percussion      #t on MIDI channel 10 (channel index 9)
;;;   5 velocity-begin  note-on velocity
;;;   6 velocity-end    note-off (release) velocity
;;;   7 pitch-bend      raw 14-bit pitch wheel value at note-on
;;;   8 channel         0-15
;;;   9 track           track index
;;;  10 pitch-curve     ((t . semitones) ...) bend + RPN tuning
;;;  11 gain-curve      ((t . factor) ...) volume, expression, pedals
;;;  12 mod-curve       ((t . depth) ...) 0..1 vibrato depth
;;;  13 glide           (semitones . seconds) portamento, or #f
;;;  14 stop            time at which sound is cut (All Sound Off), or #f
;;;
;;; Curve and stop times are absolute ticks from MidiEvents->NoteList
;;; and seconds relative to the note's start after
;;; NormalizeNoteListTimes!.

(define-module (mlg notelist)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 optargs)
  #:export (note-time
            note-duration
            note-key
            note-patch
            note-percussion
            note-velocity-begin
            note-velocity-end
            note-pitch-bend
            note-channel
            note-track
            note-pitch-curve
            note-gain-curve
            note-mod-curve
            note-glide
            note-stop

            MidiEvents->NoteList
            NormalizeNoteListTimes!
            MakeTickToSeconds
            default-tempo
            pitch-bend-range))

;; Microseconds per quarter note until the first SET_TEMPO event (or
;; for the whole song when there is none).  500000 is the MIDI
;; default of 120 BPM.
(define default-tempo (make-parameter 500000))

;; Initial pitch bend sensitivity (Registered Parameter 0) in
;; semitones.  Files can change it per channel with RPN 0.
(define pitch-bend-range (make-parameter 2.0))

;;; ------------------------------------------------------------------
;;; Note accessors

(define (field n i default)
  (if (> (length n) i) (list-ref n i) default))

(define (note-time n) (list-ref n 0))
(define (note-duration n) (list-ref n 1))
(define (note-key n) (list-ref n 2))
(define (note-patch n) (list-ref n 3))
(define (note-percussion n) (list-ref n 4))
(define (note-velocity-begin n) (list-ref n 5))
(define (note-velocity-end n) (list-ref n 6))
(define (note-pitch-bend n) (list-ref n 7))
(define (note-channel n) (field n 8 (if (note-percussion n) 9 0)))
(define (note-track n) (field n 9 0))
(define (note-pitch-curve n)
  (field n 10 (list (cons 0 (* (pitch-bend-range)
                               (/ (- (note-pitch-bend n) 8192) 8192.0))))))
(define (note-gain-curve n) (field n 11 '()))
(define (note-mod-curve n) (field n 12 '()))
(define (note-glide n) (field n 13 #f))
(define (note-stop n) (field n 14 #f))

;;; ------------------------------------------------------------------
;;; Synthesis constants

;; How long after its note-off a note keeps receiving controller
;; changes.  Longer than any release in the instrument tables.
(define TAIL_SECONDS 1.0)
;; Gain applied while the soft pedal (CC 67) is down.
(define SOFT_PEDAL_GAIN 0.7)
;; Longest portamento time, reached at CC 5 = 127.
(define MAX_PORTAMENTO_SECONDS 3.0)

;;; ------------------------------------------------------------------
;;; Channel state

(define-record-type <channel>
  (%make-channel cc patch bend pressure key-pressure rpn-data
                 param-kind mono porta-key last-key)
  channel?
  (cc ch-cc)                            ; vector of 128 controller values
  (patch ch-patch set-ch-patch!)
  (bend ch-bend set-ch-bend!)           ; raw 14-bit pitch wheel
  (pressure ch-pressure set-ch-pressure!)
  (key-pressure ch-key-pressure set-ch-key-pressure!) ; alist key -> value
  (rpn-data ch-rpn-data)                ; hash: RPN number -> (msb . lsb)
  (param-kind ch-param-kind set-ch-param-kind!) ; rpn, nrpn or #f
  (mono ch-mono set-ch-mono!)
  (porta-key ch-porta-key set-ch-porta-key!) ; CC 84 source key
  (last-key ch-last-key set-ch-last-key!))

(define RPN_BEND_RANGE 0)
(define RPN_FINE_TUNING 1)
(define RPN_COARSE_TUNING 2)
(define RPN_NULL 16383)

(define (make-channel)
  (let ([cc (make-vector 128 0)]
        [rpn (make-hash-table)])
    (vector-set! cc 7 100)              ; volume
    (vector-set! cc 10 64)              ; pan
    (vector-set! cc 11 127)             ; expression
    (vector-set! cc 100 127)            ; RPN null
    (vector-set! cc 101 127)
    (vector-set! cc 98 127)             ; NRPN null
    (vector-set! cc 99 127)
    (let* ([range (pitch-bend-range)]
           [semis (inexact->exact (floor range))]
           [cents (inexact->exact (round (* 100 (- range semis))))])
      (hash-set! rpn RPN_BEND_RANGE (cons semis cents)))
    (hash-set! rpn RPN_FINE_TUNING (cons 64 0))
    (hash-set! rpn RPN_COARSE_TUNING (cons 64 0))
    (%make-channel cc 0 8192 0 '() rpn #f #f #f #f)))

(define (cc-ref ch n) (vector-ref (ch-cc ch) n))
(define (cc14 ch msb)
  ;; A controller with 14-bit resolution: MSB n, LSB n+32.
  (+ (cc-ref ch msb) (/ (cc-ref ch (+ msb 32)) 128.0)))
(define (pedal? ch n) (>= (cc-ref ch n) 64))

(define (rpn-ref ch n) (hash-ref (ch-rpn-data ch) n '(0 . 0)))

(define (channel-pitch ch)
  "Pitch offset in semitones from the pitch wheel and RPN tuning."
  (let* ([range-data (rpn-ref ch RPN_BEND_RANGE)]
         [range (+ (car range-data) (/ (cdr range-data) 100.0))]
         [fine-data (rpn-ref ch RPN_FINE_TUNING)]
         [fine (/ (- (+ (* 128 (car fine-data)) (cdr fine-data)) 8192) 8192.0)]
         [coarse (- (car (rpn-ref ch RPN_COARSE_TUNING)) 64)])
    (+ (* range (/ (- (ch-bend ch) 8192) 8192.0)) coarse fine)))

(define (channel-gain ch master)
  "Linear gain from volume (CC 7) and expression (CC 11), using the
usual 40*log10 response curve, plus the soft pedal and master volume."
  (let ([vol (min 1.0 (/ (cc14 ch 7) 127.0))]
        [expr (min 1.0 (/ (cc14 ch 11) 127.0))])
    (* vol vol expr expr master
       (if (pedal? ch 67) SOFT_PEDAL_GAIN 1.0))))

(define (channel-mod ch key)
  "Vibrato depth 0..1 from the modulation wheel and aftertouch."
  (/ (max (min 127 (cc14 ch 1))
          (ch-pressure ch)
          (or (assv-ref (ch-key-pressure ch) key) 0))
     127.0))

(define (portamento-seconds ch)
  (* MAX_PORTAMENTO_SECONDS (expt (/ (min 127 (cc14 ch 5)) 127.0) 2)))

;;; ------------------------------------------------------------------
;;; Tone generators (sounding notes)

(define-record-type <tg>
  (%make-tg start key channel track patch perc velocity bend-raw
            pitch gain mod glide state latched end end-vel tail-end stop)
  tg?
  (start tg-start)
  (key tg-key)
  (channel tg-channel)
  (track tg-track)
  (patch tg-patch)
  (perc tg-perc)
  (velocity tg-velocity)
  (bend-raw tg-bend-raw)
  (pitch tg-pitch set-tg-pitch!)        ; reversed curves
  (gain tg-gain set-tg-gain!)
  (mod tg-mod set-tg-mod!)
  (glide tg-glide)
  (state tg-state set-tg-state!)        ; on or sustained
  (latched tg-latched set-tg-latched!)  ; held by sostenuto
  (end tg-end set-tg-end!)
  (end-vel tg-end-vel set-tg-end-vel!)
  (tail-end tg-tail-end set-tg-tail-end!)
  (stop tg-stop set-tg-stop!))

(define (curve-push curve tick val)
  "Add a point to a reversed step curve, skipping repeated values and
replacing a point at the same tick."
  (cond
   [(and (pair? curve) (= (cdar curve) val)) curve]
   [(and (pair? curve) (= (caar curve) tick))
    (let ([rest (cdr curve)])
      (if (and (pair? rest) (= (cdar rest) val))
          rest
          (cons (cons tick val) rest)))]
   [else (cons (cons tick val) curve)]))

(define (tg->note tg)
  (list (tg-start tg)
        (- (tg-end tg) (tg-start tg))
        (tg-key tg)
        (tg-patch tg)
        (tg-perc tg)
        (tg-velocity tg)
        (tg-end-vel tg)
        (tg-bend-raw tg)
        (tg-channel tg)
        (tg-track tg)
        (reverse (tg-pitch tg))
        (reverse (tg-gain tg))
        (reverse (tg-mod tg))
        (tg-glide tg)
        (tg-stop tg)))

;;; ------------------------------------------------------------------
;;; The receiver

(define (MidiEvents->NoteList midiEvents)
  "Given a MIDI events list as generated by ParseMidi, play it through
a 16-channel MIDI receiver model and return the list of notes, sorted
by start time."

  (define channels (list->vector (map (lambda (i) (make-channel)) (iota 16))))
  (define (chan n) (vector-ref channels n))

  (define header (car midiEvents))
  (define tpq (let ([m (memq #:ticksPerQuarter (cdr header))]) (if m (cadr m) 96)))
  (define smpte? (let ([m (memq #:division (cdr header))])
                   (and m (eq? (cadr m) 'SMPTE))))

  (define tempo (if smpte? 1000000 (default-tempo)))
  (define master 1.0)
  (define now 0)
  (define active '())                   ; key down or held by a pedal
  (define releasing '())                ; ended, still in release tail
  (define finished '())

  (define (tail-ticks)
    (inexact->exact (ceiling (/ (* TAIL_SECONDS 1e6 tpq) tempo))))

  (define (live-on c)
    (filter (lambda (tg) (= (tg-channel tg) c)) (append active releasing)))
  (define (active-on c)
    (filter (lambda (tg) (= (tg-channel tg) c)) active))

  (define (refresh! c)
    ;; Record the channel's current pitch, gain and vibrato on every
    ;; note it is sounding.
    (let* ([ch (chan c)]
           [pitch (channel-pitch ch)]
           [gain (channel-gain ch master)])
      (for-each (lambda (tg)
                  (unless (tg-perc tg)
                    (set-tg-pitch! tg (curve-push (tg-pitch tg) now pitch))
                    (set-tg-mod! tg (curve-push (tg-mod tg) now
                                                (channel-mod ch (tg-key tg)))))
                  (set-tg-gain! tg (curve-push (tg-gain tg) now gain)))
                (live-on c))))

  (define (refresh-all!)
    (for-each refresh! (iota 16)))

  (define (end-note! tg vel)
    (set-tg-end! tg now)
    (set-tg-end-vel! tg vel)
    (set-tg-tail-end! tg (+ now (tail-ticks)))
    (set! active (delq tg active))
    (set! releasing (cons tg releasing)))

  (define (note-off! tg vel)
    ;; Key released: the note ends unless a pedal holds it.
    (let ([ch (chan (tg-channel tg))])
      (if (or (pedal? ch 64)
              (and (pedal? ch 66) (tg-latched tg)))
          (begin
            (set-tg-state! tg 'sustained)
            (set-tg-end-vel! tg vel))
          (end-note! tg vel))))

  (define (release-held! c)
    ;; A pedal came up: end held notes no longer held by any pedal.
    (let ([ch (chan c)])
      (for-each (lambda (tg)
                  (when (and (eq? (tg-state tg) 'sustained)
                             (not (pedal? ch 64))
                             (not (and (pedal? ch 66) (tg-latched tg))))
                    (end-note! tg (tg-end-vel tg))))
                (active-on c))))

  (define (all-notes-off! c)
    (for-each (lambda (tg)
                (when (eq? (tg-state tg) 'on)
                  (note-off! tg 0)))
              (active-on c)))

  (define (all-sound-off! c)
    ;; Silence immediately, ignoring pedals and release envelopes.
    (for-each (lambda (tg) (end-note! tg 0)) (active-on c))
    (for-each (lambda (tg)
                (when (and (= (tg-channel tg) c) (not (tg-stop tg)))
                  (set-tg-stop! tg now)))
              releasing))

  (define (reset-controllers! c)
    ;; Recommended Practice RP-015: volume, pan, bank, program and
    ;; the RPN values themselves are not reset.
    (let ([ch (chan c)]
          [cc (ch-cc (chan c))])
      (set-ch-bend! ch 8192)
      (set-ch-pressure! ch 0)
      (set-ch-key-pressure! ch '())
      (for-each (lambda (n) (vector-set! cc n 0)) '(1 33 64 65 66 67 68 69))
      (vector-set! cc 11 127)
      (vector-set! cc 43 0)
      (for-each (lambda (n) (vector-set! cc n 127)) '(98 99 100 101))
      (set-ch-param-kind! ch #f)
      (for-each (lambda (tg) (set-tg-latched! tg #f)) (active-on c))
      (release-held! c)
      (refresh! c)))

  (define (system-reset!)
    ;; GM System On/Off or System Reset: every channel back to its
    ;; power-up state.
    (for-each (lambda (c) (all-sound-off! c)) (iota 16))
    (set! channels (list->vector (map (lambda (i) (make-channel)) (iota 16))))
    (set! master 1.0))

  ;; Registered / Non-Registered Parameter data entry.
  (define (data-entry! c how value)
    (let* ([ch (chan c)]
           [num (+ (* 128 (cc-ref ch 101)) (cc-ref ch 100))])
      (when (and (eq? (ch-param-kind ch) 'rpn) (not (= num RPN_NULL)))
        (let* ([old (rpn-ref ch num)]
               [msb (car old)]
               [lsb (cdr old)]
               [new
                (case how
                  [(msb) (cons value 0)]
                  [(lsb) (cons msb value)]
                  [(inc dec)
                   (let ([step (if (eq? how 'inc) 1 -1)])
                     (cond
                      ;; Bend range: LSB is cents, 0-99.
                      [(= num RPN_BEND_RANGE)
                       (let* ([total (max 0 (min (+ (* 100 127) 99)
                                                 (+ (* 100 msb) lsb step)))])
                         (cons (quotient total 100) (remainder total 100)))]
                      ;; Coarse tuning steps in whole semitones.
                      [(= num RPN_COARSE_TUNING)
                       (cons (max 0 (min 127 (+ msb step))) lsb)]
                      [else
                       (let ([v (max 0 (min 16383 (+ (* 128 msb) lsb step)))])
                         (cons (quotient v 128) (remainder v 128)))]))])])
          (hash-set! (ch-rpn-data ch) num new)
          (refresh! c)))))

  (define (control-change! c n value)
    (let* ([ch (chan c)]
           [cc (ch-cc ch)]
           [was-down (>= (vector-ref cc n) 64)])
      (vector-set! cc n value)
      ;; Writing a 14-bit controller's MSB clears its LSB.
      (when (< n 32)
        (vector-set! cc (+ n 32) 0))
      (case n
        [(1 33) (refresh! c)]            ; modulation wheel
        [(7 39 11 43) (refresh! c)]     ; volume, expression
        [(6) (data-entry! c 'msb value)]
        [(38) (data-entry! c 'lsb value)]
        [(96) (data-entry! c 'inc value)]
        [(97) (data-entry! c 'dec value)]
        [(98 99) (set-ch-param-kind! ch 'nrpn)]
        [(100 101) (set-ch-param-kind! ch 'rpn)]
        [(64)                           ; sustain (damper) pedal
         (when (and was-down (< value 64))
           (release-held! c))]
        [(66)                           ; sostenuto
         (cond
          [(and (not was-down) (>= value 64))
           (for-each (lambda (tg)
                       (when (eq? (tg-state tg) 'on)
                         (set-tg-latched! tg #t)))
                     (active-on c))]
          [(and was-down (< value 64))
           (release-held! c)
           (for-each (lambda (tg) (set-tg-latched! tg #f)) (active-on c))])]
        [(67) (refresh! c)]             ; soft pedal
        [(84) (set-ch-porta-key! ch value)]
        [else #t])))

  (define (note-on! c key velocity track)
    (let* ([ch (chan c)]
           [perc (= c 9)])
      ;; Re-striking a sounding key ends the old note first.
      (for-each (lambda (tg)
                  (when (= (tg-key tg) key)
                    (end-note! tg 0)))
                (active-on c))
      ;; A channel in Mono mode plays one note at a time.
      (when (ch-mono ch)
        (for-each (lambda (tg) (end-note! tg 0)) (active-on c)))
      (let* ([from (or (ch-porta-key ch)
                       (and (pedal? ch 65) (ch-last-key ch)))]
             [secs (portamento-seconds ch)]
             [glide (and (not perc) from (not (= from key)) (positive? secs)
                         (cons (- from key) secs))]
             [tg (%make-tg now key c track (ch-patch ch) perc velocity
                           (ch-bend ch)
                           (if perc '() (list (cons now (channel-pitch ch))))
                           (list (cons now (channel-gain ch master)))
                           (if perc '() (list (cons now (channel-mod ch key))))
                           glide (quote on) #f
                           #f 0 #f #f)])
        (set-ch-porta-key! ch #f)
        (set-ch-last-key! ch key)
        (set! active (cons tg active)))))

  (define (universal-sysex! msg)
    ;; MSG excludes the leading F0 and trailing F7.
    (cond
     ;; Universal Non-Real-Time, General MIDI On/Off: 7E dev 09 0n
     [(and (>= (length msg) 4) (= (car msg) #x7E) (= (caddr msg) #x09))
      (system-reset!)]
     ;; Universal Real-Time, Master Volume: 7F dev 04 01 lsb msb
     [(and (>= (length msg) 6) (= (car msg) #x7F)
           (= (caddr msg) #x04) (= (cadddr msg) #x01))
      (set! master (/ (+ (list-ref msg 4) (* 128 (list-ref msg 5))) 16383.0))
      (refresh-all!)]
     [else #t]))

  (define (process! event)
    (let ([type (car event)]
          [args (cdr event)])
      (define (arg k default)
        (let ([m (memq k args)]) (if m (cadr m) default)))
      (set! now (arg #:time now))
      ;; Notes whose release tail has passed take no more changes.
      (when (pair? releasing)
        (let-values ([(done still) (partition (lambda (tg) (< (tg-tail-end tg) now))
                                              releasing)])
          (set! finished (append done finished))
          (set! releasing still)))
      (let ([c (arg #:channel #f)])
        (case type
          [(NOTE_ON)
           (note-on! c (arg #:key 60) (arg #:velocity 64) (arg #:track 0))]
          [(NOTE_OFF)
           (let ([tg (find (lambda (tg)
                             (and (= (tg-channel tg) c)
                                  (= (tg-key tg) (arg #:key -1))
                                  (eq? (tg-state tg) 'on)))
                           active)])
             (when tg (note-off! tg (arg #:velocity 0))))]
          [(KEY_PRESSURE)
           (let ([ch (chan c)])
             (set-ch-key-pressure! ch (assv-set! (alist-copy (ch-key-pressure ch))
                                                 (arg #:key 0) (arg #:pressure 0)))
             (refresh! c))]
          [(CHANNEL_PRESSURE)
           (set-ch-pressure! (chan c) (arg #:pressure 0))
           (refresh! c)]
          [(PITCH_WHEEL)
           (set-ch-bend! (chan c) (arg #:value 8192))
           (refresh! c)]
          [(PROGRAM_CHANGE)
           (set-ch-patch! (chan c) (arg #:patch 0))]
          [(CONTROL_CHANGE)
           (control-change! c (arg #:controller 0) (arg #:value 0))]
          [(ALL_SOUND_OFF) (all-sound-off! c)]
          [(RESET_ALL_CONTROLLERS) (reset-controllers! c)]
          [(ALL_NOTES_OFF) (all-notes-off! c)]
          ;; Omni and Mono/Poly messages also turn all notes off.
          [(OMNI_MODE_OFF OMNI_MODE_ON) (all-notes-off! c)]
          [(MONO_MODE_ON)
           ;; The value is the number of channels, starting at this
           ;; one, to put in Mono mode; 0 means as many as possible.
           (let* ([m (arg #:value 0)]
                  [last-ch (if (zero? m) 15 (min 15 (+ c m -1)))])
             (for-each (lambda (k)
                         (all-notes-off! k)
                         (set-ch-mono! (chan k) #t))
                       (iota (1+ (- last-ch c)) c)))]
          [(POLY_MODE_ON)
           (all-notes-off! c)
           (set-ch-mono! (chan c) #f)]
          [(SET_TEMPO)
           (unless smpte?
             (set! tempo (arg #:microsecondsPerQuarter tempo)))]
          [(SOX) (universal-sysex! (arg #:message '()))]
          [(SYSTEM_RESET) (system-reset!)]
          [else #t]))))

  (for-each process! (cdr midiEvents))
  ;; Anything still sounding at the end of the song ends there.
  (for-each (lambda (tg) (end-note! tg 0)) active)
  (stable-sort (map tg->note (append finished releasing))
               (lambda (a b) (< (note-time a) (note-time b)))))

;;; ------------------------------------------------------------------
;;; Ticks to seconds

(define (MakeTickToSeconds ticksPerQuarterNote tempoMap)
  "Return a procedure converting an absolute tick to seconds, given a
tempo map as generated by MidiEvents->TempoMap.  Before the first
tempo event the tempo is (default-tempo)."
  (let* ([entries (stable-sort tempoMap (lambda (a b) (< (car a) (car b))))]
         [entries (if (or (null? entries) (positive? (caar entries)))
                      (cons (cons 0 (default-tempo)) entries)
                      entries)]
         [n (length entries)]
         [ticks (make-vector n 0)]
         [secs (make-vector n 0.0)]
         [rates (make-vector n 0.0)])  ; seconds per tick
    (let loop ([i 0] [es entries] [t0 0] [s0 0.0] [rate 0.0])
      (unless (null? es)
        (let* ([tick (caar es)]
               [s (+ s0 (* (- tick t0) rate))]
               [r (exact->inexact (/ (cdar es) ticksPerQuarterNote 1e6))])
          (vector-set! ticks i tick)
          (vector-set! secs i s)
          (vector-set! rates i r)
          (loop (1+ i) (cdr es) tick s r))))
    (lambda (tick)
      ;; Binary search for the last tempo entry at or before TICK.
      (let search ([lo 0] [hi (1- n)])
        (if (< lo hi)
            (let ([mid (quotient (+ lo hi 1) 2)])
              (if (<= (vector-ref ticks mid) tick)
                  (search mid hi)
                  (search lo (1- mid))))
            (+ (vector-ref secs lo)
               (* (- tick (vector-ref ticks lo)) (vector-ref rates lo))))))))

(define (NormalizeNoteListTimes! noteList ticksPerQuarterNote tempoMap)
  "Given a note list as generated by MidiEvents->NoteList and a tempo
map as generated by MidiEvents->TempoMap, convert the times in
noteList from MIDI ticks to seconds, handling tempo changes.  Curve
points and stop times become seconds relative to the note's start."
  (let ([->sec (MakeTickToSeconds ticksPerQuarterNote tempoMap)])
    (for-each
     (lambda (note)
       (let* ([start-tick (note-time note)]
              [start (->sec start-tick)]
              [end (->sec (+ start-tick (note-duration note)))])
         (define (relative tick) (max 0.0 (- (->sec tick) start)))
         (list-set! note 0 start)
         (list-set! note 1 (- end start))
         (when (> (length note) 14)
           (for-each (lambda (i)
                       (list-set! note i
                                  (map (lambda (pt) (cons (relative (car pt)) (cdr pt)))
                                       (list-ref note i))))
                     '(10 11 12))
           (when (note-stop note)
             (list-set! note 14 (relative (note-stop note)))))))
     noteList)))
