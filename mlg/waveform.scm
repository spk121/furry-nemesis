(define-module (mlg waveform)
  #:use-module (ice-9 optargs)
  #:use-module (mlg notelist)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:export (NoteList->PCM
            sample-rate
            output-channels))

;; Output sample rate in Hz.  A parameter so callers can override it
;; with `parameterize'.
(define sample-rate (make-parameter 44100))

;; 2 for interleaved stereo (left, right) following each note's pan
;; curve, or 1 for mono, which ignores pan and balance.
(define output-channels (make-parameter 2))

(define MAX #x7FF)

;; Vibrato produced by the modulation wheel and aftertouch.
(define VIBRATO_RATE 5.5)               ; Hz
(define VIBRATO_DEPTH 0.5)              ; semitones at full modulation
(define PI 3.141592653589793)

(define tuned
  ;;    NAME     => ['D_atk', 'D_dcy', 'D_rls', 'A_atk', 'A_stn', 'F_ini', 'F_atk', 'F_rls', 'Wav', 'Duty']
  '([SQUARE         0.0       0.0     0.0       1.0     1.0      1.0      1.0      1.0      0       0.5]
    [PIANO          0.01      0.6     0.01      1.0     0.5      1.02     1.01     1.0      0       0.5] 
    [CLAVI          0.01      0.3     0.01      1.0     0.3      1.01     1.00     1.00     1       0.6] 
    [ORGAN          0.01      0.1     0.05      1.0     0.9      0.99     1.00     0.99     1       0.6] 
    [STRING         0.15      0.1     0.10      1.0     0.9      1.02     1.01     1.00     0       0.5] 
    [ENSEMB         0.0       0.0     0.0       1.0     1.0      1.0      1.0      1.0      0       0.5] 
    [BRASS          0.10      0.1     0.10      1.0     0.9      0.99     1.00     0.99     0       0.5] 
    [REED           0.10      0.1     0.10      1.0     0.9      0.99     1.00     0.99     0       0.5] 
    [PIPE           0.10      0.1     0.10      1.0     0.9      0.99     1.00     0.99     1       0.5] 
    [LEAD           0.0       0.1     0.0       1.0     0.6      1.0      1.0      1.0      0       0.5] 
    [PAD            0.0       0.0     0.2       1.0     1.0      1.0      1.0      1.0      0       0.5] 
    [GUITAR         0.005     0.4     0.05      1.0     0.3      1.0      1.0      1.0      0       0.3]
    [BASS           0.005     0.2     0.03      1.0     0.7      1.0      1.0      1.0      0       0.5]
    [SYNFX          0.05      0.3     0.40      1.0     0.7      1.01     1.0      0.99     0       0.25]
    [ETHNIC         0.01      0.4     0.10      1.0     0.4      1.0      1.0      1.0      1       0.5]
    [PERCUS         0.005     0.15    0.05      1.0     0.2      1.05     1.0      0.95     0       0.5]
    [OTHER          0.10      0.2     0.30      1.0     0.9      1.03      1.0      0.98    0       0.8]))

;; General MIDI groups programs into 16 families of 8.
(define gm-families
  #(PIANO CLAVI ORGAN GUITAR BASS STRING ENSEMB BRASS
    REED PIPE LEAD PAD SYNFX ETHNIC PERCUS OTHER))

;; General MIDI percussion key map (channel 10), keys 35-81.
(define gm-drums
  '((35 . KICK) (36 . KICK) (37 . SNARE) (38 . SNARE) (39 . SNARE)
    (40 . SNARE) (41 . TOM1) (42 . HHCLOSED) (43 . TOM2) (44 . HHPEDAL)
    (45 . TOM3) (46 . HHOPEN) (47 . TOM4) (48 . TOM5) (49 . CRASH)
    (50 . TOM6) (51 . RIDE) (52 . CRASH) (53 . RIDE) (54 . HHOPEN)
    (55 . CRASH) (57 . CRASH) (59 . RIDE) (60 . TOM6) (61 . TOM5)
    (62 . TOM5) (63 . TOM4) (64 . TOM3) (65 . TOM5) (66 . TOM4)
    (69 . HHCLOSED) (70 . HHCLOSED) (73 . HHPEDAL) (74 . HHPEDAL)
    (80 . HHCLOSED) (81 . HHOPEN)))

(define untuned
  ;; D_attack, D_decay, D_sustain, D_release, - seconds
  ;; A_attack, A_sustain - ratio
  ;; F_initial, F_attack, F_sustain, F_release - in Hz
  ;; Waveform - 0 = square, 1 = sine
  ;; Duty - ratio
  ;; Noise - 1 = noise, 0 = tone
  '([KICK      0.010  0.030  0.100  0.030  1.0  0.5    150.0     87.0     70.0     70.0  1  0.5  1] 
    [SNARE     0.010  0.100  0.100  0.030  1.0  0.3    370.0    360.0    350.0     70.0  0  0.5  1] 
    [TOM1      0.010  0.030  0.100  0.030  1.0  0.5     85.0     75.0     65.0     55.0  1  0.5  1] 
    [TOM2      0.010  0.030  0.100  0.030  1.0  0.5    102.0     92.0     82.0     72.0  1  0.5  1] 
    [TOM3      0.010  0.030  0.100  0.030  1.0  0.5    124.0    114.0    104.0     94.0  1  0.5  1] 
    [TOM4      0.010  0.030  0.100  0.030  1.0  0.5    151.0    141.0    131.0    121.0  1  0.5  1] 
    [TOM5      0.010  0.030  0.100  0.030  1.0  0.5    185.0    175.0    165.0    155.0  1  0.5  1] 
    [TOM6      0.010  0.030  0.100  0.030  1.0  0.5    228.0    218.0    208.0    198.0  1  0.5  1] 
    [HHCLOSED  0.010  0.060  0.160  0.010  1.0  0.5  10000.0  10000.0  10000.0  10000.0  0  0.5  1] 
    [HHPEDAL   0.010  0.060  0.160  0.010  1.0  0.5  12000.0  12000.0  12000.0  12000.0  0  0.5  1] 
    [HHOPEN    0.010  0.060  0.630  0.010  1.0  0.5   2900.0   2900.0   2900.0   2900.0  0  0.5  1] 
    [CRASH     0.010  0.060  0.630  0.010  1.0  0.5   3700.0   3700.0   3700.0   3700.0  0  0.5  1] 
    [RIDE      0.010  0.060  0.630  0.010  1.0  0.2   3700.0   3700.0   3700.0   3700.0  0  0.5  1] 
    [OTHER     0.030  0.060  0.160  0.030  1.0  1.0   2000.0   2000.0   2000.0   2000.0  0  0.5  1]))


(define (make-step-reader curve default)
  "Return a procedure giving the value of the step function CURVE,
a list of (time . value) in ascending time, at time T.  Successive
calls must use non-decreasing T."
  (let ([rest curve])
    (lambda (t)
      (if (null? rest)
          default
          (begin
            (while (and (pair? (cdr rest)) (<= (car (cadr rest)) t))
              (set! rest (cdr rest)))
            (cdar rest))))))

;;; ------------------------------------------------------------------
;;; Voice rendering
;;;
;;; Every voice is added straight into one preallocated mix bus: a
;;; bytevector of 32-bit floats holding the whole song, with
;;; (output-channels) interleaved channels.  Nothing is allocated per
;;; note or per sample, and samples are rounded to 16-bit integers
;;; only once, when the finished bus is converted (see NoteList->PCM).

(define (bus-frames bus nch)
  (quotient (bytevector-length bus) (* 4 nch)))

(define* (GeneratePCM bus start-frame nch
                      D_attack D_decay D_sustain D_release
                      F_initial F_attack F_sustain F_release
                      A_attack A_sustain duty noise waveform
                      #:key (pitch-curve '()) (gain-curve '())
                      (mod-curve '()) (glide #f) (pan-curve '()) (stop #f))
  "Render one voice with an ADSR envelope (12-bit amplitude scale) and
add it into BUS starting at START-FRAME.  The optional curves modulate
it over time: PITCH-CURVE in semitones, GAIN-CURVE as a linear factor,
MOD-CURVE as vibrato depth 0..1 and, for stereo buses, PAN-CURVE as
(left . right) gains.  GLIDE is (semitones . seconds): start that far
from the target pitch and slide to it.  STOP, in seconds, cuts the
voice off early."
  (let* ([sr (exact->inexact (sample-rate))]
         [dt (/ 1.0 sr)]
         [D_attack (exact->inexact D_attack)]
         [D_decay (exact->inexact D_decay)]
         [D_sustain (exact->inexact D_sustain)]
         [D_release (exact->inexact D_release)]
         [D_ad (+ D_attack D_decay)]
         [D_ads (+ D_ad D_sustain)]
         [D_total (+ D_ads D_release)]
         [len (inexact->exact (ceiling (* D_total sr)))]
         [len (if stop (min len (inexact->exact (floor (* stop sr)))) len)]
         [len (max 0 (min len (- (bus-frames bus nch) start-frame)))]
         [stereo (= nch 2)]
         [square (eqv? waveform 0)]
         [noisy (= noise 1)]
         [duty (exact->inexact duty)]
         [A_attack (exact->inexact A_attack)]
         [A_sustain (exact->inexact A_sustain)]
         [pitch-at (make-step-reader pitch-curve 0.0)]
         [gain-at (make-step-reader gain-curve 1.0)]
         [mod-at (make-step-reader mod-curve 0.0)]
         ;; Pan points not yet reached, as a list of (time . gains).
         [pan-rest (if (and stereo (pair? pan-curve)) pan-curve '())]
         [pan-gains-now (pan-gains 64 64)])

    (define (pitch-offset t)
      (+ (pitch-at t)
         (if (and glide (< t (cdr glide)))
             (* (car glide) (- 1.0 (/ t (cdr glide))))
             0.0)
         (let ([depth (mod-at t)])
           (if (zero? depth)
               0.0
               (* depth VIBRATO_DEPTH (sin (* 2 PI VIBRATO_RATE t)))))))

    (define (level amplitude)
      ;; Noise voices flip the sign of each half-period at random.
      (let ([v (* amplitude MAX)])
        (if (and noisy (zero? (random 2))) (- v) v)))

    (define (boundary t t_start period first)
      ;; At the start of each waveform period, recompute the envelope,
      ;; apply the controller curves and return
      ;; (values t_start period level-a level-b).
      (let ([t_start (if first
                         t_start
                         (let advance ([ts t_start])
                           (if (>= (- t ts) period) (advance (+ ts period)) ts)))])
        (let-values
            ([(amplitude frequency)
              (cond
               [(< t D_attack)
                (values (* (/ A_attack D_attack) t)
                        (+ (* (/ (- F_attack F_initial) D_attack) t) F_initial))]
               [(< t D_ad)
                (values (+ (* (/ (- A_sustain A_attack) D_decay) (- t D_attack)) A_attack)
                        (+ (* (/ (- F_sustain F_attack) D_decay) (- t D_attack)) F_attack))]
               [(< t D_ads)
                (values A_sustain F_sustain)]
               [(< t D_total)
                (values (+ (* (/ (- 0.0 A_sustain) D_release) (- t D_attack D_decay D_sustain)) A_sustain)
                        (+ (* (/ (- F_release F_sustain) D_release) (- t D_attack D_decay D_sustain)) F_sustain))]
               [else (values 0.0 F_release)])])
          (let* ([amplitude (* amplitude (gain-at t))]
                 [frequency (* frequency (expt 2.0 (/ (pitch-offset t) 12.0)))]
                 [la (if noisy (level amplitude) (* amplitude MAX))]
                 [lb (if noisy (level amplitude) (- (* amplitude MAX)))])
            (values t_start (/ 1.0 frequency) la lb)))))

    (define (update-pan! i)
      ;; Apply every pan point at or before sample I (time i/sr, as
      ;; the step curve is sampled) and return the time of the next.
      (let loop ()
        (when (and (pair? pan-rest) (<= (caar pan-rest) (/ i sr)))
          (set! pan-gains-now (cdar pan-rest))
          (set! pan-rest (cdr pan-rest))
          (loop)))
      (if (pair? pan-rest) (exact->inexact (caar pan-rest)) +inf.0))

    ;; Render one waveform period at a time.  Each period's constants
    ;; go into a small f64 scratch bytevector and are read back before
    ;; the inner loop: values read that way are known to be floats, so
    ;; Guile's compiler keeps the per-sample arithmetic unboxed.  The
    ;; inner loop also stops at the next pan point, so a pan change
    ;; takes effect on its exact sample without disturbing the wave.
    (let ([st (make-bytevector (* 8 10))])
      (define-syntax-rule (st-ref k) (bytevector-ieee-double-native-ref st (* 8 k)))
      (define-syntax-rule (st-set! k v) (bytevector-ieee-double-native-set! st (* 8 k) v))
      (st-set! 0 dt)
      (st-set! 6 duty)
      (st-set! 8 sr)
      (let outer ([i 0] [t_start 0.0] [period 0.0] [first #t] [pan-only #f])
        (when (< i len)
          (let-values ([(ts p a b) (if pan-only
                                       (values t_start period (st-ref 3) (st-ref 4))
                                       (boundary (* i dt) t_start period first))])
            (let ([next-pan (update-pan! i)])
              (st-set! 1 ts)
              (st-set! 2 p)
              (st-set! 3 a)
              (st-set! 4 b)
              (st-set! 5 (if stereo (car pan-gains-now) 1.0))
              (st-set! 7 (if stereo (cdr pan-gains-now) 0.0))
              (st-set! 9 next-pan))
            (let ([next
                   (let ([dt (st-ref 0)] [ts (st-ref 1)] [p (st-ref 2)]
                         [la (st-ref 3)] [lb (st-ref 4)] [gl (st-ref 5)]
                         [duty (st-ref 6)] [gr (st-ref 7)] [sr (st-ref 8)]
                         [next-pan (st-ref 9)])
                     (let* ([high-len (* p duty)]
                            [low-len (* p (- duty 1.0))])
                       ;; Samples up to the end of this period or the
                       ;; next pan point, whichever comes first.
                       (let inner ([j i])
                         (if (>= j len)
                             j
                             (let ([phase (- (* j dt) ts)])
                               (if (and (> j i)
                                        (or (>= phase p) (>= (/ j sr) next-pan)))
                                   j
                                   (let ([s (cond
                                             [(< phase high-len)
                                              (if square la
                                                  (* la (sin (/ (* PI phase) high-len))))]
                                             [square lb]
                                             [else (* lb (sin (/ (* PI (- phase high-len))
                                                                 low-len)))])]
                                         [offset (* 4 nch (+ start-frame j))])
                                     (bytevector-ieee-single-native-set!
                                      bus offset
                                      (+ (bytevector-ieee-single-native-ref bus offset) (* s gl)))
                                     (when stereo
                                       (let ([offset (+ offset 4)])
                                         (bytevector-ieee-single-native-set!
                                          bus offset
                                          (+ (bytevector-ieee-single-native-ref bus offset)
                                             (* s gr)))))
                                     (inner (1+ j)))))))))])
              ;; If the period has not ended, only the pan changed.
              (outer next ts p #f
                     (< (- (* next dt) ts) p)))))))))

(define (instrument-row table name)
  (or (find (lambda (entry) (eqv? (car entry) name)) table)
      (find (lambda (entry) (eqv? (car entry) 'OTHER)) table)))

(define (release-scale velocity-end)
  "Note-off velocity shortens (fast release) or lengthens (slow
release) the release phase.  0 means no release velocity was sent."
  (if (and velocity-end (positive? velocity-end))
      (- 1.5 (/ velocity-end 127.0))
      1.0))

(define (GenerateTone bus start-frame nch note)
  (let* ([row (list->vector (instrument-row tuned (vector-ref gm-families
                                                              (quotient (note-patch note) 8))))]
         [duration (note-duration note)]
         [velocity (note-velocity-begin note)]
         [d_attack (vector-ref row 1)]
         [d_decay (vector-ref row 2)]
         [d_sustain 0]
         [d_release (* (vector-ref row 3) (release-scale (note-velocity-end note)))]
         ;; Pitch bend and tuning are applied through the pitch curve.
         [freq (* 440 (expt 2.0 (/ (- (note-key note) 69) 12.0)))])
    ;; Fit the attack/decay/sustain phases to the note's duration.
    (cond
     [(<= duration d_attack) (set! d_decay 0)]
     [(<= duration (+ d_attack d_decay)) (set! d_decay (- duration d_attack))]
     [else (set! d_sustain (- duration d_attack d_decay))])
    (GeneratePCM bus start-frame nch
                 d_attack d_decay d_sustain d_release
                 (* freq (vector-ref row 6)) (* freq (vector-ref row 7))
                 freq (* freq (vector-ref row 8))
                 (/ (* velocity (vector-ref row 4)) 127)
                 (/ (* velocity (vector-ref row 5)) 127)
                 (vector-ref row 10) 0 (vector-ref row 9)
                 #:pitch-curve (note-pitch-curve note)
                 #:gain-curve (note-gain-curve note)
                 #:mod-curve (note-mod-curve note)
                 #:glide (note-glide note)
                 #:pan-curve (note-pan-curve note)
                 #:stop (note-stop note))))

(define (GenerateNoise bus start-frame nch note)
  (let* ([instrument (or (assv-ref gm-drums (note-key note)) 'OTHER)]
         [row (list->vector (instrument-row untuned instrument))]
         [velocity (note-velocity-begin note)])
    ;; Percussion ignores pitch bend and vibrato but follows volume
    ;; and pan.
    (GeneratePCM bus start-frame nch
                 (vector-ref row 1) (vector-ref row 2) (vector-ref row 3) (vector-ref row 4)
                 (vector-ref row 7) (vector-ref row 8) (vector-ref row 9) (vector-ref row 10)
                 (/ (* velocity (vector-ref row 5)) 127)
                 (/ (* velocity (vector-ref row 6)) 127)
                 (vector-ref row 12) 1 (vector-ref row 11)
                 #:gain-curve (note-gain-curve note)
                 #:pan-curve (note-pan-curve note)
                 #:stop (note-stop note))))

(define (AddNoteWaveform! bus note)
  "Mix NOTE into BUS, a float mix bus with (output-channels)
interleaved channels."
  (let ([start-frame (inexact->exact (floor (* (note-time note) (sample-rate))))]
        [nch (output-channels)])
    (if (note-percussion note)
        (GenerateNoise bus start-frame nch note)
        (GenerateTone bus start-frame nch note))))

(define (FindDuration noteList)
  "Length of the song in seconds, allowing for release tails."
  (fold (lambda (n end)
          (max end (+ (note-time n) (note-duration n) 1.0)))
        0 noteList))

(define (bus->s16 bus nch)
  "Convert the float mix bus to little-endian 16-bit PCM, saturating
at the 16-bit limits and dropping trailing silence (whole frames)."
  (let* ([samples (quotient (bytevector-length bus) 4)]
         ;; Anything under half a step rounds to zero.
         [audible? (lambda (j)
                     (> (abs (bytevector-ieee-single-native-ref bus (* 4 j))) 0.5))]
         ;; Index just past the last audible sample, rounded up to a
         ;; whole frame.
         [end (let scan ([j (1- samples)])
                (cond
                 [(< j 0) 0]
                 [(audible? j) (* nch (1+ (quotient j nch)))]
                 [else (scan (1- j))]))]
         [out (make-bytevector (* 2 end))]
         ;; Float-to-integer without generic arithmetic: adding
         ;; 1.5 * 2^52 to a double leaves round-half-even(x) in the
         ;; low bits of its bit pattern (exact for |x| < 2^31), which
         ;; are then read back as a 16-bit integer.  This matches
         ;; (inexact->exact (round x)) and keeps the loop unboxed.
         [scratch (make-bytevector 8)]
         [low (if (eq? (native-endianness) (endianness little)) 0 6)])
    (let loop ([j 0])
      (when (< j end)
        (let ([x (bytevector-ieee-single-native-ref bus (* 4 j))])
          (bytevector-ieee-double-native-set!
           scratch 0 (+ (cond [(> x 32767.0) 32767.0]
                              [(< x -32768.0) -32768.0]
                              [else x])
                        6755399441055744.0))
          (bytevector-s16-set! out (* 2 j) (bytevector-s16-native-ref scratch low)
                               (endianness little)))
        (loop (1+ j))))
    out))

(define (NoteList->PCM noteList)
  "Given a list of notes, generate a little-endian 16-bit PCM buffer
using an 8-bit-style soft synth.  The buffer has (output-channels)
interleaved channels at (sample-rate)."
  (let* ([nch (output-channels)]
         [frames (inexact->exact (ceiling (* (FindDuration noteList) (sample-rate))))]
         ;; The whole song's mix bus, allocated once and zero-filled
         ;; (all-zero bytes are 0.0 as IEEE floats).
         [bus (make-bytevector (* 4 nch frames) 0)])
    (for-each (lambda (note) (AddNoteWaveform! bus note)) noteList)
    (bus->s16 bus nch)))
