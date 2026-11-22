(define-module (mlg waveform)
  #:use-module (ice-9 optargs)
  #:use-module (mlg notelist)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:export (NoteList->PCM
            sample-rate))

;; Output sample rate in Hz.  A parameter so callers can override it
;; with `parameterize'.
(define sample-rate (make-parameter 44100))

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

(define* (GeneratePCM D_attack D_decay D_sustain D_release
                      F_initial F_attack F_sustain F_release
                      A_attack A_sustain duty noise waveform
                      #:key (pitch-curve '()) (gain-curve '())
                      (mod-curve '()) (glide #f))
  "Given an ADSR envelope, generate a waveform (12-bit signed).  The
optional curves modulate it over time: PITCH-CURVE in semitones,
GAIN-CURVE as a linear factor and MOD-CURVE as vibrato depth 0..1.
GLIDE is (semitones . seconds): start that far from the target pitch
and slide to it."
  (let* ([D_total (+ D_attack D_decay D_sustain D_release)]
         [len (inexact->exact (ceiling (* D_total (sample-rate))))]
         [buf (make-bytevector (* len 2))]
         [pitch-at (make-step-reader pitch-curve 0.0)]
         [gain-at (make-step-reader gain-curve 1.0)]
         [mod-at (make-step-reader mod-curve 0.0)])
    (define (pitch-offset t)
      (+ (pitch-at t)
         (if (and glide (< t (cdr glide)))
             (* (car glide) (- 1.0 (/ t (cdr glide))))
             0.0)
         (let ([depth (mod-at t)])
           (if (zero? depth)
               0.0
               (* depth VIBRATO_DEPTH (sin (* 2 PI VIBRATO_RATE t)))))))
    (let ([t 0.0] [t_start 0.0]
          [i 0]
          [period 0.0]
          [first #t]
          [amplitude 0.0] [frequency 0.0]
          [level-a 0.0] [level-b 0.0])
      (while (< i len)
             (if (or first (>= (- t t_start) period))
                 (begin
                   (if first
                       (set! first #f)
                       ;; else
                       (while (>= (- t t_start) period)
                              (set! t_start (+ t_start period))))
                   (cond
                    [(< t D_attack)
                     (set! amplitude    (* (/    A_attack            D_attack) t))
                     (set! frequency (+ (* (/ (- F_attack F_initial) D_attack) t) F_initial))]
                    [(< t (+ D_attack D_decay))
                     (set! amplitude (+ (* (/ (- A_sustain A_attack) D_decay) (- t D_attack)) A_attack))
                     (set! frequency (+ (* (/ (- F_sustain F_attack) D_decay) (- t D_attack)) F_attack))]
                    [(< t (+ D_attack D_decay D_sustain))
                     (set! amplitude A_sustain)
                     (set! frequency F_sustain)]
                    [(< t D_total)
                     (set! amplitude (+ (* (/ (- 0         A_sustain) D_release) (- t D_attack D_decay D_sustain)) A_sustain))
                     (set! frequency (+ (* (/ (- F_release F_sustain) D_release) (- t D_attack D_decay D_sustain)) F_sustain))]
                    [else
                     (set! amplitude 0)
                     (set! frequency F_release)])

                   ;; Apply the MIDI controller curves.
                   (set! amplitude (* amplitude (gain-at t)))
                   (set! frequency (* frequency (expt 2.0 (/ (pitch-offset t) 12.0))))
                   (set! period (/ 1.0 frequency))

                   (if (= noise 1)
                       (begin
                         (if (zero? (random 2))
                             (set! level-a (inexact->exact (round (* amplitude MAX))))
                             (set! level-a (inexact->exact (round (- (* amplitude MAX))))))
                         (if (zero? (random 2))
                             (set! level-b (inexact->exact (round (* amplitude MAX))))
                             (set! level-b (inexact->exact (round (- (* amplitude MAX)))))))
                       (begin
                         (set! level-a (inexact->exact (round (* amplitude MAX))))
                         (set! level-b (inexact->exact (round (- (* amplitude MAX)))))))))

             (if (< (- t t_start) (* period duty))
                 (if (eqv? waveform 0)
                     (bytevector-s16-native-set! buf (* i 2) level-a)
                     (bytevector-s16-native-set! buf (* i 2)
                                                 (inexact->exact
                                                  (round
                                                   (* level-a (sin (/ (* PI (- t t_start))
                                                                      (* period duty))))))))
                 ;; else
                 (if (eqv? waveform 0)
                     (bytevector-s16-native-set! buf (* i 2) level-b)
                     (bytevector-s16-native-set! buf (* i 2)
                                                 (inexact->exact (round
                                                                  (* level-b
                                                                     (sin (/ (* PI (- t t_start (* period duty)))
                                                                             (* period (1- duty))))))))))

             (set! i (1+ i))
             (set! t (+ t (/ 1.0 (sample-rate)))))
      buf)))

(define (instrument-row table name)
  (or (find (lambda (entry) (eqv? (car entry) name)) table)
      (find (lambda (entry) (eqv? (car entry) 'OTHER)) table)))

(define (release-scale velocity-end)
  "Note-off velocity shortens (fast release) or lengthens (slow
release) the release phase.  0 means no release velocity was sent."
  (if (and velocity-end (positive? velocity-end))
      (- 1.5 (/ velocity-end 127.0))
      1.0))

(define (GenerateTone note)
  (let* ([instrumentData (instrument-row tuned (vector-ref gm-families
                                                           (quotient (note-patch note) 8)))]
         [duration (note-duration note)]
         [velocity (note-velocity-begin note)]
         [d_attack (list-ref instrumentData 1)]
         [d_decay (list-ref instrumentData 2)]
         [d_sustain 0]
         [d_release (* (list-ref instrumentData 3)
                       (release-scale (note-velocity-end note)))]
         ;; Pitch bend and tuning are applied through the pitch curve.
         [freq (* 440 (expt 2.0 (/ (- (note-key note) 69) 12.0)))]
         [a1 (/ (* velocity (list-ref instrumentData 4)) 127)]
         [a2 (/ (* velocity (list-ref instrumentData 5)) 127)]
         [f1 (* freq (list-ref instrumentData 6))]
         [f2 (* freq (list-ref instrumentData 7))]
         [f3 freq]
         [f4 (* freq (list-ref instrumentData 8))]
         [wav (list-ref instrumentData 9)]
         [duty (list-ref instrumentData 10)])
    ;; Fit the attack/decay/sustain phases to the note's duration.
    (cond
     [(<= duration d_attack) (set! d_decay 0)]
     [(<= duration (+ d_attack d_decay)) (set! d_decay (- duration d_attack))]
     [else (set! d_sustain (- duration d_attack d_decay))])
    (GeneratePCM d_attack d_decay d_sustain d_release f1 f2 f3 f4 a1 a2 duty 0 wav
                 #:pitch-curve (note-pitch-curve note)
                 #:gain-curve (note-gain-curve note)
                 #:mod-curve (note-mod-curve note)
                 #:glide (note-glide note))))

(define (GenerateNoise note)
  (let* ([instrument (or (assv-ref gm-drums (note-key note)) 'OTHER)]
         [instrumentData (instrument-row untuned instrument)]
         [velocity (note-velocity-begin note)])
    (let ([d_attack (list-ref instrumentData 1)]
          [d_decay (list-ref instrumentData 2)]
          [d_sustain (list-ref instrumentData 3)]
          [d_release (list-ref instrumentData 4)]
          [a1 (/ (* velocity (list-ref instrumentData 5)) 127)]
          [a2 (/ (* velocity (list-ref instrumentData 6)) 127)]
          [f1 (list-ref instrumentData 7)]
          [f2 (list-ref instrumentData 8)]
          [f3 (list-ref instrumentData 9)]
          [f4 (list-ref instrumentData 10)]
          [wav (list-ref instrumentData 11)]
          [duty (list-ref instrumentData 12)])
      ;; Percussion ignores pitch bend and vibrato but follows volume.
      (GeneratePCM d_attack d_decay d_sustain d_release f1 f2 f3 f4 a1 a2 duty 1 wav
                   #:gain-curve (note-gain-curve note)))))

(define (AddNoteWaveform! wav note)
  (let* ([sr (sample-rate)]
         [start (* 2 (inexact->exact (floor (* (note-time note) sr))))]
         [pcm (if (note-percussion note)
                  (GenerateNoise note)
                  (GenerateTone note))]
         ;; All Sound Off cuts the note dead at its stop time.
         [limit (let ([stop (note-stop note)])
                  (if stop
                      (min (bytevector-length pcm)
                           (* 2 (inexact->exact (floor (* stop sr)))))
                      (bytevector-length pcm)))]
         [end (min limit (- (bytevector-length wav) start))])
    (do ((i 0 (+ i 2)))
        ((>= i end))
      (let ([sum (+ (bytevector-s16-ref wav (+ i start) (endianness little))
                    (bytevector-s16-ref pcm i (native-endianness)))])
        ;; Saturate rather than overflow when many voices pile up.
        (bytevector-s16-set! wav (+ i start)
                             (max -32768 (min 32767 sum))
                             (endianness little))))))

(define (FindDuration noteList)
  "Length of the song in seconds, allowing for release tails."
  (fold (lambda (n end)
          (max end (+ (note-time n) (note-duration n) 1.0)))
        0 noteList))

(define (NoteList->PCM noteList)
  "Given a list of notes, generate PCM waveform using an 8-bit-style
soft synth."
  (let* ([duration (FindDuration noteList)]
         [samples (inexact->exact (ceiling (* duration (sample-rate))))]
         [wav (make-bytevector (* 2 samples) 0)])
    (for-each (lambda (note) (AddNoteWaveform! wav note)) noteList)
    ;; Drop the silence left over from padding for release tails.
    (let loop ([n (bytevector-length wav)])
      (if (and (> n 0) (zero? (bytevector-s16-ref wav (- n 2) (endianness little))))
          (loop (- n 2))
          (if (= n (bytevector-length wav))
              wav
              (let ([out (make-bytevector n)])
                (bytevector-copy! wav 0 out 0 n)
                out))))))
