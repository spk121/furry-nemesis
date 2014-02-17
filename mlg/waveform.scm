(define-module (mlg waveform)
  :use-module (ice-9 optargs)
  :use-module (mlg notelist)
  :use-module (rnrs bytevectors)
  :use-module (srfi srfi-1)
  :export (NoteList->PCM))

(define SAMPLE_RATE 44100)
(define MAX #x7FF)

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
    [OTHER          0.10      0.2     0.30      1.0     0.9      1.03      1.0      0.98    0       0.8]))

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


(define (GeneratePCM D_attack D_decay D_sustain D_release
                     F_initial F_attack F_sustain F_release
                     A_attack A_sustain duty noise waveform)
  "Given an ADSR envelope, generate a waveform (12-bit signed)"
  (let* ([D_total (+ D_attack D_decay D_sustain D_release)]
         [len (inexact->exact (ceiling (* D_total SAMPLE_RATE)))]
         [buf (make-bytevector (* len 2))])
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
                     ;; (format #t "in attack t ~a d_attack ~a a_attack ~a~%" t D_attack A_attack) (newline)
                     (set! amplitude    (* (/    A_attack            D_attack) t))
                     (set! frequency (+ (* (/ (- F_attack F_initial) D_attack) t) F_initial))]
                    [(< t (+ D_attack D_decay))
                     ;; (display "in decay") (newline)
                     (set! amplitude (+ (* (/ (- A_sustain A_attack) D_decay) (- t D_attack)) A_attack))
                     (set! frequency (+ (* (/ (- F_sustain F_attack) D_decay) (- t D_attack)) F_attack))]
                    [(< t (+ D_attack D_decay D_sustain))
                     ;; (display "in sustain") (newline)
                     (set! amplitude A_sustain)
                     (set! frequency F_sustain)]
                    [(< t D_total)
                     ;; (display "in release") (newline)
                     (set! amplitude (+ (* (/ (- 0         A_sustain) D_release) (- t D_attack D_decay D_sustain)) A_sustain))
                     (set! frequency (+ (* (/ (- F_release F_sustain) D_release) (- t D_attack D_decay D_sustain)) F_sustain))]
                    [else
                     (set! amplitude 0)
                     (set! frequency F_release)])
                   
                   (set! period (/ 1.0 frequency))
                   
                   ;; (format #t "IN generatepcm amplitude ~a frequency ~a t ~a D_attack ~a~%" amplitude frequency t D_attack)

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
                                                   (* level-a (sin (/ (* 3.1415 (- t t_start))
                                                                      (* period duty))))))))
                 ;; else
                 (if (eqv? waveform 0)
                     (bytevector-s16-native-set! buf (* i 2) level-b)
                     (bytevector-s16-native-set! buf (* i 2)
                                                 (inexact->exact (round 
                                                                  (* level-b
                                                                     (sin (/ (* 3.1415 (- t t_start (* period duty)))
                                                                             (* period (1- duty))))))))))
             
             (set! i (1+ i))
             (set! t (+ t (/ 1.0 SAMPLE_RATE))))
      buf)))

(define (GenerateTone time instrument note duration velocity)
  (let ([instrumentData (find
                         (lambda (entry)
                           (eqv? (car entry) instrument))
                         tuned)])
    (let ([d_attack (list-ref instrumentData 1)]
          [d_decay (list-ref instrumentData 2)]
          [tmp_decay (list-ref instrumentData 2)]
          [d_sustain 0]
          [d_release (list-ref instrumentData 3)]
          [freq (* 440 (expt 2.0 (/ (- note 69) 12.0)))]
          [a1 (/ (* velocity (list-ref instrumentData 4)) 127)]
          [a2 (/ (* velocity (list-ref instrumentData 5)) 127)]
          [f1 (list-ref instrumentData 6)]
          [f2 (list-ref instrumentData 7)]
          [f3 1.0]
          [f4 (list-ref instrumentData 8)]
          [wav (list-ref instrumentData 9)]
          [duty (list-ref instrumentData 10)])
      (if (<= duration d_attack)
          (set! tmp_decay 0)
          (if (<= duration (+ d_attack d_decay))
              (set! tmp_decay (- duration d_attack))
              (set! d_sustain (- duration d_attack d_decay))))
      (set! d_decay tmp_decay)
      (set! f1 (* f1 freq))
      (set! f2 (* f2 freq))
      (set! f3 (* f3 freq))
      (set! f4 (* f4 freq))
      (GeneratePCM d_attack d_decay d_sustain d_release f1 f2 f3 f4 a1 a2 duty 0 wav))))

(define (GenerateNoise time instrument velocity)
  
  (let ([instrumentData (find (lambda (entry) (eqv? (car entry) instrument)) untuned)])
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
      (GeneratePCM d_attack d_decay d_sustain d_release f1 f2 f3 f4 a1 a2 duty 1 wav))))

(define (AddNoteWaveform! wav note)
  (let ([instrument 'OTHER]
        [start 0]
        [pcm #f])
    (let ([time (note-time note)]
          [duration (note-duration note)]
          [key (note-key note)]
          [patch (note-patch note)]
          [percussion (note-percussion note)]
          [velocity-begin (note-velocity-begin note)]
          [velocity-end (note-velocity-end note)])
      (set! start (* 2 (inexact->exact (floor (* time SAMPLE_RATE)))))
      (if (not percussion)
          (begin 
            (cond
             [(< patch  8) (set! instrument 'PIANO)]
             [(< patch 16) (set! instrument 'CLAVI)]
             [(< patch 24) (set! instrument 'ORGAN)]
             [(< patch 32) (set! instrument 'STRING)]
             [(< patch 40) (set! instrument 'ENSEMB)]
             [(< patch 48) (set! instrument 'BRASS)]
             [(< patch 56) (set! instrument 'REED)]
             [(< patch 64) (set! instrument 'PIPE)]
             [(< patch 72) (set! instrument 'LEAD)]
             [(< patch 80) (set! instrument 'PAD)]
             [else (set! instrument 'OTHER)])
            (set! pcm (GenerateTone time instrument key duration velocity-begin)))
          ;; else
          (begin
            (cond
             [(= key 35) (set! instrument 'KICK)]
             [(= key 36) (set! instrument 'KICK)]
             [(= key 38) (set! instrument 'SNARE)]
             [(= key 40) (set! instrument 'SNARE)]
             [(= key 41) (set! instrument 'TOM1)]
             [(= key 43) (set! instrument 'TOM2)]
             [(= key 45) (set! instrument 'TOM3)]
             [(= key 47) (set! instrument 'TOM4)]
             [(= key 48) (set! instrument 'TOM5)]
             [(= key 50) (set! instrument 'TOM6)]
             [(= key 42) (set! instrument 'HHCLOSED)]
             [(= key 44) (set! instrument 'HHPEDAL)]
             [(= key 46) (set! instrument 'HHOPEN)]
             [(= key 49) (set! instrument 'CRASH)]
             [(= key 51) (set! instrument 'RIDE)]
             [else (set! instrument 'OTHER)])
            (set! pcm (GenerateNoise time instrument velocity-begin))))
      (do ((i 0 (+ i 2)))
          ((or (>= i (bytevector-length pcm))
               (>= (+ i start)
                   (bytevector-length wav))))
        (let ([prev (bytevector-s16-ref wav (+ i start) 'little)]
              [cur (bytevector-s16-ref pcm i 'little)])
          (bytevector-sint-set! wav (+ i start) (+ prev cur) 'little 2)))
      )))


(define (FindDuration noteList)
  (let loop ([nl noteList]
             [end 0])
    ;; (format #t "Dur END ~a NL ~a~%" end nl)
    (flush-all-ports)
    (if (not (null? nl))
        (begin

          (let ([time (note-time (car nl))]
                [duration (note-duration (car nl))])
            (loop (cdr nl) (max end (+ time duration)))))
        end)))

(define (NoteList->PCM noteList)
  "Given a list of notes, generate PCM waveform using an 8-bit-style
soft synth."
  (let* ([duration (FindDuration noteList)]
         [samples (inexact->exact (ceiling (* duration SAMPLE_RATE)))]
         [wav (make-bytevector (* 2 samples) 0)])
    (let loop ([currentNote (car noteList)]
               [remainingNotes (cdr noteList)])
      (AddNoteWaveform! wav currentNote)
      (if (not (null? remainingNotes))
          (loop (car remainingNotes) (cdr remainingNotes))
          wav))))

