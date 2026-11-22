#!/usr/bin/env -S guile --no-auto-compile -e main -s
!#
;;; main.scm -- command-line front end for the furry-nemesis MIDI
;;; chiptune renderer.
;;;
;;; Usage: ./main.scm [OPTION]... [INPUT.mid]
;;; Run with --help for the full list of options.

(add-to-load-path (dirname (current-filename)))
(use-modules (mlg midi)
             (mlg waveform)
             (mlg notelist)
             (srfi srfi-1)
             (ice-9 getopt-long)
             (ice-9 format)
             (ice-9 pretty-print)
             (rnrs io ports)
             (rnrs bytevectors))

(define program-name "furry-nemesis")
(define program-version "0.2")

(define default-input "tmp.midi")

;;; ------------------------------------------------------------------
;;; Option handling

(define (positive-number? s)
  (let ([n (string->number s)])
    (and n (real? n) (positive? n))))

(define option-spec
  `((help          (single-char #\h) (value #f))
    (version       (single-char #\V) (value #f))
    (input         (single-char #\i) (value #t))
    (output        (single-char #\o) (value #t))
    (sample-rate   (single-char #\r) (value #t) (predicate ,positive-number?))
    (bend-range    (single-char #\b) (value #t) (predicate ,positive-number?))
    (tempo         (single-char #\t) (value #t) (predicate ,positive-number?))
    (no-drums      (value #f))
    (drums-only    (value #f))
    (dump-events   (value #f))
    (dump-tempo    (value #f))
    (dump-notes    (value #f))
    (trace         (value #f))
    (dry-run       (single-char #\n) (value #f))
    (verbose       (single-char #\v) (value #f))
    (quiet         (single-char #\q) (value #f))))

(define (show-help)
  (format #t "Usage: ~a [OPTION]... [INPUT]
Render a Standard MIDI File to a 16-bit mono WAV file using an
8-bit-style chiptune soft synth.

Input/output:
  -i, --input=FILE        MIDI file to read (default: ~a).  The input
                          may also be given as a positional argument.
  -o, --output=FILE       WAV file to write (default: INPUT with its
                          extension replaced by .wav).

Synthesis:
  -r, --sample-rate=HZ    output sample rate in Hz (default: ~a)
  -b, --bend-range=SEMI   pitch wheel range in semitones (default: ~a)
  -t, --tempo=BPM         tempo used when the file has no SET_TEMPO
                          events (default: ~a)
      --no-drums          skip percussion (MIDI channel 10) notes
      --drums-only        render only percussion notes

Diagnostics:
      --dump-events       pretty-print the parsed MIDI events
      --dump-tempo        pretty-print the tempo map
      --dump-notes        pretty-print the note list (times in seconds)
      --trace             print each MIDI event as the parser reads it
  -n, --dry-run           parse and analyse, but do not render audio
  -v, --verbose           report progress and statistics
  -q, --quiet             print nothing except errors

  -h, --help              show this help and exit
  -V, --version           show version information and exit
"
          program-name default-input
          (sample-rate) (pitch-bend-range)
          (/ 60e6 (default-tempo))))

(define (show-version)
  (format #t "~a ~a~%" program-name program-version))

(define (die fmt . args)
  (apply format (current-error-port)
         (string-append program-name ": " fmt "~%") args)
  (exit 1))

(define (option-number options key default)
  (let ([v (option-ref options key #f)])
    (if v (exact->inexact (string->number v)) default)))

(define (replace-extension filename new-ext)
  (let* ([base (basename filename)]
         [dot (string-rindex base #\.)]
         [stem (if (and dot (> dot 0)) (substring base 0 dot) base)]
         [dir (dirname filename)])
    (string-append (if (string=? dir ".") "" (string-append dir "/"))
                   stem new-ext)))

;;; ------------------------------------------------------------------
;;; WAV output

(define (write-wav filename pcm rate)
  "Write the 16-bit mono PCM bytevector PCM to FILENAME as a RIFF WAV
file at sample rate RATE."
  (let ([port (open-file-output-port filename (file-options no-fail))]
        [header (make-bytevector 44 0)]
        [data-len (bytevector-length pcm)])
    (define (put-str! offset str)
      (bytevector-copy! (string->utf8 str) 0 header offset (string-length str)))
    (define (put-u32! offset n)
      (bytevector-u32-set! header offset n (endianness little)))
    (define (put-u16! offset n)
      (bytevector-u16-set! header offset n (endianness little)))
    (put-str! 0 "RIFF")
    (put-u32! 4 (+ 36 data-len))
    (put-str! 8 "WAVE")
    (put-str! 12 "fmt ")
    (put-u32! 16 16)                    ; fmt chunk size
    (put-u16! 20 1)                     ; PCM
    (put-u16! 22 1)                     ; mono
    (put-u32! 24 rate)                  ; sample rate
    (put-u32! 28 (* rate 2))            ; byte rate
    (put-u16! 32 2)                     ; block align
    (put-u16! 34 16)                    ; bits per sample
    (put-str! 36 "data")
    (put-u32! 40 data-len)
    (put-bytevector port header)
    (put-bytevector port pcm)
    (close-port port)))

;;; ------------------------------------------------------------------
;;; Main

(define (main args)
  ;; getopt-long reports unknown options and failed predicates itself
  ;; and exits with status 1.
  (let* ([options (getopt-long args option-spec)]
         [positional (option-ref options '() '())])

    (when (option-ref options 'help #f)
      (show-help)
      (exit 0))
    (when (option-ref options 'version #f)
      (show-version)
      (exit 0))

    (let* ([quiet (option-ref options 'quiet #f)]
           [verbose (and (not quiet) (option-ref options 'verbose #f))]
           [input (or (option-ref options 'input #f)
                      (and (pair? positional) (car positional))
                      default-input)]
           [output (or (option-ref options 'output #f)
                       (replace-extension input ".wav"))]
           [rate (inexact->exact
                  (round (option-number options 'sample-rate (sample-rate))))]
           [bend (option-number options 'bend-range (pitch-bend-range))]
           [bpm (option-number options 'tempo (/ 60e6 (default-tempo)))]
           [no-drums (option-ref options 'no-drums #f)]
           [drums-only (option-ref options 'drums-only #f)]
           [dry-run (option-ref options 'dry-run #f)])

      (define (say fmt . args)
        (unless quiet
          (apply format #t fmt args)
          (force-output)))
      (define (detail fmt . args)
        (when verbose
          (apply format #t fmt args)
          (force-output)))

      (when (and (option-ref options 'input #f) (pair? positional))
        (die "input given both with --input and as an argument"))
      (when (> (length positional) 1)
        (die "too many arguments: ~{~a~^ ~}" positional))
      (when (and no-drums drums-only)
        (die "--no-drums and --drums-only are mutually exclusive"))
      (unless (file-exists? input)
        (die "cannot find input file '~a'" input))
      (when (and (not dry-run) (string=? input output))
        (die "output file would overwrite input file '~a'" input))

      (parameterize ([sample-rate rate]
                     [pitch-bend-range bend]
                     [default-tempo (inexact->exact (round (/ 60e6 bpm)))]
                     [midi-trace (option-ref options 'trace #f)])

        (say "Parsing MIDI file ~a~%" input)
        (let* ([midiEvents (catch #t
                             (lambda () (ParseMidiFile input))
                             (lambda (key . rest)
                               (die "failed to parse '~a' (~a)" input key)))]
               [ticksPerQuarter (third (car midiEvents))])
          (detail "  ~a events, ~a ticks per quarter note~%"
                  (length midiEvents) ticksPerQuarter)
          (when (option-ref options 'dump-events #f)
            (pretty-print midiEvents))

          (say "Extracting tempo information~%")
          (let ([tempoMap (MidiEvents->TempoMap midiEvents)])
            (if (null? tempoMap)
                (detail "  no tempo events; using ~a BPM~%" bpm)
                (detail "  ~a tempo change~:p~%" (length tempoMap)))
            (when (option-ref options 'dump-tempo #f)
              (pretty-print tempoMap))

            (say "Extracting notes~%")
            (let ([noteList
                   (filter (lambda (n)
                             (cond [no-drums (not (note-percussion n))]
                                   [drums-only (note-percussion n)]
                                   [else #t]))
                           (MidiEvents->NoteList midiEvents))])
              (NormalizeNoteListTimes! noteList ticksPerQuarter tempoMap)
              (let ([song-length
                     (fold (lambda (n acc)
                             (max acc (+ (note-time n) (note-duration n))))
                           0 noteList)])
                (detail "  ~a note~:p, ~,2f seconds~%"
                        (length noteList) song-length))
              (when (option-ref options 'dump-notes #f)
                (pretty-print noteList))

              (cond
               [dry-run
                (say "Dry run; not rendering~%")]
               [(null? noteList)
                (die "no notes to render")]
               [else
                (say "Rendering to ~a at ~a Hz...~%" output rate)
                (let ([pcm (NoteList->PCM noteList)])
                  (write-wav output pcm rate)
                  (detail "  wrote ~a bytes~%"
                          (+ 44 (bytevector-length pcm))))
                (say "Done~%")]))))))))
