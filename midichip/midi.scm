;;; (midichip midi) -- Standard MIDI File parser (MIDI 1.0 / SMF 1.1)
;;;
;;; ParseMidi returns a time-sorted list of events.  Every event is a
;;; list whose head is a symbol naming the event, followed by keyword
;;; arguments.  All events read from a track carry #:time (in ticks
;;; since the start of the song) and #:track.  The first element of
;;; the list is always a synthetic MTHD event describing the header:
;;;
;;;   (MTHD #:ticksPerQuarter N #:format F #:tracks T
;;;         #:division METRICAL|SMPTE [#:framesPerSecond FPS
;;;         #:ticksPerFrame TPF])
;;;
;;; For SMPTE files #:ticksPerQuarter holds ticks per *second* and
;;; MidiEvents->TempoMap returns a single 1-second-per-quarter entry,
;;; so the usual tick-to-seconds conversion yields absolute time.

(define-module (midichip midi)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (rnrs io ports)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 optargs)
  #:export (ParseMidi
            ParseMidiFile
            MidiEvents->TempoMap
            MidiHeaderRef
            midi-trace))

;; When true, the parser prints the track count and every event as it
;; is read.  Useful for debugging; off by default.
(define midi-trace (make-parameter #f))

(define (warn fmt . args)
  (apply format (current-error-port) (string-append "midi: " fmt "~%") args))

;;; ------------------------------------------------------------------
;;; Cursor over a bytevector

(define (make-cursor bv)
  "Return a procedure that dispatches on a message symbol to read
from BV.  Reading past the end throws 'midi-truncated."
  (let ([pos 0]
        [len (bytevector-length bv)])
    (define (need n)
      (when (> (+ pos n) len)
        (throw 'midi-truncated pos)))
    (lambda (msg . args)
      (case msg
        [(u8)
         (need 1)
         (let ([b (bytevector-u8-ref bv pos)])
           (set! pos (1+ pos))
           b)]
        [(peek)
         (need 1)
         (bytevector-u8-ref bv pos)]
        [(bytes)
         (let* ([n (car args)]
                [out (make-bytevector n)])
           (need n)
           (bytevector-copy! bv pos out 0 n)
           (set! pos (+ pos n))
           out)]
        [(varint)
         ;; Variable-length quantity: 7 bits per byte, high bit set
         ;; on all but the last byte, at most 4 bytes.
         (let loop ([val 0] [count 1])
           (need 1)
           (let ([b (bytevector-u8-ref bv pos)])
             (set! pos (1+ pos))
             (let ([val (logior (ash val 7) (logand b #x7F))])
               (cond
                [(not (logtest b #x80)) val]
                [(>= count 4)
                 (warn "variable-length quantity longer than 4 bytes")
                 val]
                [else (loop val (1+ count))]))))]
        [(eof?) (>= pos len)]))))

(define (bytes->latin1 bv)
  (list->string (map integer->char (bytevector->u8-list bv))))

(define (subbytes bv start n)
  (let ([out (make-bytevector n)])
    (bytevector-copy! bv start out 0 n)
    out))

;;; ------------------------------------------------------------------
;;; Header

(define (decode-division division)
  "Return a property list describing the MThd division word."
  (if (logtest division #x8000)
      ;; SMPTE: the high byte is the negated frame rate (-24, -25,
      ;; -29, -30) in two's complement; the low byte is ticks per
      ;; frame.  -29 means 30 drop-frame, i.e. 29.97 frames/second.
      (let* ([fps-code (- 256 (ash division -8))]
             [tpf (logand division #xFF)]
             [fps (if (= fps-code 29) 30000/1001 fps-code)])
        (list #:ticksPerQuarter (* fps tpf)
              #:division 'SMPTE
              #:framesPerSecond fps
              #:ticksPerFrame tpf))
      (list #:ticksPerQuarter (if (zero? division) 96 division)
            #:division 'METRICAL)))

;;; ------------------------------------------------------------------
;;; Meta events

(define text-meta-names
  #(#f TEXT_EVENT COPYRIGHT_NOTICE TRACK_NAME INSTRUMENT_NAME LYRIC
       MARKER CUE_POINT PROGRAM_NAME DEVICE_NAME))

(define (decode-meta type data channel trackID)
  (let ([n (bytevector-length data)])
    (define (u8 i) (bytevector-u8-ref data i))
    (cond
     [(= type #x00)
      ;; If the number is omitted it defaults to the track's position.
      (list 'SEQUENCE_NUMBER #:channel channel
            #:value (if (>= n 2) (logior (ash (u8 0) 8) (u8 1)) trackID))]
     [(<= #x01 type #x09)
      (list (vector-ref text-meta-names type) #:channel channel
            #:text (bytes->latin1 data))]
     [(<= #x0A type #x0F)
      ;; Reserved text event types.
      (list 'TEXT_EVENT #:channel channel #:metaType type
            #:text (bytes->latin1 data))]
     [(and (= type #x20) (>= n 1))
      (list 'MIDI_CHANNEL_PREFIX #:channel channel #:prefix (u8 0))]
     [(and (= type #x21) (>= n 1))
      (list 'MIDI_PORT #:channel channel #:port (u8 0))]
     [(= type #x2F)
      (list 'END_OF_TRACK #:channel channel)]
     [(and (= type #x51) (>= n 3))
      (list 'SET_TEMPO #:channel channel
            #:microsecondsPerQuarter
            (logior (ash (u8 0) 16) (ash (u8 1) 8) (u8 2)))]
     [(and (= type #x54) (>= n 5))
      ;; The hours byte is 0rrhhhhh, where rr is a frame rate code.
      (list 'SMPTE_OFFSET #:channel channel
            #:framesPerSecond (vector-ref #(24 25 29.97 30)
                                          (logand (ash (u8 0) -5) 3))
            #:hours (logand (u8 0) #x1F)
            #:minutes (u8 1)
            #:seconds (u8 2)
            #:frames (+ (u8 3) (* 0.01 (u8 4))))]
     [(and (= type #x58) (>= n 4))
      (list 'TIME_SIGNATURE #:channel channel
            #:numerator (u8 0)
            #:denominator (expt 2 (u8 1))
            #:clocksPerClick (u8 2)
            #:thirtySecondNotesPerQuarter (u8 3))]
     [(and (= type #x59) (>= n 2))
      (let ([sf (bytevector-s8-ref data 0)])
        (list 'KEY_SIGNATURE #:channel channel
              (if (< sf 0) #:flats #:sharps) sf
              #:tonality (if (zero? (u8 1)) 'MAJOR 'MINOR)))]
     [(= type #x7F)
      (list 'SEQUENCER_SPECIFIC #:channel channel
            #:data (bytevector->u8-list data))]
     [else
      ;; Unknown meta events must be ignored by players, but keep
      ;; them in the event list for inspection.
      (list 'META #:channel channel #:metaType type
            #:data (bytevector->u8-list data))])))

;;; ------------------------------------------------------------------
;;; System common / real-time messages

(define (system-message-length status)
  (case status
    [(#xF1 #xF3) 1]
    [(#xF2) 2]
    [else 0]))

(define (decode-system status data)
  "Decode a system common or system real-time message.  DATA is a
list of the data bytes."
  (case status
    [(#xF1)
     (let ([v (if (pair? data) (car data) 0)])
       (list 'TIME_CODE_QUARTER_FRAME
             #:scale (vector-ref #(FRAME_BITS_ZERO_TO_THREE
                                   FRAME_BITS_FOUR_TO_SEVEN
                                   SECONDS_BITS_ZERO_TO_THREE
                                   SECONDS_BITS_FOUR_TO_SEVEN
                                   MINUTES_BITS_ZERO_TO_THREE
                                   MINUTES_BITS_FOUR_TO_SEVEN
                                   HOURS_BITS_ZERO_TO_THREE
                                   HOURS_BITS_FOUR_TO_SEVEN)
                                 (logand (ash v -4) 7))
             #:value (logand v #x0F)))]
    [(#xF2)
     (list 'SONG_POSITION_POINTER
           #:beat (if (>= (length data) 2)
                      (logior (ash (cadr data) 7) (car data))
                      0))]
    [(#xF3) (list 'SONG_SELECT #:song (if (pair? data) (car data) 0))]
    [(#xF6) (list 'TUNE_REQUEST)]
    [(#xF8) (list 'TIMING_CLOCK)]
    [(#xFA) (list 'START)]
    [(#xFB) (list 'CONTINUE)]
    [(#xFC) (list 'STOP)]
    [(#xFE) (list 'ACTIVE_SENSING)]
    [(#xFF) (list 'SYSTEM_RESET)]
    [else (list 'UNDEFINED_SYSTEM #:status status #:data data)]))

;;; ------------------------------------------------------------------
;;; Channel messages

(define (channel-message-length status)
  (case (logand status #xF0)
    [(#xC0 #xD0) 1]
    [else 2]))

(define (decode-channel status d1 d2)
  (let ([ch (logand status #x0F)])
    (case (logand status #xF0)
      [(#x80)
       (list 'NOTE_OFF #:channel ch #:key d1 #:velocity d2)]
      [(#x90)
       ;; A NOTE_ON with velocity zero is a NOTE_OFF.
       (list (if (zero? d2) 'NOTE_OFF 'NOTE_ON)
             #:channel ch #:key d1 #:velocity d2)]
      [(#xA0)
       (list 'KEY_PRESSURE #:channel ch #:key d1 #:pressure d2)]
      [(#xB0)
       (case d1
         ;; Channel mode messages.  Receivers should act on them
         ;; whatever the second byte says, so decode leniently.
         [(120) (list 'ALL_SOUND_OFF #:channel ch #:value d2)]
         [(121) (list 'RESET_ALL_CONTROLLERS #:channel ch #:value d2)]
         [(122) (list (if (< d2 64) 'LOCAL_CONTROL_OFF 'LOCAL_CONTROL_ON)
                      #:channel ch #:value d2)]
         [(123) (list 'ALL_NOTES_OFF #:channel ch #:value d2)]
         [(124) (list 'OMNI_MODE_OFF #:channel ch #:value d2)]
         [(125) (list 'OMNI_MODE_ON #:channel ch #:value d2)]
         [(126) (list 'MONO_MODE_ON #:channel ch #:value d2)]
         [(127) (list 'POLY_MODE_ON #:channel ch #:value d2)]
         [else (list 'CONTROL_CHANGE #:channel ch #:controller d1 #:value d2)])]
      [(#xC0)
       (list 'PROGRAM_CHANGE #:channel ch #:patch d1)]
      [(#xD0)
       (list 'CHANNEL_PRESSURE #:channel ch #:pressure d1)]
      [(#xE0)
       (list 'PITCH_WHEEL #:channel ch #:value (logior (ash d2 7) d1))])))

;;; ------------------------------------------------------------------
;;; Tracks

(define (ParseTrack bv trackID)
  "Parse the body of one MTrk chunk.  Returns two values: the list
of events in file order and the track's end time in ticks."
  (let ([rd (make-cursor bv)]
        [running #f]            ; running status (channel messages only)
        [prefix #f]             ; MIDI channel prefix for meta/sysex
        [sysex-open #f]         ; inside a multi-packet SysEx?
        [now 0]
        [events '()]
        [warned-orphan #f])

    (define (emit! ev)
      (let ([ev (cons* (car ev) #:time now #:track trackID (cdr ev))])
        (when (midi-trace)
          (display "E: ") (write ev) (newline))
        (set! events (cons ev events))))

    (define (read-data-list n)
      (let loop ([i 0] [acc '()])
        (if (= i n) (reverse acc) (loop (1+ i) (cons (rd 'u8) acc)))))

    (define (sysex-packet data)
      ;; Returns (message . complete?) for a SysEx packet body.
      (let ([complete (and (pair? data) (= (last data) #xF7))])
        (cons (if complete (drop-right data 1) data) complete)))

    (catch 'midi-truncated
      (lambda ()
        (let loop ()
          (unless (rd 'eof?)
            (set! now (+ now (rd 'varint)))
            (let ([b (rd 'peek)])
              (cond
               ;; Running status: a data byte where a status byte was
               ;; expected repeats the previous channel status.
               [(< b #x80)
                (if running
                    (let* ([d1 (rd 'u8)]
                           [d2 (if (= 2 (channel-message-length running))
                                   (rd 'u8) 0)])
                      (emit! (decode-channel running d1 d2)))
                    (begin
                      (rd 'u8)
                      (unless warned-orphan
                        (warn "track ~a: data byte without running status" trackID)
                        (set! warned-orphan #t))))
                (loop)]

               ;; Channel voice or channel mode message.
               [(< b #xF0)
                (rd 'u8)
                (set! running b)
                (let* ([d1 (rd 'u8)]
                       [d2 (if (= 2 (channel-message-length b)) (rd 'u8) 0)])
                  (emit! (decode-channel b d1 d2)))
                (loop)]

               ;; Meta event: FF type length data.  Meta and SysEx
               ;; events never become the running status.
               [(= b #xFF)
                (rd 'u8)
                (let* ([type (rd 'u8)]
                       [len (rd 'varint)]
                       [data (rd 'bytes len)]
                       [ev (decode-meta type data prefix trackID)])
                  (emit! ev)
                  (when (eq? (car ev) 'MIDI_CHANNEL_PREFIX)
                    (set! prefix (bytevector-u8-ref data 0)))
                  ;; Nothing after End of Track is meaningful.
                  (unless (eq? (car ev) 'END_OF_TRACK)
                    (loop)))]

               ;; SysEx: F0 length data.  If the data does not end
               ;; with F7, the message continues in later F7 packets.
               [(= b #xF0)
                (rd 'u8)
                (let ([pkt (sysex-packet (read-data-list (rd 'varint)))])
                  (emit! (list 'SOX #:channel prefix
                               #:message (car pkt) #:complete (cdr pkt)))
                  (set! sysex-open (not (cdr pkt))))
                (loop)]

               ;; F7 length data: a SysEx continuation packet, or an
               ;; escape carrying arbitrary bytes such as real-time or
               ;; system common messages.
               [(= b #xF7)
                (rd 'u8)
                (let ([data (read-data-list (rd 'varint))])
                  (cond
                   [sysex-open
                    (let ([pkt (sysex-packet data)])
                      (emit! (list 'SOX_CONTINUATION #:channel prefix
                                   #:message (car pkt) #:complete (cdr pkt)))
                      (set! sysex-open (not (cdr pkt))))]
                   [(and (pair? data) (>= (car data) #xF1) (<= (car data) #xFF))
                    (emit! (decode-system (car data) (cdr data)))]
                   [else
                    (emit! (list 'SYSEX_ESCAPE #:channel prefix #:data data))]))
                (loop)]

               ;; Bare system common / real-time bytes are not legal
               ;; in an SMF, but decode them if they turn up.  They do
               ;; not affect running status.
               [else
                (rd 'u8)
                (emit! (decode-system b (read-data-list (system-message-length b))))
                (loop)])))))
      (lambda (key . args)
        (warn "track ~a is truncated; using the events read so far" trackID)))

    (values (reverse events) now)))

;;; ------------------------------------------------------------------
;;; Files

(define (read-chunk port)
  "Read one chunk header and body.  Returns (type . body), or #f at
end of file.  A short body is returned as far as it goes."
  (let ([hdr (get-bytevector-n port 8)])
    (if (or (eof-object? hdr) (< (bytevector-length hdr) 8))
        #f
        (let* ([type (bytes->latin1 (subbytes hdr 0 4))]
               [len (bytevector-u32-ref hdr 4 (endianness big))]
               [body (get-bytevector-n port len)])
          (cons type (if (eof-object? body) (make-bytevector 0) body))))))

(define (skip-riff-wrapper! port)
  "Handle RMID files (a Standard MIDI File inside a RIFF 'data'
chunk).  Leaves PORT positioned at the MThd chunk."
  (when (port-has-port-position? port)
    (let* ([pos (port-position port)]
           [peek (get-bytevector-n port 4)])
      (set-port-position! port pos)
      (when (and (bytevector? peek) (= 4 (bytevector-length peek))
                 (string=? (bytes->latin1 peek) "RIFF"))
        (get-bytevector-n port 12)      ; "RIFF" size "RMID"
        (let loop ()
          (let ([hdr (get-bytevector-n port 8)])
            (when (and (bytevector? hdr) (= 8 (bytevector-length hdr)))
              (let ([id (bytes->latin1 (subbytes hdr 0 4))]
                    [len (bytevector-u32-ref hdr 4 (endianness little))])
                (unless (string=? id "data")
                  (get-bytevector-n port (+ len (logand len 1)))
                  (loop))))))))))

(define (event-time ev)
  ;; Every track event is (TYPE #:time T ...).
  (caddr ev))

(define (ParseMidi port)
  "Extract MIDI events for all tracks from a binary input port."
  (skip-riff-wrapper! port)
  (let ([header (read-chunk port)])
    (unless (and header (string=? (car header) "MThd")
                 (>= (bytevector-length (cdr header)) 6))
      (error "Not a Standard MIDI File (missing MThd header)"))
    ;; The header may be longer than 6 bytes in future versions; the
    ;; extra bytes are ignored.
    (let* ([hb (cdr header)]
           [format (bytevector-u16-ref hb 0 (endianness big))]
           [trackCount (bytevector-u16-ref hb 2 (endianness big))]
           [division (decode-division (bytevector-u16-ref hb 4 (endianness big)))])
      (unless (<= format 2)
        (warn "unknown SMF format ~a; treating it as format 1" format))
      (when (midi-trace)
        (format #t "Track Count: ~a~%" trackCount))

      ;; Read every MTrk chunk.  Chunks of other types are skipped.
      (let loop ([trackID 0] [offset 0] [tracks '()])
        (let ([chunk (read-chunk port)])
          (cond
           [(not chunk)
            (unless (= trackID trackCount)
              (warn "header promises ~a tracks but the file has ~a"
                    trackCount trackID))
            (let ([hdr (cons* 'MTHD
                              (append division
                                      (list #:format
                                            (list-ref '(MIDI_FORMAT_0
                                                        MIDI_FORMAT_1
                                                        MIDI_FORMAT_2)
                                                      (if (<= format 2) format 1))
                                            #:tracks trackID)))])
              ;; Merge into one time-ordered list.  The sort is
              ;; stable, so simultaneous events keep track order.
              (cons hdr (stable-sort (concatenate (reverse tracks))
                                     (lambda (a b)
                                       (< (event-time a) (event-time b))))))]
           [(string=? (car chunk) "MTrk")
            (let-values ([(events end) (ParseTrack (cdr chunk) trackID)])
              ;; Format 2 tracks are independent sequences played one
              ;; after another, so shift each by the preceding ones.
              (let ([events (if (and (= format 2) (positive? offset))
                                (map (lambda (ev)
                                       (cons* (car ev) #:time
                                              (+ offset (event-time ev))
                                              (cdddr ev)))
                                     events)
                                events)])
                (loop (1+ trackID)
                      (if (= format 2) (+ offset end) offset)
                      (cons events tracks))))]
           [else
            (loop trackID offset tracks)]))))))

(define (ParseMidiFile filename)
  (call-with-port (open-file-input-port filename) ParseMidi))

(define* (MidiHeaderRef midiEvents key #:optional default)
  "Look up KEY (a keyword such as #:format) in the MTHD header event
at the head of MIDIEVENTS."
  (let ([m (memq key (cdar midiEvents))])
    (if m (cadr m) default)))

(define (MidiEvents->TempoMap midiEvents)
  "Given a list of MIDI events as generated by ParseMidi, return an
alist mapping tick -> microseconds per quarter note, one entry per
SET_TEMPO event.  For SMPTE-timed files tempo is irrelevant, so a
single (0 . 1000000) entry is returned; ticksPerQuarter then holds
ticks per second."
  (if (eq? (MidiHeaderRef midiEvents #:division) 'SMPTE)
      (list (cons 0 1000000))
      (filter-map (lambda (ev)
                    (and (eq? (car ev) 'SET_TEMPO)
                         (let-keywords (cdr ev) #t
                             ([time 0] [microsecondsPerQuarter 500000])
                           (cons time microsecondsPerQuarter))))
                  midiEvents)))
