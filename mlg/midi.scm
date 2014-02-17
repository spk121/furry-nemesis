(define-module (mlg midi)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-11)
  :use-module (rnrs io ports)
  :use-module (mlg binports)
  :use-module (ice-9 pretty-print)
  :use-module (ice-9 optargs)
  :export (ParseMidi
	   ParseMidiFile
           MidiEvents->TempoMap))

(define V_NOTE_OFF #x80)
(define V_NOTE_ON #x90)
(define V_POLYPHONIC_AFTERTOUCH #xA0)
(define V_CONTROL_CHANGE #xB0)
(define V_PROGRAM_CHANGE #xC0)
(define V_CHANNEL_AFTERTOUCH #xD0)
(define V_PITCH_WHEEL_CONTROL #xE0)

(define S_SOX #xF0)
(define S_MIDI_TIME_CODE #xF1)
(define S_SONG_POSITION_POINTER #xF2)
(define S_SONG_SELECT #xF3)
(define S_TUNE_REQUEST #xF6)
(define S_EOX #xF7)
(define S_SYSTEM_RESET #xFF)

(define (get-midi-var-int port)
  "Unpacks a variable-length integer from the data store.  This
integer is encoded such that the low 7-bits of each input byte are
numerical data and the high bit indicates if another byte of data is
expected.  Variable length integers never have more than 4 bytes."
  (define (_Shift cur prev)
    (logior (ash prev 7) (logand cur #x7F)))

  (let ([bytes '()])

    (define (_AppendToBytesAndTestHigh)
      (let ([cur (get-u8 port)])
	(set! bytes (append bytes (list cur)))
	(logtest cur #x80)))

    (while (_AppendToBytesAndTestHigh))

    (when (> (length bytes) 4)
	  (error "Invalid MIDI variable-length number" bytes))
    (fold _Shift 0 bytes)))

(define (ParseHeader port)
  "Read a binary port and parsing the MIDI header contained therein."
  (let* ([type (get-latin1-string port 4)]
	 [length (get-u32-be port)]
	 [format (get-u16-be port)]
	 [tracks (get-u16-be port)]
	 [division (get-u16-be port)])
    (unless (string=? type "MThd")
	    (error "Bad MIDI header string" type))
    (unless (and (>= format 0) (<= format 2))
	    (error "Bad MIDI format type" format))

    ;; MIDI time divisions are in one of two formats, depending on the
    ;; MSB.  If the high bit is zero, it is ticks per quarter note.
    ;; If the high bit is one, it a SMPTE frame time.
    (when (logtest division #x8000)
	  ;; FIXME: the following isn't right because frames-per-second
	  ;; has negative numbers and drop frames.
	  (let ([ticksPerFrame (logand division #xFF)]
		[framesPerSecond (ash division -8)])
	    (set! division (* ticksPerFrame framesPerSecond))))
    
    ;; Return
    (values (list-ref '(MIDI_FORMAT_0 MIDI_FORMAT_1 MIDI_FORMAT_2) format)
	    tracks
	    division)))

(define (ParseMetaEvent port channel)
  (let ([byte1 (get-u8 port)]
	[byte2 (get-u8 port)])
    (cond 
     [(and (eqv? byte1 #x01))
      (list 'TEXT_EVENT
	    #:channel channel
	    #:text (get-latin1-string port byte2))]
     [(and (eqv? byte1 #x02))
      (list 'COPYRIGHT_NOTICE 
	    #:channel channel
	    #:text (get-latin1-string port byte2))]
     [(and (eqv? byte1 #x03))
      (list 'TRACK_NAME 
	    #:channel channel
	    #:text (get-latin1-string port byte2))]
     [(and (eqv? byte1 #x04))
      (list 'INSTRUMENT_NAME
	    #:channel channel
	    #:text (get-latin1-string port byte2))]
     [(and (eqv? byte1 #x05))
      (list 'LYRIC
	    #:channel channel
	    #:text (get-latin1-string port byte2))]
     [(and (eqv? byte1 #x06))
      (list 'MARKER
	    #:channel channel
	    #:text (get-latin1-string port byte2))]
     [(and (eqv? byte1 #x07))
      (list 'CUE_POINT
	    #:channel channel
	    #:text (get-latin1-string port byte2))]
     [(and (eqv? byte1 #x21) (eqv? byte2 #x01))
      (list 'MIDI_PORT
            #:channel channel
            #:port (get-u8 port))]
     [(and (eqv? byte1 #x2F) (eqv? byte2 #x00))
      (list 'END_OF_TRACK
	    #:channel channel)]
     [(and (eqv? byte1 #x00) (eqv? byte2 #x02))
      (list 'SEQUENCE_NUMBER
	    #:channel channel
	    #:value (get-u16-be port))]
     [(and (eqv? byte1 #x51) (eqv? byte2 #x03))
      (list 'SET_TEMPO
	    #:channel channel
	    #:microsecondsPerQuarter (logior (ash (get-u8 port) 16)
					     (ash (get-u8 port) 8)
					     (get-u8 port)))]
     [(and (eqv? byte1 #x54) (eqv? byte2 #x05))
      (list 'SMPTE_OFFSET
	    #:channel channel
	    #:hours (get-u8 port)
	    #:minutes (get-u8 port)
	    #:seconds (get-u8 port)
	    #:frames (+ (get-u8 port) (* 0.01 (get-u8 port))))]

     [(and (eqv? byte1 #x58) (eqv? byte2 #x04))
      (list 'TIME_SIGNATURE
	    #:channel channel 
	    #:numerator (get-u8 port)
	    #:denominator (list-ref '( 2 4 8 16 32 64) (get-u8 port))
	    #:clocksPerClick (get-u8 port)
	    #:thirtySecondNotesPerQuarter (get-u8 port))]

     [(and (eqv? byte1 #x59) (eqv? byte2 #x02))
      (let ((byte1 (get-s8 port))
	    (byte2 (get-u8 port)))
	(list 'KEY_SIGNATURE
	      #:channel channel
	      (if (< byte1 0)
		  #:flats
		  #:sharps)
	      byte1
	      #:tonality
	      (if (eqv? 0 byte2)
		  'MAJOR
		  'MINOR)))]
     [else
      (do ([i 0 (1+ i)]) ([= i byte2])
          (get-u8 port))
      (format #t "Unhandled meta event ~a len ~a~%" byte1 byte2)
      (list 'META #:channel channel #:byte1 byte1)])))


(define (ParseEvent port track lastStatus)
  "Extract a single MIDI event."
  
  (define (_HiTest x y)
    (eqv? (logand x #xF0) (logand y #xF0)))
  
  (define (_LoVal x)
    (logand x #x0F))

  (define (_ListToString lst)
    (apply string (map integer->char lst)))
  
  (let* ([deltaTime (get-midi-var-int port)]
	 [status (if (< (lookahead-u8 port) #x80)
                     lastStatus
                     (get-u8 port))])
    (values
     deltaTime
     status

     (cond
      [(eqv? status S_SOX)
       (let loop ([byte (get-u8 port)]
		  [lst '()])
	 (if (not (eqv? byte S_EOX))
	     (loop (get-u8 port)
		   (append lst (list byte)))
	     (list 'SOX
		   #:channel (_LoVal status)
		   #:message lst)))]
      
      [(eqv? status S_MIDI_TIME_CODE)
       (let* ([value (get-u8 port)])
	 (list 'TIME_CODE_QUARTER_FRAME
	       #:channel (_LoVal status)
	       #:scale
	       (cond 
		((_HiTest value #x00)
		 'FRAME_BITS_ZERO_TO_THREE)
		((_HiTest value #x10)
		 'FRAME_BITS_FOUR_TO_SEVEN)
		((_HiTest value #x20)
		 'SECONDS_BITS_ZERO_TO_THREE)
		((_HiTest value #x30)
		 'SECONDS_BITS_FOUR_TO_SEVEN)
		((_HiTest value #x40)
		 'MINUTES_BITS_ZERO_TO_THREE)
		((_HiTest value #x50)
		 'MINUTES_BITS_FOUR_TO_SEVEN)
		((_HiTest value #x60)
		 'HOURS_BITS_ONE_TO_THREE)
		((_HiTest value #x70)
		 'HOURS_BITS_FOUR_TO_SEVEN))
	       #:value value))]

      [(eqv? status S_SONG_POSITION_POINTER)
       (let ([lo (get-u8 port)]
	     [hi (get-u8 port)])
	 (list 'SONG_POSITION_POINTER
	       #:channel (_LoVal status)
	       #:beat (logior (ash hi 7) lo)))]
      
      [(eqv? status S_SONG_SELECT)
       (list 'SONG_SELECT
	     #:channel (_LoVal status)
	     #:song (get-u8 port))]
      
      ((eqv? status S_SYSTEM_RESET)
       (if (not (logtest (lookahead-u8 port) #x80))
	   (ParseMetaEvent port (_LoVal status))
	   ;; else
	   (list 'SYSTEM_RESET)))
      
      [(_HiTest status V_NOTE_OFF)
       (list 'NOTE_OFF
             #:track track
	     #:channel (_LoVal status)
	     #:key (get-u8 port)
	     #:velocity (get-u8 port))]

      [(_HiTest status V_NOTE_ON)
       (let ([key (get-u8 port)]
             [velocity (get-u8 port)])
         (if (eqv? velocity 0)
             (list 'NOTE_OFF
                   #:track track
                   #:channel (_LoVal status)
                   #:key key
                   #:velocity velocity)
             ;; else
             (list 'NOTE_ON
                   #:track track
                   #:channel (_LoVal status)
                   #:key key
                   #:velocity velocity)))]

      [(_HiTest status V_POLYPHONIC_AFTERTOUCH)
       (list 'KEY_PRESSURE
	     #:channel (_LoVal status)
	     #:key (get-u8 port)
	     #:pressure (get-u8 port))]

      [(_HiTest status V_CONTROL_CHANGE)
       (let* ([controller (get-u8 port)]
	      [value (get-u8 port)])
	 (cond
	  [(and (eqv? controller 120) (eqv? value 0))
	   (list 'ALL_SOUND_OFF
		 #:channel (_LoVal status))]
	  [(eqv? controller 121)
	   (list 'RESET_ALL_CONTROLLERS
		 #:channel (_LoVal status)
		 #:value value)]
	  [(and (eqv? controller 122) (eqv? value 0))
	   (list 'LOCAL_CONTROL_OFF
		 #:channel (_LoVal status))]
	  [(and (eqv? controller 122) (eqv? value 127))
	   (list 'LOCAL_CONTROL_ON
		 #:channel (_LoVal status))]
	  [(and (eqv? controller 123) (eqv? value 0))
	   (list 'ALL_NOTES_OFF
		 #:channel (_LoVal status))]
	  [(and (eqv? controller 124) (eqv? value 0))
	   (list 'OMNI_MODE_OFF
		 #:channel (_LoVal status))]
	  [(and (eqv? controller 125) (eqv? value 0))
	   (list 'OMNI_MODE_ON
		 #:channel (_LoVal status))]
	  [(eqv? controller 126)
	   (list 'MONO_MODE_ON
		 #:channel (_LoVal status)
		 #:value value)]
	  [(and (eqv? controller 127) (eqv? value 0))
	   (list 'POLY_MODE_ON
		 #:channel (_LoVal status))]
	  [(< controller 120)
	   (list 'CONTROL_CHANGE
		 #:channel (_LoVal status)
		 #:controller controller
		 #:value value)]
	  [else
	   (error "Bad channel mode message" controller value)]))]

      [(_HiTest status V_PROGRAM_CHANGE)
       (list 'PROGRAM_CHANGE
	     #:channel (_LoVal status)
	     #:patch (get-u8 port))]
      [(_HiTest status V_CHANNEL_AFTERTOUCH)
       (list 'CHANNEL_PRESSURE 
	     #:channel (_LoVal status)
	     #:pressure (get-u8 port))]
      [(_HiTest status V_PITCH_WHEEL_CONTROL)
       (let ([lo (get-u8 port)]
	     [hi (get-u8 port)])
	 (list 'PITCH_WHEEL 
	       #:channel (_LoVal status)
	       #:value (logior (ash hi 7) lo)))]
      [else
       (format #t "Unhandled event~a~%" status)
       (list 'JUNK)]))))

(define (ParseTrack port trackID ticksPerQuarter)
  "Extract one track's set of MIDI events."
  (let* ([type (get-latin1-string port 4)]
	 [length (get-u32-be port)]
	 [trackEnd (+ length (port-position port))]
	 [eventList '()]
         [lastStatus #f]
	 [now 0])
    (unless (string=? type "MTrk")
	    (error "Bad MIDI track header string" type))
    (while #t ; (< (port-position port) trackEnd)
	   ;; Get the next MIDI event and the time difference
	   ;; (in MIDI ticks) since the last MIDI event.
	   (let-values ([(deltaTime curStatus event) (ParseEvent port trackID lastStatus)])
             (set! lastStatus curStatus)
	     ;; Splice extra information about the current time and
	     ;; track into the MIDI event.
	     (set! now (+ now deltaTime))
	     (set! event
		   (append
		    (list (car event))	; The type of the event
		    (list #:time now)	; Time since beginning of song
		    (list #:track trackID)
		    (cdr event)))	; The details of the event
	     (display "E: ") (write event) (newline)
	     (set! eventList
		   (append! eventList (list event)))
             (when (eqv? 'END_OF_TRACK (car event))
                   (break))))
    ;; (pretty-print eventList)
    eventList))

(define (ParseMidi port)
  "Extract MIDI events for all tracks from a port."

  (define (_ExtractTime x)
    "Get the time (in MIDI clicks) from a MIDI event."
    (let-keywords (cdr x) #t
                  ([time -1])
                  time))

  (define (_TimeCompare a b)
    "Compare two MIDI events by their times (in MIDI clicks)."
    (< (_ExtractTime a) (_ExtractTime b)))
  
  (let-values ([(midiFormat trackCount ticksPerQuarter) (ParseHeader port)])
    (format #t "Track Count: ~a~%" trackCount)
    (let ([midiEvents '()])
      ;; Add a fake MIDI event with the header data, mostly because we
      ;; need ticksPerQuarter
      (set! midiEvents
            (list
             (list 'MTHD #:ticksPerQuarter ticksPerQuarter)))
      
      ;; Now read each track's MIDI events.
      (do ([i 0 (1+ i)]) ([= i trackCount])
        (when (not (eof-object? (lookahead-u8 port)))
              (set! midiEvents
                    (append midiEvents
                            (ParseTrack port i ticksPerQuarter)))))

      ;; Merge the tracks into a single time-sorted list.
      (stable-sort midiEvents _TimeCompare))))

(define (ParseMidiFile filename)
  (ParseMidi (open-file-input-port filename)))

(define (MidiEvents->TempoMap midiEvents)
  "Given a list of midi events as generated by ParseMidi,
it looks for SET_TEMPO events and generates a mapping from
MIDI clicks to seconds."
  (define (_IsSetTempo x)
    (eqv? 'SET_TEMPO (car x)))

  (define (_ExtractTempoEntry x)
    (let-keywords (cdr x) #t
                  ([time -1]
                   [microsecondsPerQuarter -1])
                  (cons time microsecondsPerQuarter)))

  (let ([tempoEvents (filter _IsSetTempo midiEvents)])
    (map-in-order _ExtractTempoEntry tempoEvents)))

