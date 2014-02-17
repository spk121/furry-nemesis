;; Helper functions for binary ports

(define-module (mlg binports)
  :use-module (rnrs io ports)
  :use-module (rnrs bytevectors)
  :export (get-s8
	   _get-u8
	   get-s16-le
	   get-s16-be
	   get-u16-le
	   get-u16-be
	   get-s32-le
	   get-s32-be
	   get-u32-le
	   get-u32-be
	   get-latin1-string
	   skip))

;; get-u8
(define (get-s8 port)
  (bytevector-s8-ref (get-bytevector-n port 1) 0))

(define (_get-u8 port)
  (bytevector-u8-ref (get-bytevector-n port 1) 0))

(define (get-s16-le port)
  (bytevector-s16-ref (get-bytevector-n port 2) 0 (endianness little)))
(define (get-s16-be port)
  (bytevector-s16-ref (get-bytevector-n port 2) 0 (endianness big)))

(define (get-u16-le port)
  (bytevector-u16-ref (get-bytevector-n port 2) 0 (endianness little)))
(define (get-u16-be port)
  (bytevector-u16-ref (get-bytevector-n port 2) 0 (endianness big)))

(define (get-s32-le port)
  (bytevector-s32-ref (get-bytevector-n port 4) 0 (endianness little)))
(define (get-s32-be port)
  (bytevector-s32-ref (get-bytevector-n port 4) 0 (endianness big)))

(define (get-u32-le port)
  (bytevector-s32-ref (get-bytevector-n port 4) 0 (endianness little)))
(define (get-u32-be port)
  (bytevector-s32-ref (get-bytevector-n port 4) 0 (endianness big)))

(define (get-latin1-string port n)
  (let ((u8 (bytevector->u8-list (get-bytevector-n port n))))
    (apply string (map integer->char u8))))

(define (get-midi-var-int port)
  "Unpacks a variable-length integer from the port.  This
integer is encoded such that the low 7-bits of each input byte are
numerical data and the high bit indicates if another byte of data is
expected.  Variable length integers never have more than 4 bytes."
  (define (_Shift lo hi)
    (logior (logand lo #x7F) (ash hi 7)))

  (let loop ([i 1]
	     [val 0]
	     [cur (get-u8 port)])
    (when (and (< i 4) (logtest cur #x80))
	  (loop
	   [1+ i]
	   [_Shift cur val]
	   [get-u8 port]))
    (_Shift cur val)))

(define (skip port n)
  (do ((i 0 (+ i 1)))
      ((= i n))
    (get-u8 port)))
