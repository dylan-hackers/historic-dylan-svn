;;;;;; Words implementation based on pairs of 16-bit fixnums

;;; Copyright (C) 2004 Taylor Campbell.
;;; All rights reserved.
;;;
;;; You may do as you like with this code: quote, copy, or distribute
;;; it -- in source, binary, or ternary forms --, statically link it to
;;; GPL'd _and_ Java programs, et cetera; with the only provision that
;;; you always give credit to its original author, Taylor Campbell, you
;;; do not remove this copyright notice and you do not hold the author,
;;; Taylor Campbell, liable for _any_ damages that may be caused as a
;;; result, direct or indirect, of using this software, not even if it
;;; should cause outbreaks of nasal demons.

;;; --------------------
;;; Terminology

;;; Words represent 32 bit unsigned integers; bytes are 8 bit unsigned
;;; integers.  Words are not actually numbers; they are implemented as
;;; pairs of 16 bit fixnums, though this is an abstraction.  Bytes are
;;; or happen to be numbers.

;;; (MAKE-WORD <high-bits> <low-bits>) -> word
;;;   Low-level word constructor.
(define (make-word high low) (cons high low))

(define (word? x)
  (and (pair? x)
       (integer? (car x))
       (integer? (cdr x))
       (<= 0 (car x) 65536)
       (<= 0 (cdr x) 65536)))

;;; (WORD-HIGH-BITS <word>) -> 16 bit unsigned integer
;;; (WORD-LOW-BITS  <word>) -> 16 bit unsigned integer
;;;   Low-level word destructurers.
(define (word-high-bits word) (car word))
(define (word-low-bits  word) (cdr word))

;;; (MAKE-WORD <32-bit-unsigned-integer>) -> word
;;;   High-level word constructor.
(define (integer->word k)
  ;; Make sure that K won't ever cause us to overflow into bignums.  16
  ;; bits is the most we'll consider letting be in the high bits field.
  (if (zero? (arithmetic-shift k -32))
      (make-word (high-bits k 16)
                 (low-bits  k 16))
      (error "Not a 32-bit unsigned integer" k)))

;;; (WORD->INTEGER <word>) -> 32 bit unsigned integer
;;;   High-level word destructurer.
(define (word->integer word)
  (adjoin-bits (word-high-bits word)
               (word-low-bits  word)
               16))

;;; (WORDS->INTEGER <word-vector>) -> integer
;;;   Convert a big-endian vector of words to a bignum.
(define (words->integer words)
  (do ((len (vector-length words))
       (i 0 (+ i 1))
       (width 0 (+ width 32))
       (integer 0 (adjoin-bits (word->integer (vector-ref words i))
                               integer
                               width)))
      ((= i len) integer)))

;;; (INTEGER->WORDS <integer>) -> word-vector
;;;   Convert a bignum to a big-endian vector of words.
(define (integer->words integer)
  (if (zero? (arithmetic-shift integer -32)) ;+++
      (vector (integer->word integer))
      (let loop ((integer integer) (words '()) (count 0))
        (if (zero? integer)
            (reverse-list->vector words count)
            (loop (arithmetic-shift integer -32)
                  (cons (integer->word (low-bits integer 32)) words)
                  (+ count 1))))))

(define (word+ word1 word2)
  (let ((t1 (+ (word-high-bits word1)
               (word-high-bits word2)))
        (t2 (+ (word-low-bits  word1)
               (word-low-bits  word2))))
    (make-word (bitwise-and (+ t1 (high-bits t2 16)) #xFFFF)
               (bitwise-and t2 #xFFFF))))

;;; (WORD4+ <word1> <word2> <word3> <word4>) -> word
;;;   Add four words.  This comes in handy in the MD5 package without
;;;   needing the overhead of handling a general n-ary case.
(define (word4+ word1 word2 word3 word4)
  (word+ (word+ (word+ word1 word2) word3) word4))

(define (word-unop bitwise-op)
  (lambda (word)
    (make-word (bitwise-op (word-high-bits word))
               (bitwise-op (word-low-bits  word)))))

(define (word-binop bitwise-op)
  (lambda (word1 word2)
    (make-word (bitwise-op (word-high-bits word1)
                           (word-high-bits word2))
               (bitwise-op (word-low-bits  word1)
                           (word-low-bits  word2)))))

(define word-not
  (word-unop (lambda (bits)
               (bitwise-and (bitwise-not bits) #xFFFF))))

(define word-ior (word-binop bitwise-ior))
(define word-xor (word-binop bitwise-xor))
(define word-and (word-binop bitwise-and))

;;; (WORD-SHIFT <word> <k>) -> word
;;;   Shift WORD left K bits.
(define word-shift
  (let*
      ((masks '#(#x0    #x1    #x3    #x7
                 #x0F   #x1F   #x3F   #x7F
                 #x0FF  #x1FF  #x3FF  #x7FF
                 #x0FFF #x1FFF #x3FFF #x7FFF
                 #x0FFFF))
       (rotate
        (lambda (high low k)
          (let ((k* (- 16 k)) (k-* (- k 16)))
            (let ((mask  (vector-ref masks k))
                  (mask* (vector-ref masks k*)))
              (make-word
               (bitwise-ior
                (arithmetic-shift (bitwise-and high mask*) k)
                (bitwise-and (arithmetic-shift low  k-*) mask))
               (bitwise-ior
                (arithmetic-shift (bitwise-and low  mask*) k)
                (bitwise-and (arithmetic-shift high k-*) mask))))))))
    (lambda (word k)
      (cond ((< 0 k 16)
             (rotate (word-high-bits word)
                     (word-low-bits  word)
                     k))
            ((< 15 k 32)
             (rotate (word-low-bits  word)
                     (word-high-bits word)
                     (- k 16)))
            (else
             (error "Shift out of range"
                    word-shift word k))))))

;;; (WORD->BYTE-STRING <word>) -> little-endian byte string of 4 bytes
(define (word->byte-string word)
  (let ((s (make-byte-string 4 0)))
    (byte-string-set! s 0 (low-bits  (word-low-bits  word) 8))
    (byte-string-set! s 1 (high-bits (word-low-bits  word) 8))
    (byte-string-set! s 2 (low-bits  (word-high-bits word) 8))
    (byte-string-set! s 3 (high-bits (word-high-bits word) 8))
    s))

;;; (BYTE-STRING->WORD <byte-string> [<start>]) -> word
;;;   START may be passed to specify where to begin obtaining four
;;;   bytes from which to create a word.
(define (byte-string->word s . maybe-start)
  (let ((start (cond ((null? maybe-start) 0)
                     ((null? (cdr maybe-start)) (car maybe-start))
                     (else (error "Too many arguments"
                                  (cons byte-string->word
                                        (cons s maybe-start)))))))
    (make-word (adjoin-bits (byte-string-ref s (+ start 3))
                            (byte-string-ref s (+ start 2))
                            8)
               (adjoin-bits (byte-string-ref s (+ start 1))
                            (byte-string-ref s    start)
                            8))))

;;; (WORDS->BYTE-STRING <word-vector>) -> byte-string
(define (words->byte-string words)
  (let ((len (* (vector-length words) 4)))
    (do ((new (make-byte-string len 0))
         (i 0 (+ i 1))
         (j 0 (+ j 4)))
        ((= j len) new)
      (byte-string-copy! new
                         j
                         (word->byte-string (vector-ref words i))
                         0
                         4))))

;;; (BYTE-STRING->WORDS <byte-string>) -> word-vector
(define (byte-string->words bs)
  (let ((len (quotient (byte-string-length bs) 4)))
    (do ((new (make-vector len))
         (i 0 (+ i 1))
         (j 0 (+ j 4)))
        ((= i len) new)
      (vector-set! new i (byte-string->word bs j)))))
