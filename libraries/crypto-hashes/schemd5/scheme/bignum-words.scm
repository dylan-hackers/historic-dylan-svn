;;;;;; Words implemented using ordinary numbers

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

;;; words.scm implements words as pairs of fixnums, which will likely
;;; be _much_ more efficient.  This is not the case, however, for some
;;; implementations, such as Scheme48, and this is sometimes beneficial
;;; for automated debugging output.

(define (make-word high low)
  (adjoin-bits high low 16))

(define (integer->word integer) integer)
(define (word->integer word) word)

(define (words->integer words)
  (do ((len (vector-length words))
       (i 0 (+ i 1))
       (width 0 (+ width 1))
       (integer 0 (adjoin-bits (word->integer (vector-ref words i))
                               integer
                               width)))
      ((= i len) integer)))

(define (integer->words integer)
  (if (zero? (arithmetic-shift integer -32)) ;+++
      (vector (integer->word integer))
      (let loop ((integer integer) (words '()) (count 0))
        (if (zero? integer)
            (reverse-list->vector words count)
            (loop (arithmetic-shift integer -32)
                  (cons (integer->word (low-bits integer 32)) words)
                  (+ count 1))))))

(define (word? x)
  (and (integer? x)
       (exact? x)
       (zero? (arithmetic-shift x -32))))

(define (word-high-bits word) (high-bits word 16))
(define (word-low-bits  word) (low-bits  word 16))

(define (without-overflow n) (bitwise-and n (bit-mask 32)))

(define (word+ w1 w2) (without-overflow (+ w1 w2)))
(define (word4+ w1 w2 w3 w4) (without-overflow (+ w1 w2 w3 w4)))

(define (word-not word) (without-overflow (bitwise-not word)))
(define (word-ior w1 w2) (bitwise-ior w1 w2))
(define (word-xor w1 w2) (bitwise-xor w1 w2))
(define (word-and w1 w2) (bitwise-and w1 w2))

(define (word-shift word k)
  (bitwise-ior (arithmetic-shift word k)
               (arithmetic-shift word (- k 32))))

(define (word->byte-string word)
  (let ((s (make-byte-string 4 0)))
    (byte-string-set! s 3 (high-bits word 24))
    (byte-string-set! s 2 (high-bits (low-bits word 24) 16))
    (byte-string-set! s 1 (high-bits (low-bits word 16) 8))
    (byte-string-set! s 0 (low-bits word 8))
    s))

(define (byte-string->word s . maybe-start)
  (let ((start (cond ((null? maybe-start) 0)
                     ((null? (cdr maybe-start))
                      (car maybe-start))
                     (else
                      (error "Too many arguments"
                             byte-string->word
                             (cons s maybe-start))))))
    (make-word (adjoin-bits (byte-string-ref s (+ start 3))
                            (byte-string-ref s (+ start 2))
                            8)
               (adjoin-bits (byte-string-ref s (+ start 1))
                            (byte-string-ref s    start)
                            8))))

(define (words->byte-string words)
  (let ((len (* (vector-length words) 4)))
    (do ((new (make-byte-string len 0))
         (i 0 (+ i 1))
         (j 0 (+ j 4)))
        ((= j len) new)
      (let ((sub (word->byte-string (vector-ref words i))))
        (byte-string-set! new    j    (byte-string-ref sub 0))
        (byte-string-set! new (+ j 1) (byte-string-ref sub 1))
        (byte-string-set! new (+ j 2) (byte-string-ref sub 2))
        (byte-string-set! new (+ j 3) (byte-string-ref sub 3))))))

(define (byte-string->words s)
  (let ((len (quotient (byte-string-length s) 4)))
    (do ((new (make-vector len))
         (i 0 (+ i 1))
         (j 0 (+ j 4)))
        ((= i len) new)
      (vector-set! new i (byte-string->word s j)))))
