;;;;;; Various utilities for ScheMD5

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

;;; The next four ought to be in SRFI 33 (bitwise).
(define (bit-mask size) (bitwise-not (arithmetic-shift -1 size)))

(define (adjoin-bits high low width)
  (bitwise-ior (arithmetic-shift high width) low))
(define (high-bits  n k)
  (arithmetic-shift n (- k)))
(define (low-bits   n k)
  (bitwise-and n (bit-mask k)))

;;; I'm not sure what these two are for.
(define (integer->byte-string integer)
  (do ((integer integer (arithmetic-shift integer -8))
       (count 0 (+ count 1))
       (bytes '() (cons (bitwise-and integer #xFF) bytes)))
      ((zero? integer)
       (reverse-list->byte-string bytes count))))
(define (byte-string->integer s)
  (do ((len (byte-string-length s))
       (width 0 (+ width 8))
       (i 0 (+ i 1))
       (integer 0 (adjoin-bits (byte-string-ref s i) integer width)))
      ((= i len) integer)))

(define (number->hex n)
  (let ((s (number->string n 16)))
    (if (= (string-length s) 1)
        (string-append "0" s)
        s)))

(define (hex-byte-string bs)
  (let* ((len (string-length bs))
         (s (make-string (* len 2))))
    (do ((i 0 (+ i 1))
         (j 0 (+ j 2)))
        ((= i len) s)
      (let ((h (number->hex (byte-string-ref bs i))))
        (string-set! s j       (string-ref h 0))
        (string-set! s (+ j 1) (string-ref h 1))))))

(define (subvector vec start end)
  (let ((new (make-vector (- end start))))
    (do ((i 0     (+ i 1))
         (j start (+ j 1)))
        ((= j end) new)
      (vector-set! new i (vector-ref vec j)))))

(define (reverse-list->vector l s)
  (do ((new (make-vector s))
       (i (- s 1) (- i 1))
       (l l (cdr l)))
      ((or (null? l) (negative? i)) new)
    (vector-set! new i (car l))))

(define-syntax receive
  (syntax-rules ()
    ((receive ?formals ?producer ?body1 ?body2 ...)
     (call-with-values (lambda () ?producer)
       (lambda ?formals ?body1 ?body2 ...)))))
