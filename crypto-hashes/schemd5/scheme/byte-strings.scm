;;;;;; Byte strings: homogenous sequences of 8-bit integers

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

;;; Byte strings -- byte vectors implemented as strings.  This keeps us
;;; from depending on SRFI 4 or SRFI 47 or whatever your favourite byte
;;; vector-like device is, and it also keeps us from converting between
;;; strings and byte vectors all over the place, while still being able
;;; to have a byte string abstraction.

(define (make-byte-string length fill)
  (make-string length (ascii->char fill)))
(define (byte-string . elements) (list->byte-string elements))

(define (byte-string-ref string k)
  (char->ascii (string-ref string k)))
(define (byte-string-set! string k byte)
  (string-set! string k (ascii->char byte)))

(define (string->byte-string string) string)
(define (byte-string->string string) string)

(define (byte-string->list s)
  (do ((i (- (byte-string-length s) 1) (- i 1))
       (l '() (cons (byte-string-ref s i) l)))
      ((negative? i) l)))
(define (list->byte-string l)
  (let ((len (length l)))
    (do ((new (make-byte-string len 0))
         (i 0 (+ i 1))
         (l l (cdr l)))
        ((= i len) new)
      (byte-string-set! new i (car l)))))

(define (byte-string? x) (string? x))

(define (byte-string-length s) (string-length s))

(define (byte-string-append . strings) (apply string-append strings))
(define (byte-substring   s start end) (substring   s start end))
(define (byte-string-copy s)           (string-copy s))

;;; BYTE-STRING-COPY! is implemented in the implementation-specific
;;; section of this module.  This allows for using the wicked fast VM
;;; primitives or whatever, as it's low-level and necessarily fast.
;;; The same can be said of REVERSE-LIST->BYTE-STRING.
