module: crypto-utils
synopsis: 
author: Taylor Campbell, Gabor Greif
copyright: see below

/*
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
*/

// ;;; The next four ought to be in SRFI 33 (bitwise).

let bitwise-not = lognot;
let arithmetic-shift = ash;

define macro scheme-bindings-definer

  { define scheme-bindings ?bindings end }
  =>
  { ?bindings }

  binding:
    { (define-scm (?:name ?args) ?sexpr) }
    =>
    { define function ?name(?args) ?sexpr end }

  bindings:
    {} => {}
    { ?binding; ... } => { ?binding, ... }

  args:
    {} => {}
    { ?:name ... } => { ?name, ... }

  sexpr:
    { (?sexpr  ?sexprs) } => { ?sexpr(?sexprs) }
    { ?:expression } => { ?expression }

  sexprs:
    {} => {}
    { ?sexpr ... } => { ?sexpr, ... }
end macro;

define scheme-bindings

/// (define-scm (bit-mask size) (bitwise-not (arithmetic-shift -1 size)))


/*
(define-scm (adjoin-bits high low width)
  (bitwise-ior (arithmetic-shift high width) low))
(define-scm (high-bits  n k)
  (arithmetic-shift n (- k)))
(define-scm (low-bits   n k)
  (bitwise-and n (bit-mask k)))

// ;;; I'm not sure what these two are for.
(define-scm (integer->byte-string integer)
  (do ((integer integer (arithmetic-shift integer -8))
       (count 0 (+ count 1))
       (bytes '() (cons (bitwise-and integer #xFF) bytes)))
      ((zero? integer)
       (reverse-list->byte-string bytes count))))
(define-scm (byte-string->integer s)
  (do ((len (byte-string-length s))
       (width 0 (+ width 8))
       (i 0 (+ i 1))
       (integer 0 (adjoin-bits (byte-string-ref s i) integer width)))
      ((= i len) integer)))

(define-scm (number->hex n)
  (let ((s (number->string n 16)))
    (if (= (string-length s) 1)
        (string-append "0" s)
        s)))

(define-scm (hex-byte-string bs)
  (let* ((len (string-length bs))
         (s (make-string (* len 2))))
    (do ((i 0 (+ i 1))
         (j 0 (+ j 2)))
        ((= i len) s)
      (let ((h (number->hex (byte-string-ref bs i))))
        (string-set! s j       (string-ref h 0))
        (string-set! s (+ j 1) (string-ref h 1))))))

(define-scm (subvector vec start end)
  (let ((new (make-vector (- end start))))
    (do ((i 0     (+ i 1))
         (j start (+ j 1)))
        ((= j end) new)
      (vector-set! new i (vector-ref vec j)))))

(define-scm (reverse-list->vector l s)
  (do ((new (make-vector s))
       (i (- s 1) (- i 1))
       (l l (cdr l)))
      ((or (null? l) (negative? i)) new)
    (vector-set! new i (car l))))
*/


/*
(define-scm-syntax receive
  (syntax-rules ()
    ((receive ?formals ?producer ?body1 ?body2 ...)
     (call-with-values (lambda () ?producer)
       (lambda ?formals ?body1 ?body2 ...)))))
*/

end scheme-bindings;