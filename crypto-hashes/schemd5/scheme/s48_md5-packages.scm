;;;;;; ScheMD5 package definitions for Scheme48

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

(define-structures ((md5 md5-interface)
                    (md5-internal md5-internal-interface))
  (open scheme
        srfi-9
        byte-strings
        md5-util
        words
;++     bignum-words
        bitwise)
  (optimize auto-integrate)
  (files md5)

  (open (subset define-record-types (define-record-discloser)))
  (begin
    (define-record-discloser rtd/md5-digest
      (lambda (digest)
        (list 'md5-digest (md5-digest->string digest))))
    (define-record-discloser rtd/md5-context
      (lambda (context)
        (list 'md5-context)))))

(define-structure bignum-words words-interface
  (open scheme
        md5-util
        bitwise
        byte-strings
        (subset signals (error)))
  (optimize auto-integrate)
  (files bignum-words)

  (for-syntax (open scheme bitwise md5-util))
  (begin
    (define-syntax bytes->word
      (lambda (form rename compare)
        (adjoin-bits (cadr form)
          (adjoin-bits (caddr form)
            (adjoin-bits (cadddr form)
              (car (cddddr form))
              8)
            16)
          24)))
    (define-syntax remove-needless-bits
      (lambda (form rename compare)
        (bitwise-and (cadr form) (bit-mask 32))))
    (define-syntax word
      (syntax-rules ()
        ((word ?integer)
         (remove-needless-bits ?integer))
        ((word ?b0 ?b1 ?b2 ?b3)
         (bytes->word ?b3 ?b2 ?b1 ?b0))))))

(define-structure words words-interface
  (open scheme
        bitwise
        md5-util
        byte-strings
        (subset signals (error)))
  (optimize auto-integrate)
  (files words)

  ;; Implementation-specific WORD macro for literal constant words.
  (for-syntax
    (open scheme
          md5-util
          bitwise
          (subset signals (syntax-error)))
    (begin
      (define (integer->word integer rename)
        `(,(rename 'quote) (,(high-bits integer 16)
                            .
                            ,(low-bits  integer 16))))
      (define (four-bytes->word b0 b1 b2 b3 rename)
        `(,(rename 'quote) (,(adjoin-bits b3 b2 8)
                            .
                            ,(adjoin-bits b1 b0 8))))))

  (begin
    (define-syntax word
      (lambda (form rename compare)
        (define (die)
          (syntax-error
           "Bad syntax; expected one 32-bit int or four 8-bit ints"
           form))
        ;; Sophisticated form checking.
        (if (null? (cdr form))
            (die)
            (if (pair? (cdr form))
                (if (null? (cddr form))
                    (if (and (integer? (cadr form))
                             (exact?   (cadr form))
                             (<= 0
                                 (cadr form)
                                 (- (arithmetic-shift 1 32) 1)))
                        (integer->word (cadr form) rename)
                        (die))
                    (if (and (pair?         (cddr   form))
                             (pair?         (cdddr  form))
                             (pair?         (cddddr form))
                             (null?    (cdr (cddddr form)))
                             (integer?      (cadr   form))
                             (integer?      (caddr  form))
                             (integer?      (cadddr form))
                             (integer? (car (cddddr form)))
                             (exact?        (cadr   form))
                             (exact?        (caddr  form))
                             (exact?        (cadddr form))
                             (exact?   (car (cddddr form)))
                             (let ((l (- (arithmetic-shift 1 8) 1)))
                               (and (<= 0      (cadr   form)  l)
                                    (<= 0      (caddr  form)  l)
                                    (<= 0      (cadddr form)  l)
                                    (<= 0 (car (cddddr form)) l))))
                        (four-bytes->word      (cadr   form)
                                               (caddr  form)
                                               (cadddr form)
                                          (car (cddddr form))
                                          rename)
                        (die)))
                (die)))))))

(define-structure md5-util md5-util-interface
  (open scheme
        bitwise
        byte-strings)
  (optimize auto-integrate)
  (files util))

(define-structure byte-strings byte-strings-interface
  (open scheme
        ascii
        silly
        (subset primitives (copy-bytes!)))
  (optimize auto-integrate)
  (files byte-strings)

  ;; Implementation-specific components.
  (begin
    (define (byte-string-copy! target tstart source sstart count)
      (copy-bytes! source sstart target tstart count))
    (define (reverse-list->byte-string l size)
      ;; I hope MAP doesn't bog this down to make the boost in speed by
      ;; using VM primitives become irrelevant.  It's still faster than
      ;; the manual DO loop with manual BYTE-STRING-SET!s.
      (let ((s (reverse-list->string (map ascii->char l) size))
            (bs (make-byte-string size 0)))
        (byte-string-copy! bs 0 s 0 size)
        bs))))
