;;;;;; ScheMD5 1.0 by Taylor Campbell

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

;;; An MD5 digest record contains three fields: a vector of words (the
;;; most fundamental representation, which the other two usually are
;;; based on), a numerical digest (the numerical representation of that
;;; word vector), and a string (the string representation).
;;;
;;; Either just the word vector or both the number & string will be
;;; initially assigned; the others will be filled in lazily.

(define-record-type rtd/md5-digest
  (really-make-md5-digest words number string)
  md5-digest?
  (words  %md5-digest-words  %md5-digest-set-words!)
  (number %md5-digest-number %md5-digest-set-number!)
  (string %md5-digest-string %md5-digest-set-string!))

(define (md5-digest-for-string string)
  (md5-context->md5-digest
   (update-md5-context (make-md5-context) string)))

(define (number->md5-string n)
  (let ((s (make-string 56 #\0))
        (n (number->string n 16)))
    (do ((i 56                      (- i 1))
         (j (- (string-length n) 1) (- j 1)))
        ((negative? i) s)
      (string-set! s i (string-ref n j)))))

(define (md5-digest->words digest)
  (cond ((%md5-digest-words digest))
        (else (let ((w (integer->words
                        (%md5-digest-number digest))))
                (%md5-digest-set-words! digest w)
                w))))

(define (md5-digest->number digest)
  (cond ((%md5-digest-number digest))
        (else (let ((n (words->integer (%md5-digest-words digest))))
                (%md5-digest-set-number! digest n)
                (%md5-digest-set-string! digest
                  (number->md5-string n))
                n))))

(define (md5-digest->string digest)
  (cond ((%md5-digest-string digest))
        (else
         (md5-digest->number digest)    ; Forcibly compute the number,
         (%md5-digest-string digest)))) ;   which computes the string.

(define (words->md5-digest words)
  (really-make-md5-digest words #f #f))

(define (string->md5-digest string)
  (really-make-md5-digest #f (string->number string 16) string))

(define (number->md5-digest number)
  (really-make-md5-digest #f number (number->md5-string number)))

(define-record-type rtd/md5-context
  (really-make-md5-context state bit-count)
  md5-context?
  (state     md5-context-state)
  (bit-count md5-context-bit-count md5-context-set-bit-count!))

(define (make-md5-context)
  (really-make-md5-context
   ;; Magic constants
   (vector (word #x01 #x23 #x45 #x67)
           (word #x89 #xAB #xCD #xEF)
           (word #xFE #xDC #xBA #x98)
           (word #x76 #x54 #x32 #x10))
   (word 0)))

(define (md5-context->md5-digest context)
  (let ((words (md5-context-state context)))
    (really-make-md5-digest words
                            (words->integer words)
                            (hex-byte-string
                             (words->byte-string words)))))

(define (*update-md5-context context message)
  (let* ((all-words (string->md5-word-vector message))
         (limit (vector-length all-words))
         (state (md5-context-state context)))
    (let loop ((A (vector-ref state 0))
               (B (vector-ref state 1))
               (C (vector-ref state 2))
               (D (vector-ref state 3))
               (k 0))
      (if (= k limit)
          (values
           A B C D
           (word+ (md5-context-bit-count context)
                  (integer->word
                   (arithmetic-shift (string-length message) 3))))
          ;; Save the state to add it at the end of this iteration.
          (let ((A-save A) (B-save B) (C-save C) (D-save D)
                (words (subvector all-words k (+ k 16))))
            (receive (A* B* C* D*)
                     (md5-transform A B C D words)
              ;; Use the words we saved
              (loop (word+ A-save A*)
                    (word+ B-save B*)
                    (word+ C-save C*)
                    (word+ D-save D*)
                    (+ k 16))))))))

(define (update-md5-context context message)
  (receive (A B C D bit-count)
           (*update-md5-context context message)
    (really-make-md5-context (vector A B C D) bit-count)))

(define (update-md5-context! context message)
  (receive (A B C D bit-count)
           (*update-md5-context context message)
    (let ((s (md5-context-state context)))
      (vector-set! s 0 A)
      (vector-set! s 1 B)
      (vector-set! s 2 C)
      (vector-set! s 3 D)
      (md5-context-set-bit-count! context bit-count))))

(define (string->md5-word-vector message)
  (let* ((len (arithmetic-shift (string-length message) 3))
         (zero-bits (bitwise-and (- 448 len) (bit-mask 9)))
         (high (word->byte-string (integer->word (high-bits len 32))))
         (low  (word->byte-string (integer->word (low-bits  len 32)))))
    (byte-string->words
     (byte-string-append
      (string->byte-string message)
      (byte-string #x80)
      (make-byte-string (arithmetic-shift (- zero-bits 1) -3)
                        0)
      low
      high))))

;;; The four core MD5 functions.  I hope these get inlined.
; (define (md5/F x y z)                 ; F is optimized slightly.
;   (word-ior (word-and x y) (word-and (word-not x) z)))
(define (md5/F x y z)
  (word-xor z (word-and x (word-xor y z))))
(define (md5/G x y z)
  (word-xor y (word-and z (word-xor x y))))
(define (md5/H x y z)
  (word-xor x (word-xor y z)))
(define (md5/I x y z)
  (word-xor y (word-ior x (word-not z))))

(define (md5-transform A B C D words)
  ;; Sorry this is imperative, but the only two other ways are:
  ;;   a) Cons billions of closures.  Disadvantage: humungously
  ;;      enormulous amounts of unnecessary closure allocation.
  ;;   b) Use a _really_ complicated loop.  Disadvantages: I've
  ;;      no idea how to write it, and even if I had an idea as
  ;;      to how to, it would be an overly complicated and thus
  ;;      potentially rather performance-wise inefficient loop.
  ;; This macro is local so it can use WORDS without forcing me
  ;; to pass it redundantly every time.
  (let-syntax
      ((frob!
        (syntax-rules ()
          ((frob! ?f ?v ?x ?y ?z ?k ?w ?s)
           (set! ?v
             (word+ ?x
                    (word-shift
                     (word4+ ?v
                             (?f ?x ?y ?z)
                             (vector-ref words ?k)
                             (word ?w))
                     ?s)))))))

    ;; Round 1
    (frob! md5/F A B C D 0  #xD76AA478 7)
    (frob! md5/F D A B C 1  #xE8C7B756 12)
    (frob! md5/F C D A B 2  #x242070DB 17)
    (frob! md5/F B C D A 3  #xC1BDCEEE 22)
    (frob! md5/F A B C D 4  #xF57C0FAF 7)
    (frob! md5/F D A B C 5  #x4787C62A 12)
    (frob! md5/F C D A B 6  #xA8304613 17)
    (frob! md5/F B C D A 7  #xFD469501 22)
    (frob! md5/F A B C D 8  #x698098D8 7)
    (frob! md5/F D A B C 9  #x8B44F7AF 12)
    (frob! md5/F C D A B 10 #xFFFF5BB1 17)
    (frob! md5/F B C D A 11 #x895CD7BE 22)
    (frob! md5/F A B C D 12 #x6B901122 7)
    (frob! md5/F D A B C 13 #xFD987193 12)
    (frob! md5/F C D A B 14 #xA679438E 17)
    (frob! md5/F B C D A 15 #x49B40821 22)

    ;; Round 2
    (frob! md5/G A B C D 1  #xF61E2562 5)
    (frob! md5/G D A B C 6  #xC040B340 9)
    (frob! md5/G C D A B 11 #x265E5A51 14)
    (frob! md5/G B C D A 0  #xE9B6C7AA 20)
    (frob! md5/G A B C D 5  #xD62F105D 5)
    (frob! md5/G D A B C 10 #x02441453 9)
    (frob! md5/G C D A B 15 #xD8A1E681 14)
    (frob! md5/G B C D A 4  #xE7D3FBC8 20)
    (frob! md5/G A B C D 9  #x21E1CDE6 5)
    (frob! md5/G D A B C 14 #xC33707D6 9)
    (frob! md5/G C D A B 3  #xF4D50D87 14)
    (frob! md5/G B C D A 8  #x455A14ED 20)
    (frob! md5/G A B C D 13 #xA9E3E905 5)
    (frob! md5/G D A B C 2  #xFCEFA3F8 9)
    (frob! md5/G C D A B 7  #x676F02D9 14)
    (frob! md5/G B C D A 12 #x8D2A4C8A 20)

    ;; Round 3
    (frob! md5/H A B C D 5  #xFFFA3942 4)
    (frob! md5/H D A B C 8  #x8771F681 11)
    (frob! md5/H C D A B 11 #x6D9D6122 16)
    (frob! md5/H B C D A 14 #xFDE5380C 23)
    (frob! md5/H A B C D 1  #xA4BEEA44 4)
    (frob! md5/H D A B C 4  #x4BDECFA9 11)
    (frob! md5/H C D A B 7  #xF6BB4B60 16)
    (frob! md5/H B C D A 10 #xBEBFBC70 23)
    (frob! md5/H A B C D 13 #x289B7EC6 4)
    (frob! md5/H D A B C 0  #xEAA127FA 11)
    (frob! md5/H C D A B 3  #xD4EF3085 16)
    (frob! md5/H B C D A 6  #x04881D05 23)
    (frob! md5/H A B C D 9  #xD9D4D039 4)
    (frob! md5/H D A B C 12 #xE6DB99E5 11)
    (frob! md5/H C D A B 15 #x1FA27CF8 16)
    (frob! md5/H B C D A 2  #xC4AC5665 23)

    ;; Round 4
    (frob! md5/I A B C D 0  #xF4292244 6)
    (frob! md5/I D A B C 7  #x432AFF97 10)
    (frob! md5/I C D A B 14 #xAB9423A7 15)
    (frob! md5/I B C D A 5  #xFC93A039 21)
    (frob! md5/I A B C D 12 #x655B59C3 6)
    (frob! md5/I D A B C 3  #x8F0CCC92 10)
    (frob! md5/I C D A B 10 #xFFEFF47D 15)
    (frob! md5/I B C D A 1  #x85845DD1 21)
    (frob! md5/I A B C D 8  #x6FA87E4F 6)
    (frob! md5/I D A B C 15 #xFE2CE6E0 10)
    (frob! md5/I C D A B 6  #xA3014314 15)
    (frob! md5/I B C D A 13 #x4E0811A1 21)
    (frob! md5/I A B C D 4  #xF7537E82 6)
    (frob! md5/I D A B C 11 #xBD3AF235 10)
    (frob! md5/I C D A B 2  #x2AD7D2BB 15)
    (frob! md5/I B C D A 9  #xEB86D391 21)

    (values A B C D)))
