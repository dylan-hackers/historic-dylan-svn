;;;;;; ScheMD5 interface definitions for Scheme48

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

(define-interface md5-interface
  (export md5-digest-for-string
;++       md5-digest-for-port           ;++ implement

          md5-digest?

          md5-digest->words
          md5-digest->number
          md5-digest->string

          words->md5-digest
          number->md5-digest
          string->md5-digest

          make-md5-context
          md5-context?
          update-md5-context update-md5-context!
          md5-context->md5-digest))

(define-interface md5-internal-interface
  (export number->md5-string
          md5-context-state md5-context-bit-count
          string->md5-word-vector
          md5/F md5/G md5/H md5/I
          md5-transform))

(define-interface words-interface
  (export make-word
          integer->word integer->words
          word->integer words->integer
          (word :syntax)
          word?
          word-high-bits word-low-bits
          word+ word4+
          word-not word-ior word-xor word-and
          word-shift
          word->byte-string words->byte-string
          byte-string->word byte-string->words))

(define-interface md5-util-interface
  (export bit-mask adjoin-bits high-bits low-bits
          integer->byte-string byte-string->integer
          number->hex hex-byte-string
          subvector
          reverse-list->vector
          (receive :syntax)))

(define-interface byte-strings-interface
  (export make-byte-string byte-string
          byte-string?
          byte-string-length
          byte-string-ref byte-string-set!
          string->byte-string byte-string->string
          list->byte-string byte-string->list
          byte-string-append
          byte-substring
          byte-string-copy
          byte-string-copy!
          reverse-list->byte-string))
