;;; -*- Mode: LISP; Syntax: Common-lisp; Package: DTP; Base: 10 -*-

;;;----------------------------------------------------------------------------
;;;
;;;	File		Terms.Lisp
;;;	System		Don's Theorem Prover
;;;
;;;	Written by	Don Geddis (Geddis@CS.Stanford.Edu)
;;;
;;;	Extensions to the term simplification routine are possible by
;;;	defining the function "term-inference" in the DTP package.

(in-package "DTP")

;;;----------------------------------------------------------------------------
;;;
;;;	Public

;;;----------------------------------------------------------------------------

(defun nsimplify-terms (literal)
  "Procedurally attach to rewrite ground terms to canonical form"
  #+dtp-types (declare (type literal-node literal))
  (unless *use-procedural-attachments*
    (return-from nsimplify-terms literal) )
  (loop
      for term in (literal-terms literal)
      collect
	(if (groundp term)
	    (rewrite-ground-term term)
	  term )
      into simple-terms
      finally
	(setf (literal-terms literal) simple-terms)
	(return literal) ))

;;;----------------------------------------------------------------------------
;;;
;;;	Private

;;;----------------------------------------------------------------------------

;;; This list was copied from Epikit's list of attached functions, after
;;; removing the following special cases:
;;;	denotation, eval, execute, list, name
;;; (The function eval is treated specially.)
;;; All of these are arithmetic operations on numbers.

(defparameter *attached-lisp-functions*
    '(+ - * / 1+ 1-
      abs acos acosh ash asin asinh atan atanh
      boole ceiling cis complex conjugate cos cosh decode-float denominator
      exp expt fceiling ffloar float float-digits float-precision float-radix
      float-sign floor fround ftruncate gcd imagpart integer-decode-float
      integer-length isqrt lcm log logand logandc1 logandc2 logcount logeqv
      logior lognand lognor lognot logorc1 logorc2 logxor max min mod
      numerator phase rational rationalize realpart rem round
      scale-float signum sin sinh sqrt tan tanh truncate )
  "Ground terms with these functions will be simplified by calling Lisp" )

;;;----------------------------------------------------------------------------

(defun rewrite-ground-term (term)
  (cond
   ((atom term)
    term )
   ((eq (first term) 'eval)
    (ignore-errors (apply (first term) (rest term))) )
   ((find (first term) *attached-lisp-functions*)
    ;; Must be an arithmetic function, so error/NIL -> 0
    (let ((new-term (ignore-errors (eval term))))
      (or new-term 0) ))
   ((fboundp 'term-inference)
    (funcall (symbol-function 'term-inference) term) )
   (t
    term )))

;;;----------------------------------------------------------------------------
