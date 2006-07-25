;;; -*- Mode: LISP; Syntax: Common-lisp; Package: DTP; Base: 10 -*-

;;;----------------------------------------------------------------------------
;;;
;;;	File		Labels.Lisp
;;;	System		Don's Theorem Prover
;;;
;;;	Written by	Don Geddis (Geddis@CS.Stanford.Edu)

(in-package "DTP")

;;;----------------------------------------------------------------------------

(defstruct label-structure
  name
  maximum
  minimum
  and-fn
  or-fn )

(defstruct (label
	    (:print-function label-print-function) )
  value
  structure )

(defun label-print-function (structure stream depth)
  (declare (ignore depth))
  (format stream "<Label ~A>" (label-value structure)) )

;;;----------------------------------------------------------------------------

(defvar *label-structures* nil)

;;;----------------------------------------------------------------------------

(defun get-label-structure (name)
  (find name *label-structures* :key #'label-structure-name) )

;;;----------------------------------------------------------------------------

(defmacro create-label-structure (name max min and-fn or-fn)
  `(let (ls)
     (setq ls (get-label-structure ',name))
     (if ls
	 (setf (label-structure-maximum ls) ',max
	       (label-structure-minimum ls) ',min
	       (label-structure-and-fn ls) ',and-fn
	       (label-structure-or-fn ls) ',or-fn )
       (add-to-end
	(make-label-structure
	 :name ',name :maximum ',max :minimum ',min
	 :and-fn #',and-fn :or-fn #',or-fn )
	*label-structures* ))
     ))

;;;----------------------------------------------------------------------------

(defun label-equal-p (label1 label2)
  (if (or (null label1) (null label2))
      (and (null label1) (null label2))
    (and (equal (label-value label1) (label-value label2))
	 (eq (label-structure label1) (label-structure label2)) )))

;;;----------------------------------------------------------------------------

(defun label-instance? (instance general)
  "True IFF INSTANCE is more specific (or equal) to GENERAL"
  (cond
   ((null general)
    t )
   ((null instance)
    nil )
   (t
    (and (equal (label-value instance) (label-value general))
	 (eq (label-structure instance) (label-structure general)) ))
   ))

;;;----------------------------------------------------------------------------

(defun label-and (label1 label2)
  (cond
   ((null label1)
    label2 )
   ((null label2)
    label1 )
   (t
    (let ((ls1 (label-structure label1))
	  (ls2 (label-structure label2)) )
      (if (and ls1 ls2 (eq ls1 ls2))
	  (make-label
	   :value (apply (label-structure-and-fn ls1)
			 (label-value label1)
			 (label-value label2) )
	   :structure ls1 )
	(make-label :value 'error :structure nil) ))
    )))

;;;----------------------------------------------------------------------------
;;;
;;;	First-order logic
;;;
;;;	Values: T (for true), NIL (for false)

(defun fo-and (&rest labels)
  (eval (cons 'and labels)) )

(defun fo-or (&rest labels)
  (eval (cons 'or labels)) )

(create-label-structure
 first-order
 t nil fo-and fo-or )

;;;----------------------------------------------------------------------------
;;;
;;;	Qualitative likelihoods

(defparameter *ql-values* '(false default-false unknown default-true true))

(defun ql-and (&rest labels)
  (setq labels
    (mapcar #'(lambda (x) (position (label-value x) *ql-values*)) labels) )
  (nth (apply #'min labels) *ql-values*) )

(defun ql-or (&rest labels)
  (setq labels
    (mapcar #'(lambda (x) (position (label-value x) *ql-values*)) labels) )
  (nth (apply #'max labels) *ql-values*) )

(create-label-structure
 qualitative-likelihoods
 true false ql-and ql-or )

;;;----------------------------------------------------------------------------
;;;
;;;	Fuzzy Logic
;;;
;;;	[From Charles Elkan, _The Paradoxical Success of Fuzzy Logic_,
;;;	 AAAI-93, pp 698-703.]
;;;
;;;	1. Truth value t(A) of an assertion A: 0 <= t(A) <= 1
;;;	2. t(A ^ B) = min{t(A),t(B)}
;;;	3. t(A v B) = max{t(A),t(B)}
;;;	4. t(- A) = 1 - t(A)
;;;	5. t(A) = t(B) if A and B are logically equivalent

(create-label-structure
 fuzzy-logic
 1 0 max min )

;;;----------------------------------------------------------------------------
;;;
;;;	Mycin Certainty Factors

#|
(create-label-structure
 mycin
 1 -1 mycin-and mycin-or )
|#

;;;----------------------------------------------------------------------------
