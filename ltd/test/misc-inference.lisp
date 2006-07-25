;;; -*- Mode: LISP; Syntax: Common-lisp; Package: DTP; Base: 10 -*-

;;;----------------------------------------------------------------------------
;;;
;;;	File		Misc-Inference.Lisp
;;;	System		Don's Theorem Prover
;;;
;;;	Written by	Don Geddis (Geddis@CS.Stanford.Edu)

(in-package "DTP")

;;;----------------------------------------------------------------------------
;;;
;;;	Generic functions

(defgeneric expand (node)
  (:documentation "Attempt to find another <answer> for NODE") )

(defgeneric exhausted-p (node &key &allow-other-keys)
  (:documentation "Test whether there might be more answers from NODE") )

(defgeneric propagate (answer node)
  #+dtp-types (declare (type answer answer))
  (:documentation "Inform NODE that ANSWER is another answer") )

;;;----------------------------------------------------------------------------
