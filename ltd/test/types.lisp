;;; -*- Mode: LISP; Syntax: Common-lisp; Package: DTP; Base: 10 -*-

;;;----------------------------------------------------------------------------
;;;
;;;	File		Types.Lisp
;;;	System		Don's Theorem Prover
;;;
;;;	Written by	Don Geddis (Geddis@CS.Stanford.Edu)

(in-package "DTP")

;;;----------------------------------------------------------------------------

(deftype binding-list ()
  "Mapping from variables to bindings, implemented as a list of conses"
  'list )

(deftype binding ()
  "A single map, assigning a variable to a value, implemented as a cons"
  'cons )

;;;----------------------------------------------------------------------------

(deftype boolean ()
  "T or NIL"
  '(or (eql t) null) )

;;;----------------------------------------------------------------------------

(deftype natural-number ()
  "Integer greater than or equal to zero"
  '(integer 0 *) )

;;;----------------------------------------------------------------------------
