;;; -*- Mode: LISP; Syntax: Common-lisp; Package: DTP; Base: 10 -*-

;;;----------------------------------------------------------------------------
;;;
;;;	File		Residue.Lisp
;;;	System		Don's Theorem Prover
;;;
;;;	Written by	Don Geddis (Geddis@CS.Stanford.Edu)

(in-package "DTP")

;;;----------------------------------------------------------------------------
;;;
;;;	Public

(defun residue-answer (subgoal assumable)
  #+dtp-types (declare (type dtp-subgoal subgoal))
  #+dtp-types (declare (type literal-node assumable))
  (with-slots (literal) subgoal
    (when (literal-instance assumable literal)
      (make-answer
       :residue (list literal)
       #+dtp-trace :justification
       #+dtp-trace (make-res-justification :assumable assumable)
       #+dtp-trace :proof #+dtp-trace *proof* )
      )))

;;;----------------------------------------------------------------------------

(defun residue-merge (residue1 residue2)
  (let ((new-res (append residue1 residue2)))
    (if (proof-consistency-check *proof*)
	(if (funcall (proof-consistency-check *proof*) new-res)
	    new-res
	  :not-a-residue )
      new-res )))

;;;----------------------------------------------------------------------------

(defun residue-equal-p (residue1 residue2)
  (set-equal residue1 residue2 #'literal-equal-p) )

;;;----------------------------------------------------------------------------

(defun residue-instance? (instance general)
  "True IFF the INSTANCE residue is more specific than (or equal to) GENERAL"
  (subsetp general instance :test #'literal-instance?) )

;;;----------------------------------------------------------------------------
