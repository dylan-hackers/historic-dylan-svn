;;; -*- Mode: LISP; Syntax: Common-lisp; Package: DTP; Base: 10 -*-

;;;----------------------------------------------------------------------------
;;;
;;;	File		Backtrack.Lisp
;;;	System		Don's Theorem Prover
;;;
;;;	Written by	Don Geddis (Geddis@CS.Stanford.Edu)

(in-package "DTP")

;;;----------------------------------------------------------------------------
;;;
;;;	Public

;;;----------------------------------------------------------------------------

(defun backtrack (conjunction)
  "No answers were found, so back up to where a relevant variable was bound"
  #+dtp-types (declare (type dtp-conjunction conjunction))
  (compute-nogoods conjunction)
  (backjump conjunction)
  (maybe-expand-conjunction conjunction) )

;;;----------------------------------------------------------------------------
;;;
;;;	Private

;;;----------------------------------------------------------------------------

(defun compute-nogoods (conjunction)
  "Record the index of previous conjuncts that bound variables in failed one"
  #+dtp-types (declare (type dtp-conjunction conjunction))
  (unless *use-backjumping*
    (return-from compute-nogoods) )
  (with-slots (stack-pointer backtrack-pointer) conjunction
    (when (= stack-pointer backtrack-pointer)
      (return-from compute-nogoods) ))
  (with-slots (literal nogoods) (active-conjunct conjunction)
    (when (eq nogoods :uninitialized)
      (setq nogoods nil) )
    (loop
	with failed-vars = (literal-vars-in literal)
	for conjunct-index from 0
	for answer in (reverse (slot-value conjunction 'stack))
	when (some #'(lambda (var) (answer-binds-var-p answer var))
		   failed-vars )
	do (pushnew conjunct-index nogoods :test #'=) )
    ))

;;;----------------------------------------------------------------------------

(defun backjump (conjunction)
  "Back up conjunction to most recent nogood justification"
  #+dtp-types (declare (type dtp-conjunction conjunction))
  (with-slots (list stack stack-pointer backtrack-pointer) conjunction
    #+dtp-trace
    (when (find :backtrack *trace*)
      (indent-line)
      (if (or (not *use-backjumping*)
	      (= stack-pointer backtrack-pointer) )
	  (format *debug-io* "Backtracking")
	(format *debug-io* "Backjumping") )
      (format *debug-io* " from ")
      (output-conjunct-in-context conjunction)
      (format *debug-io* " to ") )
    (let ((nogoods (slot-value (nth stack-pointer list) 'nogoods)))
      (when (or (not *use-backjumping*)
		(= stack-pointer backtrack-pointer) )
	(when *use-backjumping* (decf backtrack-pointer))
	;; Chronological backtracking
	(if (= stack-pointer 0)
	    (setq nogoods nil)
	  (setq nogoods (list (1- stack-pointer))) ))
      (when (and (typep conjunction 'dtp-forked-conjunction)
		 (<= stack-pointer (slot-value conjunction 'top-conjunct)) )
	(setq nogoods nil) )
      (if nogoods
	  (loop
	      with new-index = (max (reduce #'max nogoods) backtrack-pointer)
	      for index from stack-pointer downto (1+ new-index)
	      for conjunct = (nth index list)
	      for subgoal = (slot-value conjunct 'subgoal)
	      do
		(when (typep subgoal 'dtp-subgoal)
		  (with-slots
		      (conjuncts-to-propagate-to #+dtp-trace used-conjuncts)
		      subgoal
		    (setf conjuncts-to-propagate-to
		      (remove conjunct conjuncts-to-propagate-to) )
		    #+dtp-trace
		    (add-to-end conjunct used-conjuncts) ))
		(reset conjunct #+dtp-trace :in-trace #+dtp-trace t)
		(pop stack)
		(decf stack-pointer)
		#+dtp-trace
		(when (find :backtrack *trace*)
		  (output-conjunct-in-context conjunction)
		  (format *debug-io* "...") )
	      finally
		(setq conjunct (nth new-index list))
		(setf (slot-value conjunct 'nogoods)
		  (union
		   (unless (eq (slot-value conjunct 'nogoods) :uninitialized)
		     (slot-value conjunct 'nogoods) )
		   (remove-if-not #'(lambda (n) (< n new-index)) nogoods)
		   :test #'= )))
	(progn
	  #+dtp-trace
	  (when (find :backtrack *trace*)
	    (format *debug-io* "Exhausted") )
	  (dolist (conjunct list)
	    (reset conjunct #+dtp-trace :in-trace #+dtp-trace t) )
	  (setq stack nil)
	  (setq stack-pointer -1) )))
    #+dtp-trace
    (when (find :backtrack *trace*)
      (format *debug-io* "~%") )
    ))

;;;----------------------------------------------------------------------------

#+dtp-trace
(defun output-conjunct-in-context (conjunction)
  #+dtp-types (declare (type dtp-conjunction conjunction))
  (let ((conjunct (active-conjunct conjunction)))
    (if conjunct
	(print-literal-node
	 (literal-plug
	  (slot-value conjunct 'literal)
	  (slot-value conjunct 'binding-list) )
	 :s *debug-io* )
      (format *debug-io* "Exhausted") )))

;;;----------------------------------------------------------------------------

(defun maybe-expand-conjunction (conjunction)
  "Unless waiting for next subgoal answer, get the next answer"
  #+dtp-types (declare (type dtp-conjunction conjunction))
  (let ((conjunct (active-conjunct conjunction)))
    #+dtp-types (declare (type (or dtp-conjunct null) conjunct))
    (when conjunct
      (with-slots (subgoal) conjunct
	(unless
	    (and
	     (typep subgoal 'dtp-subgoal)
	     (find conjunct (slot-value subgoal 'conjuncts-to-propagate-to)) )
	  (expand conjunction) )))
    ))

;;;----------------------------------------------------------------------------
