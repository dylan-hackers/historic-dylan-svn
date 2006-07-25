;;; -*- Mode: LISP; Syntax: Common-lisp; Package: DTP; Base: 10 -*-

;;;----------------------------------------------------------------------------
;;;
;;;	File		Conjunct.Lisp
;;;	System		Don's Theorem Prover
;;;
;;;	Written by	Don Geddis (Geddis@CS.Stanford.Edu)

(in-package "DTP")

;;;----------------------------------------------------------------------------
;;;
;;;	Public

;;;----------------------------------------------------------------------------

(defmethod expand ((conjunct dtp-conjunct))
  (with-slots (literal parent-conjunction binding-list transform-binding-list
	       answer-count subgoal nogoods ) conjunct
    (when (eq subgoal :uninitialized)
      (if (find *caching* '(:recursion :postponement))
	  (multiple-value-bind (new-subgoal tbl) (find-subgoal conjunct)
	    (setq subgoal new-subgoal)
	    (setq transform-binding-list tbl) )
	(setq subgoal (make-new-subgoal conjunct)) )
      (setq nogoods nil) )
    (with-slots (answers conjuncts-to-propagate-to) subgoal
      (let ((answer (get-next-answer subgoal conjunct)))
        (cond
         (answer
	  (incf (slot-value conjunct 'answer-count))
	  (propagate answer parent-conjunction) )
         ((exhausted-p subgoal)
	  (propagate :not-an-answer parent-conjunction) )
	 (t
	  (pushnew conjunct conjuncts-to-propagate-to)
	  (unless (find subgoal (proof-subgoal-agenda *proof*))
	    (agenda-add subgoal) )
	  (propagate :blocked parent-conjunction) )
	 ))
      )))

#+dtp-trace
(defmethod expand :around ((conjunct dtp-conjunct))
  "For proof tracing"
  (when (find :conjunct-proofs *trace*)
    (indent-line)
    (format *debug-io* "Expanding conjunct ")
    (debug-print-conjunct conjunct)
    (format *debug-io* "~%") )
  (call-next-method) )

;;;----------------------------------------------------------------------------

(defmethod propagate (answer (conjunct dtp-conjunct))
  #+dtp-types (declare (type answer answer))
  (with-slots
      (parent-conjunction transform-binding-list answer-count)
      conjunct

    ;; Optional error checking: should never occur
    (unless (and (>= (slot-value parent-conjunction 'stack-pointer) 0)
		 (eq (nth (slot-value parent-conjunction 'stack-pointer)
			  (slot-value parent-conjunction 'list) )
		     conjunct ))
      #+dtp-trace
      (when (find :conjunct-proofs *trace*)
	(indent-line)
	(format *debug-io* "Error: Answer ~A propagated to conjunct ~A~%"
		answer conjunct )
	(indent-line)
	(format *debug-io* "  but conjunction ~A isn't working on it.~%"
		parent-conjunction ))
      (return-from propagate nil) )
    
    (if (eq answer :not-an-answer)
	(propagate :not-an-answer parent-conjunction)
      (multiple-value-bind (failure ancestors)
	  (invalid-context answer conjunct transform-binding-list)
	(if failure
	    (progn
	      #+dtp-trace (debug-print-context answer conjunct failure) )
	  (progn
	    (when transform-binding-list
	      (setq answer (copy-answer answer))
	      (setf (answer-binding-list answer)
		(plug (answer-binding-list answer) transform-binding-list) )
	      (setf (answer-context answer) ancestors) )
	    (incf answer-count)
	    (propagate answer parent-conjunction) ))
	))
    ))

#+dtp-trace
(defmethod propagate :around (answer (conjunct dtp-conjunct))
  "For proof tracing"
  #+dtp-types (declare (type answer answer))
  (when (find :conjunct-proofs *trace*)
    (indent-line)
    (format *debug-io* "Propagating ~S to conjunct " answer)
    (print-literal-node
     (literal-plug (slot-value conjunct 'literal)
		   (slot-value conjunct 'binding-list) )
     :s *debug-io* )
    (debug-print-subgoal conjunct)
    (format *debug-io* "~%") )
  (call-next-method) )

;;;----------------------------------------------------------------------------

(defmethod reset ((conjunct dtp-conjunct)
		  #+dtp-trace &key #+dtp-trace (in-trace nil) )
  "Backtracking over CONJUNCT, so return to uninitialized state"
  (with-slots (subgoal answer-count nogoods #+dtp-trace used-subgoals) conjunct
    (unless (eq subgoal :uninitialized)
      #+dtp-trace (add-to-end-if-new subgoal used-subgoals)
      (unattach conjunct #+dtp-trace in-trace)
      (setq subgoal :uninitialized) )
    (setq answer-count 0)
    (setq nogoods :uninitialized) ))

(defun unattach (conjunct #+dtp-trace &optional #+dtp-trace (in-trace nil))
  "Remove CONJUNCT from master subgoal propagate list"
  #+dtp-types (declare (type dtp-conjunct conjunct))
  (with-slots (subgoal) conjunct
    (unless (eq subgoal :uninitialized)
      (with-slots (conjuncts-to-propagate-to) subgoal
	(when (find conjunct conjuncts-to-propagate-to)
	  (setf conjuncts-to-propagate-to
	    (remove conjunct conjuncts-to-propagate-to) )
	  #+dtp-trace
	  (when (find :conjunct-proofs *trace*)
	    (when in-trace (format *debug-io* "~%"))
	    (indent-line)
	    (format *debug-io* "Removing conjunct ")
	    (debug-print-conjunct conjunct)
	    (format *debug-io* " from subgoal ")
	    (print-literal-node (slot-value subgoal 'literal) :s *debug-io*)
	    (format *debug-io* " propagate list~%")
	    (when in-trace (indent-line)) )
	  (when (null conjuncts-to-propagate-to)
	    (agenda-remove subgoal) )
	  )))))

;;;----------------------------------------------------------------------------

(defun copy-conjunct (conj)
  "Should be defined by CLOS, but for some reason isn't"
  #+dtp-trace (declare (type dtp-conjunct conj))
  (let (new)
    (setq new (make-instance 'dtp-conjunct))
    (dolist (slot '(literal parent-conjunction binding-list
		    transform-binding-list answer-count subgoal nogoods ))
      (setf (slot-value new slot) (slot-value conj slot)) )
    #+dtp-trace
    (setf (slot-value new 'used-subgoals) nil)
    new ))

;;;----------------------------------------------------------------------------
;;;
;;;	Private

;;;----------------------------------------------------------------------------

(defun get-next-answer (subgoal conjunct)
  "Get the next answer from SUBGOAL valid in CONJUNCT's context"
  #+dtp-types (declare (type dtp-subgoal subgoal))
  #+dtp-types (declare (type dtp-conjunct conjunct))

  (when (original-parent? subgoal conjunct)
    (let (answer)
      (setq answer
	(nth (slot-value conjunct 'answer-count)
	     (slot-value subgoal 'answers) ))
      (return-from get-next-answer answer) ))

  (loop
      with tbl = (slot-value conjunct 'transform-binding-list)
      for answer =
	(nth (slot-value conjunct 'answer-count) (slot-value subgoal 'answers))
      while answer
      do (multiple-value-bind (failure ancestors)
	     (invalid-context answer conjunct tbl)
	   (if failure
	       (progn
		 (incf (slot-value conjunct 'answer-count))
		 #+dtp-trace (debug-print-context answer conjunct failure) )
	     (progn
	       (setq answer (copy-answer answer))
	       (setf (answer-binding-list answer)
		 (plug (answer-binding-list answer) tbl) )
	       (when ancestors
		 (setf (answer-context answer) ancestors) )
	       (return answer) )))
      finally (return answer) ))

;;;----------------------------------------------------------------------------

(defun invalid-context (answer conjunct tbl)
  #+dtp-types (declare (type answer answer))
  #+dtp-types (declare (type dtp-conjunct conjunct))
  #+dtp-types (declare (type binding-list tbl))
  "Returns (1) residue of literals/T if invalid, (2) needed ancestors"
  
  (cond
   
   ;; Not a reduction answer
   ((null (answer-context answer))
    (values nil nil) )
   
   ;; Propagating up original line
   ((original-parent? (answer-subgoal answer) conjunct)
    (values nil nil) )
   
   ;; (Perhaps) don't copy reductions answers across cache link
   ((not *cache-reductions*)
    (values t nil) )
   
   ;; Otherwise, copy answer if new context is sufficient
   (t
    (loop
	with literals =
	  (mapcar #'(lambda (s) (literal-plug (slot-value s 'literal) tbl))
		  (answer-context answer) )
	while literals
	for ancestor in (subgoal-ancestors-of conjunct)
	for match =
	  (find (slot-value ancestor 'literal) literals
		:test #'literal-equal-p )
	when match
	collect ancestor into used-ancestors
	when match
	do (setq literals (remove match literals))
	finally (return (values literals used-ancestors)) ))
   ))

;;;----------------------------------------------------------------------------

(defun original-parent? (subgoal conjunct)
  "True IFF CONJUNCT was the original parent of SUBGOAL"
  #+dtp-types (declare (type (or dtp-subgoal null) subgoal))
  #+dtp-types (declare (type dtp-conjunct conjunct))
  (when subgoal
    (eq (slot-value subgoal 'parent-conjunct) conjunct) ))

;;;----------------------------------------------------------------------------

#+dtp-trace
(defun debug-print-conjunct (conjunct)
  #+dtp-types (declare (type dtp-conjunct conjunct))
  (print-literal-node
   (literal-plug (slot-value conjunct 'literal)
		 (slot-value conjunct 'binding-list) )
   :s *debug-io* ))

#+dtp-trace
(defun debug-print-context (answer conjunct missing)
  #+dtp-types (declare (type answer answer))
  #+dtp-types (declare (type dtp-conjunct conjunct))
  (let ((parent (subgoal-parent-of conjunct)))
    (when (find :conjunct-proofs *trace*)
      (indent-line)
      (format *debug-io* "Answer ~A not valid for " answer)
      (if parent
	  (print-literal-node (slot-value parent 'literal) :s *debug-io*)
	(format *debug-io* "[Query]") )
      (if (listp missing)
	  (progn
	    (format *debug-io* " (Residue:")
	    (dolist (sg missing)
	      (format *debug-io* " ")
	      (when (typep sg 'dtp-subgoal)
		(setq sg (slot-value sg 'literal)) )
	      (print-literal-node sg :s *debug-io*) )
	    (format *debug-io* ")~%") )
	(format *debug-io* " (Reduction)~%") )
      )))

;;;----------------------------------------------------------------------------
