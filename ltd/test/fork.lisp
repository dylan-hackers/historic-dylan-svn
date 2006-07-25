;;; -*- Mode: LISP; Syntax: Common-lisp; Package: DTP; Base: 10 -*-

;;;----------------------------------------------------------------------------
;;;
;;;	File		Fork.Lisp
;;;	System		Don's Theorem Prover
;;;
;;;	Written by	Don Geddis (Geddis@CS.Stanford.Edu)

(in-package "DTP")

;;;----------------------------------------------------------------------------
;;;
;;;	Public

;;;----------------------------------------------------------------------------

(defun update-blocking (conjunction answer)
  #+dtp-types (declare (type dtp-conjunction conjunction))
  #+dtp-types (declare (type (or answer null) answer))
  (with-slots (parent-subgoal) conjunction
    (if (eq answer :blocked)

	;; Block the conjunction
	(if parent-subgoal
	    (with-slots (inferences blocked-conjunctions) parent-subgoal
	      (setf inferences (remove conjunction inferences))
	      (add-to-end-if-new conjunction blocked-conjunctions) )
	  (progn
	    (setf (proof-query-conjunctions *proof*)
	      (remove conjunction (proof-query-conjunctions *proof*)) )
	    (add-to-end-if-new
	     conjunction (proof-blocked-conjunctions *proof*)) ))
      
      ;; Unblock the conjunction
      (if parent-subgoal
	  (with-slots (inferences blocked-conjunctions) parent-subgoal
	    (unless (find conjunction inferences)
	      (push conjunction inferences)
	      (setq blocked-conjunctions
		(remove conjunction blocked-conjunctions)) ))
	(progn
	  (unless (find conjunction (proof-query-conjunctions *proof*))
	    (push conjunction (proof-query-conjunctions *proof*))
	    (setf (proof-blocked-conjunctions *proof*)
	      (remove conjunction (proof-blocked-conjunctions *proof*)) )))))
    ))

;;;----------------------------------------------------------------------------

(defun fork-conjunction (conj)
  #+dtp-types (declare (type dtp-conjunction conj))
  (let (forked)
    #+dtp-trace (trace-fork conj)
    (setq forked (copy-conjunction conj))
    (fork-specialize! forked conj)
    
    ;; Add it as a new conjunction
    (let ((parent (slot-value forked 'parent-subgoal)))
      (if parent
	  (add-to-end forked (slot-value parent 'blocked-conjunctions))
	(add-to-end forked (proof-blocked-conjunctions *proof*)) ))
    ))

;;;----------------------------------------------------------------------------

(defun unblock-agenda ()
  "All remaining subgoals blocked, so proof effort is in cycle"
  #+dtp-trace
  (when (find :proofs *trace*)
    (format *debug-io* "Blocked cycle, so unblocking a subgoal~%") )
  (let (blocked-conj)
    (setq blocked-conj (best-blocked-conjunction))
    (fork-conjunction blocked-conj)
    (setf (slot-value blocked-conj 'backtrack-pointer)
      (slot-value blocked-conj 'stack-pointer) )
    (propagate :not-an-answer (active-conjunct blocked-conj)) ))

;;;----------------------------------------------------------------------------

(defun forkable-conjunctions? ()
  "True IFF the subgoal agenda or query has a forkable conjunction"
  (or (find-if #'forkable? (proof-blocked-conjunctions *proof*))
      (find-if
       #'(lambda (sg)
	   (find-if #'forkable? (slot-value sg 'blocked-conjunctions)) )
       (proof-subgoal-agenda *proof*) )))

;;;----------------------------------------------------------------------------
;;;
;;;	Private

;;;----------------------------------------------------------------------------

(defun best-blocked-conjunction ()
  "Return a blocked conjunction whose unblocking might make progress"
  (let (conjs)
    (setq conjs
      (reduce
       #'append
       (mapcar #'(lambda (sg) (slot-value sg 'blocked-conjunctions))
	       (reverse (proof-subgoal-agenda *proof*)) )))
    (setq conjs (append (proof-blocked-conjunctions *proof*) conjs))
    (find-if #'forkable? conjs) ))

;;;----------------------------------------------------------------------------

(defun forkable? (conjunction)
  "True IFF the conjunction could make progress by forking
   I.e.: If the current blocked conjunction got an answer, and then got no
   more answers (and so backtracked) after that, would there still be more
   in the conjunction space to search?"
  #+dtp-types (declare (type dtp-conjunction conjunction))
  (if (typep conjunction 'dtp-forked-conjunction)
      (> (slot-value conjunction 'stack-pointer)
	 (slot-value conjunction 'top-conjunct) )
    (> (slot-value conjunction 'stack-pointer) 0) ))

;;;----------------------------------------------------------------------------

#+dtp-trace
(defun trace-fork (conj)
  #+dtp-types (declare (type dtp-conjunction conj))
  (when (find :proofs *trace*)
    (indent-line)
    (format *debug-io* "Forking conjunction ")
    (debug-print conj)
    (when (slot-value conj 'stack)
      (format *debug-io* " starting with")
      (dolist (ans (slot-value conj 'stack))
	(format *debug-io* " ~A" ans) ))
    (format *debug-io* "~%") ))

;;;----------------------------------------------------------------------------

(defun copy-conjunction (conj)
  "Return a copy of the conjunction (with some slot values copied)"
  #+dtp-types (declare (type dtp-conjunction conj))
  (let (new)
    (setq new (make-instance 'dtp-forked-conjunction))
    (setf (slot-value new 'list)
      (mapcar #'copy-conjunct (slot-value conj 'list)) )
    (setf (slot-value new 'stack)
      (copy-list (slot-value conj 'stack)) )
    (dolist (slot '(stack-pointer backtrack-pointer parent-subgoal
		    binding-list label residue ae-binding-list
		    #+dtp-trace origin ))
      (setf (slot-value new slot) (slot-value conj slot)) )
    new ))

;;;----------------------------------------------------------------------------

(defun fork-specialize! (instance original)
  #+dtp-types (declare (type dtp-conjunction instance original))

  (dolist (conjunct (slot-value instance 'list))
    (setf (slot-value conjunct 'parent-conjunction) instance) )
  (setf (slot-value instance 'top-conjunct)
    (slot-value original 'stack-pointer) )
  (let (ac subgoal)
    (setq ac (active-conjunct instance))
    (setq subgoal (slot-value ac 'subgoal))
    (pushnew ac (slot-value subgoal 'conjuncts-to-propagate-to)) )

  #+dtp-trace
  (let ((parent (slot-value original 'parent-subgoal)))
    (if parent
	(setf (slot-value instance 'order)
	  (incf (slot-value parent 'conjunction-count)) )
	(setf (slot-value instance 'order)
	  (incf (proof-conjunction-count *proof*)) ))
    (loop
	for conjunct in (slot-value instance 'list)
	for count from 0 to (slot-value instance 'top-conjunct)
	do (setf (slot-value conjunct 'used-subgoals) nil) )
    (setf (slot-value instance 'answers) nil)
    (setf (slot-value instance 'parent-conjunction) original) ))

;;;----------------------------------------------------------------------------
