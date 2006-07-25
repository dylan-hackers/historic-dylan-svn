;;; -*- Mode: LISP; Syntax: Common-lisp; Package: DTP; Base: 10 -*-

;;;----------------------------------------------------------------------------
;;;
;;;	File		Ordering.Lisp
;;;	System		Don's Theorem Prover
;;;
;;;	Written by	Don Geddis (Geddis@CS.Stanford.Edu)

(in-package "DTP")

;;;----------------------------------------------------------------------------
;;;
;;;	Public

;;;----------------------------------------------------------------------------

(defun norder-conjunctions (conjunctions)
  "Return all lookup answers first, then others"
  (if *use-reordering*
      (partition conjunctions #'(lambda (c) (null (slot-value c 'list))))
    conjunctions ))

;;;----------------------------------------------------------------------------

(defun agenda-best ()
  "Return the first element on the agenda that doesn't violate a cutoff"
  #+dtp-trace
  (when *single-step*
    (return-from agenda-best (user-choice (proof-subgoal-agenda *proof*))) )
  (loop
      for best-subgoal =
	(case *search-order*
	  (:dfs	(find-if-not #'blocked-p (proof-subgoal-agenda *proof*)))
	  (:bfs	(find-if-not
		 #'blocked-p (proof-subgoal-agenda *proof*) :from-end t ))
	  (otherwise nil) )
      until (null best-subgoal)
      do
	(cond

	 ((not (good-subgoal-depth best-subgoal))
	  (setf (proof-subgoal-cutoff-occurred *proof*) t)
	  #+dtp-trace
	  (when (find :cutoffs *trace*)
	    (print-literal-node
	     (slot-value best-subgoal 'literal) :s *debug-io* )
	    (format *debug-io* " cutoff because depth > ~D (subgoal max)~%"
		    (proof-subgoal-depth-cutoff *proof*) ))
	  (agenda-remove best-subgoal) )

	 ((not (good-function-depth best-subgoal))
	  (setf (proof-function-cutoff-occurred *proof*) t)
	  #+dtp-trace
	  (when (find :cutoffs *trace*)
	    (print-literal-node
	     (slot-value best-subgoal 'literal) :s *debug-io* )
	    (format *debug-io* " cutoff because depth > ~D (function max)~%"
		    (proof-function-depth-cutoff *proof*) ))
	  (agenda-remove best-subgoal) )

	 (t
	  (return best-subgoal) ))

      finally (return nil) ))

;;;----------------------------------------------------------------------------
;;;
;;;	Private

;;;----------------------------------------------------------------------------

(defun good-subgoal-depth (subgoal)
  "T unless SUBGOAL is past valid depth limit"
  #+dtp-types (declare (type dtp-subgoal subgoal))
  (unless (and *use-subgoal-cutoffs* (proof-subgoal-depth-cutoff *proof*))
    (return-from good-subgoal-depth t) )
  (< (slot-value subgoal 'depth) (proof-subgoal-depth-cutoff *proof*)) )

;;;----------------------------------------------------------------------------

(defun good-function-depth (subgoal)
  "T unless SUBGOAL is past valid function limit"
  #+dtp-types (declare (type dtp-subgoal subgoal))
  (unless (and *use-function-cutoffs* (proof-function-depth-cutoff *proof*))
    (return-from good-function-depth t) )
  (< (function-depth (slot-value subgoal 'literal))
     (proof-function-depth-cutoff *proof*) ))

;;;----------------------------------------------------------------------------

(defun user-choice (agenda)
  "Return some element from the agenda, selected by the user"
  #+dtp-types (declare (type list agenda))
  (format t "~%")
  (cond
   ((null agenda)
    (format t "~&[Empty agenda, so no single step possible]~%")
    nil )
   ((null (cdr agenda))
    (format t "~&[Solo agenda item, so choosing ~A]~%" (first agenda))
    (first agenda) )
   (t
    (loop
	for count from 0 to 9
	for subgoal in agenda
	do (format t "~D: ~A~%" count subgoal)
	finally (when (> (length agenda) 10)
		  (format t "A: Show entire agenda~%") ))
    (loop
	with choice = nil
	until choice
	do (format t "Choice [0]? ")
	   (setq choice (read-line))
	   (if (= (length choice) 0)
	       (setq choice 0)
	     (setq choice (read-from-string choice)) )
	   (cond
	    ((not (integerp choice))
	     (format t "~A~%" agenda)
	     (setq choice nil) )
	    ((and (>= choice 0) (< choice (length agenda)))
	     (setq choice (nth choice agenda)) ))
	finally (return-from user-choice choice) ))
   ))

;;;----------------------------------------------------------------------------
