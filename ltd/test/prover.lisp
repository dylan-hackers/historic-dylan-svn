;;; -*- Mode: LISP; Syntax: Common-lisp; Package: DTP; Base: 10 -*-

;;;----------------------------------------------------------------------------
;;;
;;;	File		Prover.Lisp
;;;	System		Don's Theorem Prover
;;;
;;;	Written by	Don Geddis (Geddis@CS.Stanford.Edu)

(in-package "DTP")

(eval-when (compile load eval)
  (export
   '(prove prove-next-answer prove-all-remaining-answers) ))

;;;----------------------------------------------------------------------------
;;;
;;;	Public

;;;----------------------------------------------------------------------------

(defun prove
    (query
     &key
     (all-answers nil) (return-form nil) (suppress-disjunctive-answers nil) )
  "Returns (1) bound query(ies), (2) label(s), (3) <answer(s)>, (4) <proof>"
  (let (*proof*)
    (setq *proof*
      (make-proof
       :query query
       :return-form return-form
       :sda suppress-disjunctive-answers ))
    (reset *proof*)
    (setq *last-proof* *proof*)
    (when *timeout-maximum-seconds*
      (setq *timeout-count* 0)
      (setq *timeout-end*
	(+ (get-internal-run-time)
	   (* internal-time-units-per-second
	      *timeout-maximum-seconds* ))))
    (cond
     (all-answers
      (prove-all-remaining-answers *proof*) )
     (t
      (prove-next-answer *proof*) ))
    ))

;;;----------------------------------------------------------------------------

(defun prove-next-answer (&optional (*proof* *proof*))
  "Returns (1) bound query, (2) label, (3) <answer> or :NOT-AN-ANSWER, and (4) <proof>"
  #-dtp-trace (explode-answer (prove-next-answer-with-iteration *proof*))
  #+dtp-trace
  (let ((*proof-line-count* 0)
	(*depth* 0)
	(*subgoal-map* nil) )
    (explode-answer (prove-next-answer-with-iteration *proof*)) ))

;;;----------------------------------------------------------------------------

(defun prove-all-remaining-answers (&optional (*proof* *proof*))
  "Returns (1) queries, (2) labels, (3) residues, (4) <answer>s, (5) <proof>"
  (let (#+dtp-trace (*proof-line-count* 0)
	#+dtp-trace (*depth* 0)
	#+dtp-trace (*subgoal-map* nil)
	answers bounds label-values residues )
    (setq answers (prove-all-remaining-answers-internal *proof*))
    (setq bounds (mapcar #'apply-answer answers))
    (setq label-values (mapcar #'extract-label answers))
    (setq residues (mapcar #'extract-residue answers))
    (values bounds label-values residues answers *proof*) ))

;;;----------------------------------------------------------------------------

(defmethod propagate (answer (proof proof))
  #+dtp-types (declare (type answer answer))
  (if (or (find answer (proof-answers proof) :test #'answer-equal-p)
	  (find answer (proof-new-answers proof) :test #'answer-equal-p) )
      (progn
	#+dtp-trace
	(when (find :solutions *trace*)
	  (format *debug-io* "Answer ~A ignored because not new~%" answer) ))
    (add-to-end answer (proof-new-answers proof)) ))

;;;----------------------------------------------------------------------------

(defun apply-answer (answer &optional (form (proof-return-form *proof*)))
  "Turn an answer structure into a user-readable form"
  #+dtp-types (declare (type answer answer))
  (unless form (setq form (proof-query *proof*)))
  (if (answer-ae-binding-lists answer)
      (cons 'or
	    (cons (plug form (answer-binding-list answer))
		  (mapcar #'(lambda (bl) (plug form bl))
			  (answer-ae-binding-lists answer) )))
    (plug form (answer-binding-list answer)) ))

;;;----------------------------------------------------------------------------

(defun active-agenda (&optional (proof *proof*))
  (find-if #'active-p (proof-subgoal-agenda proof)) )

;;;----------------------------------------------------------------------------

(defun agenda-add (subgoal)
  "Add SUBGOAL to the proof agenda"
  (if (find subgoal (proof-subgoal-agenda *proof*))
      (progn
	#+dtp-trace
	(when (find :proofs *trace*)
	  (indent-line)
	  (format *debug-io* "Subgoal ")
	  (print-literal-node (slot-value subgoal 'literal) :s *debug-io*)
	  (format *debug-io* " found on the proof agenda at position ")
	  (format *debug-io* "~D~%"
		  (position subgoal (proof-subgoal-agenda *proof*)) )))
    (progn
      #+dtp-trace
      (when (find :proofs *trace*)
	(indent-line)
	(format *debug-io* "Pushing ")
	(print-literal-node (slot-value subgoal 'literal) :s *debug-io*)
	(format *debug-io* " on the proof agenda~%") )
      (push subgoal (proof-subgoal-agenda *proof*)) )
    ))

;;;----------------------------------------------------------------------------

(defun agenda-remove (subgoal)
  "Remove SUBGOAL from the proof agenda"
  #+dtp-trace
  (when (find :proofs *trace*)
    (indent-line)
    (format *debug-io* "Removing ")
    (print-literal-node (slot-value subgoal 'literal) :s *debug-io*)
    (format *debug-io* " from the proof agenda")
    (cond
     ((eq subgoal (first (proof-subgoal-agenda *proof*)))
      (format *debug-io* " (first item)~%") )
     ((not (find subgoal (proof-subgoal-agenda *proof*)))
      (format *debug-io* " (but it isn't on the agenda!)~%") )
     (t
      (format *debug-io* "~%") )))
  (if (eq subgoal (first (proof-subgoal-agenda *proof*)))
      (pop (proof-subgoal-agenda *proof*))
    (setf (proof-subgoal-agenda *proof*)
      (remove subgoal (proof-subgoal-agenda *proof*)) )))

;;;----------------------------------------------------------------------------
;;;
;;;	Private

;;;----------------------------------------------------------------------------

(defvar *last-sc*)
(defvar *last-fc*)

(defun prove-next-answer-with-iteration (&optional (*proof* *proof*))
  "Iterative deepening on subgoal and function depth"
  (setq *last-sc* -1)
  (setq *last-fc* -1)
  (loop
      with next-answer
      with s-depth =
	(or (and *use-subgoal-cutoffs*
		 (or (proof-subgoal-depth-cutoff *proof*)
		     (proof-subgoal-maximum-depth *proof*)
		     *initial-subgoal-depth*
		     *subgoal-maximum-depth*
		     0 ))
	    0 )
      with f-depth =
	(or (and *use-function-cutoffs*
		 (or (proof-function-depth-cutoff *proof*)
		     (proof-function-maximum-depth *proof*)
		     *initial-function-depth*
		     *function-maximum-depth*
		     0 ))
	    0 )
      while t			; Termination conditions in the body
      do
	;; Enforce maximums
	(when (and (proof-subgoal-maximum-depth *proof*)
		   (> s-depth (proof-subgoal-maximum-depth *proof*)) )
	  (setq s-depth (proof-subgoal-maximum-depth *proof*)) )
	(when (and (proof-function-maximum-depth *proof*)
		   (> f-depth (proof-function-maximum-depth *proof*)) )
	  (setq f-depth (proof-function-maximum-depth *proof*)) )

	#+dtp-trace (report-iteration s-depth f-depth)

	;; Caching
	(when (eq *caching* :answers)
	  (flush-answer-failure-cache) )

	;; Do a bounded search
	(setf (proof-subgoal-depth-cutoff *proof*) s-depth)
	(setf (proof-function-depth-cutoff *proof*) f-depth)
	(setq next-answer (prove-next-answer-internal))
	(unless (eq next-answer :not-an-answer)
	  (return next-answer) )
	
	;; Loop exit conditions
	(if (proof-subgoal-cutoff-occurred *proof*)
	    (if (proof-function-cutoff-occurred *proof*)
		(unless (or (proof-subgoal-depth-skip *proof*)
			    (proof-function-depth-skip *proof*) )
		  (return :not-an-answer) )
	      (unless (proof-subgoal-depth-skip *proof*)
		(return :not-an-answer) ))
	  (if (proof-function-cutoff-occurred *proof*)
	      (unless (proof-function-depth-skip *proof*)
		(return :not-an-answer) )
	    (return :not-an-answer) ))

	;; Iterate to next depth
	(when (and *use-subgoal-cutoffs* (proof-subgoal-depth-skip *proof*))
	  (setq s-depth (+ s-depth (proof-subgoal-depth-skip *proof*))) )
	(when (and *use-function-cutoffs* (proof-function-depth-skip *proof*))
	  (setq f-depth (+ f-depth (proof-function-depth-skip *proof*))) )
	(if (within-maxbounds-p s-depth f-depth)
	    (reset *proof*)
	  (return :not-an-answer) )
	))

;;;----------------------------------------------------------------------------

(defun within-maxbounds-p (subgoal-cutoff function-cutoff)
  #+dtp-trace (declare (type integer subgoal-cutoff function-cutoff))
  (if (and (= *last-sc* subgoal-cutoff)
	   (= *last-fc* function-cutoff) )
      (return-from within-maxbounds-p nil)
    (progn
      (setq *last-sc* subgoal-cutoff)
      (setq *last-fc* function-cutoff) ))
  (or
   (and (proof-subgoal-maximum-depth *proof*)
	(<= subgoal-cutoff (proof-subgoal-maximum-depth *proof*)) )
   (and (proof-function-maximum-depth *proof*)
	(<= function-cutoff (proof-function-maximum-depth *proof*)) )
   (and (null (proof-subgoal-maximum-depth *proof*))
	(null (proof-function-maximum-depth *proof*)) )))

#+dtp-trace
(defun report-iteration (subgoal-bound function-bound)
  (when (find :iteration *trace*)
    (when (or *use-subgoal-cutoffs* *use-function-cutoffs*)
      (format *debug-io* "~2&") )
    (when *use-subgoal-cutoffs*
      (format *debug-io* "Subgoal depth cutoff = ~D~%" subgoal-bound) )
    (when *use-function-cutoffs*
      (format *debug-io* "Function depth cutoff = ~D~%" function-bound) )))

;;;----------------------------------------------------------------------------

(defmethod reset ((proof proof) #+dtp-trace &key #+dtp-trace &allow-other-keys)
  (setf (proof-query-conjunctions proof)
    (mapcar #'list-to-conjunction (dnf (proof-query proof))) )
  #+dtp-trace
  (loop
      for conj in (proof-query-conjunctions proof)
      for count from 0
      do (setf (slot-value conj 'order) count)
      finally (setf (proof-conjunction-count proof) count) )
  (when *use-negated-goal*
    (let ((*goal-node-id-count* 0))
      (setf (proof-goal-nodes proof)
	(mapcar #'make-goal-node (proof-query-conjunctions proof)) )))
  (setf (proof-blocked-conjunctions proof) nil)
  (setf (proof-subgoal-index proof) (make-hash-table :test #'eq))
  (setf (proof-subgoal-cutoff-occurred proof) nil)
  (setf (proof-function-cutoff-occurred proof) nil)
  #+dtp-trace (setf (proof-used-conjunctions proof) nil) )

;;;----------------------------------------------------------------------------

(defun prove-next-answer-internal ()
  "Return <answer> or :NOT-AN-ANSWER"
  #+dtp-trace
  (when (find :solutions *trace*)
    (format *debug-io* "Looking for next answer of ~A~%"
	    (proof-query *proof*) ))
  (loop
      for next-answer = (pop (proof-new-answers *proof*))
      for next-subgoal = (agenda-best)
      for next-conjunction = (first (proof-query-conjunctions *proof*))
      while (or next-answer next-subgoal next-conjunction
		(and *use-unblocking* (forkable-conjunctions?)) )
      do
	(when *timeout-maximum-seconds*
	  (incf *timeout-count*)
	  (when (> *timeout-count* *timeout-resolution*)
	    (setq *timeout-count* 0)
	    (let ((rt (get-internal-run-time)))
	      (when (> rt *timeout-end*)
		(return :not-an-answer) )
	      #+dtp-trace
	      (when (find :timeout *trace*)
		(let (secs)
		  (setq secs
		    (floor (- *timeout-end* rt)
			   internal-time-units-per-second ))
		  (if (find :proofs *trace*)
		      (format *debug-io* "~D seconds until timeout~%" secs)
		    (format *debug-io* "[~D]..." secs) )))
	      )))
	(cond
	 (next-answer
	  (let ((valid (process-answer next-answer)))
	    (when valid (return valid)) ))
	 (next-subgoal
	  (process-subgoal next-subgoal) )
	 ((and *use-unblocking* (forkable-conjunctions?))
	  (unblock-agenda) )
	 (t
	  (process-conjunction next-conjunction) ))
      finally (return :not-an-answer) ))

;;;----------------------------------------------------------------------------

(defun process-answer (answer)
  #+dtp-types (declare (type answer answer))
  #+dtp-trace
  (when (and (proof-sda *proof*)
	     (disjunctive-p answer)
	     (find :solutions *trace*) )
    (format *debug-io* "Suppressing disjunctive answer ~A~%" answer) )
  (unless (and (proof-sda *proof*) (disjunctive-p answer))
    (add-to-end answer (proof-answers *proof*))
    answer ))

;;;----------------------------------------------------------------------------

(defun process-subgoal (subgoal)
  #+dtp-types (declare (type dtp-subgoal subgoal))
  (cond
   ((null (slot-value subgoal 'conjuncts-to-propagate-to))
    #+dtp-trace
    (when (find :proofs *trace*)
      (format *debug-io* "Subgoal ")
      (print-literal-node (slot-value subgoal 'literal) :s *debug-io*)
      (format *debug-io* " is irrelevant~%") )
    (agenda-remove subgoal) )
   ((exhausted-p subgoal :ignore-blocked t)
    #+dtp-trace
    (when (find :proofs *trace*)
      (format *debug-io* "Subgoal ")
      (print-literal-node (slot-value subgoal 'literal) :s *debug-io*)
      (format *debug-io* " is exhausted~%") )
    (when (exhausted-p subgoal)
      #+dtp-trace (incf *depth*)
      (terminate subgoal)
      #+dtp-trace (decf *depth*) )
    (agenda-remove subgoal) )
   (t
    (expand subgoal) )))

;;;----------------------------------------------------------------------------

(defun process-conjunction (conjunction)
  #+dtp-types (declare (type dtp-conjunction conjunction))
  (if (exhausted-p conjunction)
      (progn
	#-dtp-trace (pop (proof-query-conjunctions *proof*))
	#+dtp-trace
	(add-to-end
	 (pop (proof-query-conjunctions *proof*))
	 (proof-used-conjunctions *proof*) ))
    (expand conjunction) ))

;;;----------------------------------------------------------------------------

(defun prove-all-remaining-answers-internal (&optional (*proof* *proof*))
  "Return list of all remaining <answers>"
  (loop
      for next-answer = (prove-next-answer-with-iteration *proof*)
      until (eq next-answer :not-an-answer)
      collect next-answer into answers
      #+dtp-trace do
	#+dtp-trace
	(when (find :solutions *trace*)
	  (format *debug-io* "Found answer ~A~2%" next-answer) )
      finally (return answers) ))

;;;----------------------------------------------------------------------------

(defun explode-answer (answer)
  "Returns (1) bound query, (2) label, (3) residue, (4) ANSWER, (5) *PROOF*"
  #+dtp-types (declare (type answer answer))
  (if (answer-p answer)
      (values (apply-answer answer) (extract-label answer)
	      (extract-residue answer) answer *proof* )
    (values nil nil nil :not-an-answer *proof*) ))

;;;----------------------------------------------------------------------------

(defun extract-label (answer)
  #+dtp-types (declare (type answer answer))
  (let ((label (answer-label answer)))
    (when label (label-value label)) ))

;;;----------------------------------------------------------------------------

(defun extract-residue (answer)
  #+dtp-types (declare (type answer answer))
  (mapcar #'literal-to-list (answer-residue answer)) )

;;;----------------------------------------------------------------------------

(defun make-goal-node (conjunction)
  "Return clause corresponding to negated conjunction"
  #+dtp-types (declare (type dtp-conjunction conjunction))
  (with-slots (list) conjunction
    (loop
	for conjunct in list
	for literal = (slot-value conjunct 'literal)
	for new-literal = (copy-literal-node literal)
	do (setf (literal-negated-p new-literal)
	     (not (literal-negated-p new-literal)) )
	collect new-literal into literals
	finally
	  (return
	    (make-kb-node
	     :id (make-new-id "GOAL" (incf *goal-node-id-count*))
	     :clause (make-clause-node :literals literals)) ))
    ))

;;;----------------------------------------------------------------------------
