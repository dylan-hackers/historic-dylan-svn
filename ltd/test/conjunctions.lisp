;;; -*- Mode: LISP; Syntax: Common-lisp; Package: DTP; Base: 10 -*-

;;;----------------------------------------------------------------------------
;;;
;;;	File		Conjunctions.Lisp
;;;	System		Don's Theorem Prover
;;;
;;;	Written by	Don Geddis (Geddis@CS.Stanford.Edu)

(in-package "DTP")

;;;----------------------------------------------------------------------------
;;;
;;;	Public

;;;----------------------------------------------------------------------------

(defmethod expand ((conjunction dtp-conjunction))
  (with-slots
      (list stack stack-pointer parent-subgoal binding-list label answers)
      conjunction
    (unless (< stack-pointer 0)
      (let ((conjunct (nth stack-pointer list)))
	(if conjunct
	    (expand conjunct)
	  (let (answer)
	    (setq answer (merge-conjunction-answers conjunction))
	    (unless (eq answer :not-an-answer)
	      #+dtp-trace
	      (let ((propagate-answer (copy-answer answer)))
		(add-to-end answer answers)
		(setf (answer-justification propagate-answer)
		  (make-c-justification
		   :conjunction conjunction
		   :answer answer ))
		(setq answer propagate-answer) )
	      (note-solution conjunction)
	      (if parent-subgoal
		  (propagate answer parent-subgoal)
		(propagate answer *proof*) ))
	    (pop stack)
	    (decf stack-pointer) ))
	))))

#+dtp-trace
(defmethod expand :around ((conjunction dtp-conjunction))
  "For proof tracing"
  (when (find :proofs *trace*)
    (indent-line)
    (format *debug-io* "Expanding conjunction ")
    (debug-print conjunction)
    (format *debug-io* "~%") )
  (call-next-method) )

;;;----------------------------------------------------------------------------

(defmethod propagate (answer (conjunction dtp-conjunction))
  (with-slots (list stack stack-pointer) conjunction
    (update-blocking conjunction answer)
    (case answer
      (:blocked)
      (:not-an-answer
       (backtrack conjunction) )
      (otherwise
       (let ((this-conjunct (nth stack-pointer list))
	     (next-conjunct (nth (incf stack-pointer) list))
	     mgu )
	 (unattach this-conjunct)
	 (push answer stack)
	 (when next-conjunct
	   (setq mgu (merge-conjunction-binding-lists conjunction))
	   (if (eq mgu :not-a-binding-list)
	       (progn
		 (pop stack)
		 (decf stack-pointer)
		 (return-from propagate) )
	     (setf (slot-value next-conjunct 'binding-list) mgu) ))
	 (expand conjunction) ))
      )))

#+dtp-trace
(defmethod propagate :around (answer (conjunction dtp-conjunction))
  "For proof tracing"
  #+dtp-types (declare (type answer answer))
  (when (find :answers *trace*)
    (indent-line)
    (format *debug-io* "Propagating ~S to conjunction " answer)
    (debug-print conjunction)
    (debug-print-subgoal conjunction)
    (format *debug-io* "~%") )
  (call-next-method) )

;;;----------------------------------------------------------------------------

(defmethod exhausted-p ((conjunction dtp-conjunction) &key &allow-other-keys)
  (< (slot-value conjunction 'stack-pointer) 0) )

(defmethod virgin-p ((conjunction dtp-conjunction))
  "True IFF CONJUNCTION has never been worked on"
  (with-slots (list stack-pointer) conjunction
    (or (null list)
	(and list
	     (= stack-pointer 0)
	     (eq (slot-value (first list) 'subgoal) :uninitialized)
	     ))))

(defun active-conjunct (conjunction)
  #+dtp-types (declare (type dtp-conjunction conjunction))
  (with-slots (list stack-pointer) conjunction
    (when (>= stack-pointer 0)
      (nth stack-pointer list) )))

;;;----------------------------------------------------------------------------
;;;
;;;	Private

;;;----------------------------------------------------------------------------

(defun merge-conjunction-answers (conjunction)
  "Returns merge of answers (plus conj bl), or :NOT-AN-ANSWER"
  #+dtp-types (declare (type dtp-conjunction conjunction))
  (let ((answer (merge-answers (slot-value conjunction 'stack))))
    (when (eq answer :not-an-answer)
      (return-from merge-conjunction-answers :not-an-answer) )
    (when (slot-value conjunction 'binding-list)
      (setq answer
	(nanswer-merge-binding-list
	 answer (slot-value conjunction 'binding-list) ))
      (when (eq answer :not-an-answer)
	(return-from merge-conjunction-answers :not-an-answer) ))
    (when (slot-value conjunction 'label)
      (setq answer
	(nanswer-merge-label answer (slot-value conjunction 'label)) ))
    (when (slot-value conjunction 'residue)
      (setq answer
	(nanswer-merge-residue answer (slot-value conjunction 'residue)) )
      (when (eq answer :not-an-answer)
	(return-from merge-conjunction-answers :not-an-answer) ))
    (when (slot-value conjunction 'ae-binding-list)
      (push (slot-value conjunction 'ae-binding-list)
	    (answer-ae-binding-lists answer) ))
    #+dtp-trace
    (record-justification conjunction answer)
    #+dtp-trace
    (let ((origin (slot-value conjunction 'origin)))
      (unless (eq origin 'query)
	(push (make-l-justification :id origin)
	      (answer-justification answer) )))
    answer ))

;;;----------------------------------------------------------------------------

(defun merge-conjunction-binding-lists (conjunction)
  #+dtp-types (declare (type dtp-conjunction conjunction))
  (let ((bls (mapcar #'answer-binding-list (slot-value conjunction 'stack))))
    (setq bls (cons (slot-value conjunction 'binding-list) bls))
    (setq bls (remove nil bls))
    (if (cdr bls)
	(merge-binding-lists bls)
      (first bls) )))

;;;----------------------------------------------------------------------------

(defun merge-conjunction-labels (conjunction)
  #+dtp-types (declare (type dtp-conjunction conjunction))
  (let ((labels (mapcar #'answer-label (slot-value conjunction 'stack))))
    (setq labels (cons (slot-value conjunction 'label) labels))
    (setq labels (remove nil labels))
    (if (cdr labels)
	(reduce #'label-and labels)
      (first labels) )))

;;;----------------------------------------------------------------------------

(defun merge-conjunction-residues (conjunction)
  #+dtp-types (declare (type dtp-conjunction conjunction))
  (let ((residues (mapcar #'answer-residue (slot-value conjunction 'stack))))
    (setq residues (cons (slot-value conjunction 'residue) residues))
    (setq residues (remove nil residues))
    (if (cdr residues)
	(let ((merge-res (reduce #'residue-merge residues)))
	  (if (eq merge-res :not-a-residue) :not-a-residue merge-res) )
      (first residues) )))

;;;----------------------------------------------------------------------------

(defun list-to-conjunction (list)
  #+dtp-types (declare (type list list))
  (loop
      for sublist in list
      collect (make-instance 'dtp-conjunct :literal (list-to-literal sublist))
      into conjuncts
      finally
	(let (conjunction)
	  (setq conjunction
	    (make-instance 'dtp-conjunction :list conjuncts
			   #+dtp-trace :origin #+dtp-trace 'query) )
	  (dolist (conjunct conjuncts)
	    (setf (slot-value conjunct 'parent-conjunction) conjunction) )
	  (return conjunction) )))

;;;----------------------------------------------------------------------------

(defun debug-print (conjunction)
  "Concise summary of object"
  #+dtp-types (declare (type dtp-conjunction conjunction))
  (loop
      for remaining on (slot-value conjunction 'list)
      for literal = (slot-value (first remaining) 'literal)
      for in-middle = nil then t
      do (when in-middle (format *debug-io* "^"))
	 (print-literal-node literal :s *debug-io*) ))

;;;----------------------------------------------------------------------------

(defun note-solution (conjunction)
  "Solution found, so must backtrack instead of backjump"
  #+dtp-types (declare (type dtp-conjunction conjunction))
  (with-slots (list backtrack-pointer) conjunction
    (setq backtrack-pointer (1- (length list))) ))

;;;----------------------------------------------------------------------------

#+dtp-trace
(defun record-justification (conjunction answer)
  #+dtp-type (declare (type dtp-conjunction conjunction))
  #+dtp-type (declare (type answer answer))
  (loop
      for conjunct in (slot-value conjunction 'list)
      for c-answer in (reverse (slot-value conjunction 'stack))
      for conj-num from 0
      collect
	(with-slots
	    (transform-binding-list answer-count subgoal)
	    conjunct
	  (let (ans)
	    (if (typep subgoal 'dtp-subgoal)
		(setq ans
		  (nth (1- answer-count) (slot-value subgoal 'answers)) )
	      (setq ans c-answer) )
	    (make-s-justification
	     :conjunct-number conj-num :subgoal subgoal
	     :answer ans :t-bl transform-binding-list )))
      into justs
      finally (setf (answer-justification answer) justs) ))

;;;----------------------------------------------------------------------------
