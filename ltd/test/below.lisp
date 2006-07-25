;;; -*- Mode: LISP; Syntax: Common-lisp; Package: DTP; Base: 10 -*-

;;;----------------------------------------------------------------------------
;;;
;;;	File		Below.Lisp
;;;	System		Don's Theorem Prover
;;;
;;;	Written by	Don Geddis (Geddis@CS.Stanford.Edu)
;;;
;;;	Note		This entire file is conditional on #+dtp-trace

(in-package "DTP")

;;;----------------------------------------------------------------------------
;;;
;;;	Show-Below

(defgeneric show-below (object &key answer &allow-other-keys))

;;;----------------------------------------------------------------------------

(defmethod show-below ((object proof) &key &allow-other-keys)
  (dolist (child (objects-below object))
    (output
     child
     :parent-id (when (display-query-p object) (dot-id-of object)) )))

;;;----------------------------------------------------------------------------

(defmethod show-below ((object answer) &key &allow-other-keys)
  (output (c-just-conjunction (answer-justification object))
	  :parent-id (when (display-query-p (answer-proof object))
		       (dot-id-of object) )
	  :answer (c-just-answer (answer-justification object)) ))

;;;----------------------------------------------------------------------------

(defmethod show-below
    ((object dtp-subgoal)
     &key (answer nil) (parent-id nil) (my-id nil) (record nil)
     (previous-sg-hidden nil) &allow-other-keys )
  (unless *graphic-display* (incf *depth*))
  (unless my-id (setq my-id (dot-id-of object)))
  (if answer
      (when (typep answer 'answer)
	(let ((just (answer-justification answer))
	      hidden )
	  (setq hidden
	    (and (hidden-subgoal parent-id) (not previous-sg-hidden)) )
	  (output just :link-label nil :record (when hidden record)
		  :previous-sg-hidden hidden
		  :parent-id (if hidden parent-id my-id) )))
    (unless (find object *explored*)
      (push object *explored*)
      (dolist (child (objects-below object))
	(let ((label nil))
	  (when (or (typep child 'dtp-conjunction)
		    (typep child 'r-justification) )
	    (setq label (binding-label-between object child)) )
	  (output child :parent-id my-id :link-label label) ))))
  (unless *graphic-display* (decf *depth*)) )

;;;----------------------------------------------------------------------------

(defmethod show-below
    ((object dtp-conjunction)
     &key (answer nil) (parent-id nil) (my-id nil) (record nil)
     (link-label nil) (previous-sg-hidden nil) &allow-other-keys )
  (unless (hidden-conjunction object)
    (cond
     (my-id
      (setq parent-id my-id) )
     ((and (typep object 'dtp-forked-conjunction)
	   (not *display-blocked-separately*) )
      (setq parent-id (dot-id-of (slot-value object 'parent-conjunction))) )
     (t
      (setq parent-id (dot-id-of object)) ))
    (setq link-label nil) )
  (if answer
      (when (typep answer 'answer)
	(loop
	    with sg-justs =
	      (remove-if-not #'s-justification-p (answer-justification answer))
	    for just in sg-justs
	    for o-record =
	      (if (hidden-conjunction object)
		  record
		(s-just-conjunct-number just) )
	    for o-hidden =
	      (if (hidden-conjunction object)
		  previous-sg-hidden
		nil )
	    do (output (s-just-subgoal just) :parent-id parent-id
		       :record o-record :previous-sg-hidden o-hidden
		       :link-label link-label :answer (s-just-answer just) )
	       ))
    (dolist (child (objects-below object))
      (output child :parent-id parent-id :link-label link-label) )
    ))

;;;----------------------------------------------------------------------------

(defmethod show-below
    ((object dtp-conjunct)
     &key (parent-id nil) (link-label nil) &allow-other-keys )
  (if (typep (dot-id-to-object parent-id) 'dtp-conjunction)
      (let* ((parent-c (slot-value object 'parent-conjunction))
	     (num (position object (slot-value parent-c 'list))) )
	(dolist (child (objects-below object))
	  (output
	   child :parent-id parent-id :record num :link-label link-label )))
    (dolist (child (objects-below object))
      (output child :parent-id parent-id :link-label link-label) )))

;;;----------------------------------------------------------------------------

(defmethod show-below
    ((object c-justification)
     &key (parent-id nil) (record nil) (link-label nil)
     (previous-sg-hidden nil) &allow-other-keys )
  (output (c-just-conjunction object) :parent-id parent-id :record record
	  :link-label link-label :answer (c-just-answer object)
	  :previous-sg-hidden previous-sg-hidden ))

;;;----------------------------------------------------------------------------

(defmethod show-below ((object sg-cache-justification) &key &allow-other-keys)
  (output (sg-cache-just-subgoal object)) )

;;;----------------------------------------------------------------------------

(defmethod show-below
    ((object justification) &key (answer nil) &allow-other-keys)
  "Nothing is below remaining justifications"
  (declare (ignore answer)) )

;;;----------------------------------------------------------------------------
;;;
;;;	Objects-Below

(defgeneric objects-below (object))

;;;----------------------------------------------------------------------------

(defmethod objects-below ((object proof))
  (hide-single-conjunctions
   (sort
    (remove-uninitialized
     (append
      (proof-used-conjunctions object)
      (proof-blocked-conjunctions object)
      (proof-query-conjunctions object) ))
    #'< :key #'(lambda (c) (slot-value c 'order)) )))

;;;----------------------------------------------------------------------------

(defmethod objects-below ((object dtp-subgoal))
  (let (objs)
    (setq objs
      (append
       (slot-value object 'used-inferences)
       (slot-value object 'blocked-conjunctions)
       (when (listp (slot-value object 'inferences)) ; Unless :UNINITIALIZED
	 (remove-if #'virgin-p (slot-value object 'inferences)) )))
    (setq objs (sort objs #'< :key #'(lambda (c) (slot-value c 'order))))

    (cond

     ;; Normal subgoal
     (objs
      (let ((reductions (slot-value object 'answers)))
	(setq reductions (remove-if-not #'answer-context reductions))
	(setq reductions (mapcar #'answer-justification reductions))
	(setq reductions
	  (remove-if-not
	   #'(lambda (just) (eq (r-just-leaf-subgoal just) object))
	   reductions ))
	(append reductions objs) ))
     
     ;; Success cache
     ((setq objs
	(mapcar #'answer-justification (slot-value object 'answers)) )
      objs )
     
     ;; Failure cache
     ((slot-value object 'failure-explanation)
      (list (slot-value object 'failure-explanation)) )
     )))

;;;----------------------------------------------------------------------------

(defmethod objects-below ((object dtp-conjunction))
  (let (conjuncts)
    (setq conjuncts (slot-value object 'list))
    (when (typep object 'dtp-forked-conjunction)
      (setq conjuncts (nthcdr (slot-value object 'top-conjunct) conjuncts)) )
    conjuncts ))

;;;----------------------------------------------------------------------------

(defmethod objects-below ((object dtp-conjunct))
  "If forked conjunction, then new subgoals below, else all subgoals below"
  (let ((parent-c (slot-value object 'parent-conjunction))
	subgoals )
    (setq subgoals (subgoals-below-conjunct object))
    (when (and (not *display-blocked-separately*)
	       (typep parent-c 'dtp-forked-conjunction) )
      (let ((un-f-c (slot-value parent-c 'parent-conjunction))
	    (num (position object (slot-value parent-c 'list)))
	    un-f-conj un-f-subgoals )
	(setq un-f-conj (nth num (slot-value un-f-c 'list)))
	(setq un-f-subgoals (subgoals-below-conjunct un-f-conj))
	(setq subgoals (set-difference subgoals un-f-subgoals)) ))
    subgoals ))

(defun subgoals-below-conjunct (conjunct)
  #+dtp-types (declare (type dtp-conjunct conjunct))
  (remove-duplicates
   (remove-uninitialized
    (append
     (slot-value conjunct 'used-subgoals)
     (list (slot-value conjunct 'subgoal)) ))))

;;;----------------------------------------------------------------------------

(defun hide-single-conjunctions (conjunctions)
  #+dtp-type (declare (type list conjunctions))
  (reduce
   #'append
   (mapcar
    #'(lambda (c)
	#+dtp-types (declare (type dtp-conjunction c))
	(if (eq (length (slot-value c 'list)) 1)
	    (objects-below (first (slot-value c 'list)))
	  (list c) ))
    conjunctions )))

;;;----------------------------------------------------------------------------

(defun remove-uninitialized (l)
  #+dtp-types (declare (type list l))
  (remove-if-not #'(lambda (x) (typep x 'dtp-object)) l) )

;;;----------------------------------------------------------------------------
