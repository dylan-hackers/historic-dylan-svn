;;; -*- Mode: LISP; Syntax: Common-lisp; Package: DTP; Base: 10 -*-

;;;----------------------------------------------------------------------------
;;;
;;;	File		Caching.Lisp
;;;	System		Don's Theorem Prover
;;;
;;;	Written by	Don Geddis (Geddis@CS.Stanford.Edu)

(in-package "DTP")

;;;----------------------------------------------------------------------------
;;;
;;;	Public

;;;----------------------------------------------------------------------------
;;; Postponement and Recursion caching

(defun find-subgoal (conjunct)
  #+dtp-types (declare (type dtp-conjunct conjunct))
  "Return (1) an old or new subgoal SAMEP as LITERAL, (2) binding list or nil"
  (let ((bl nil)
	(literal (literal-in-context conjunct))
	sg )
    (setq sg (find-stored-subgoal literal (subgoal-parent-of conjunct)))
    (if sg
	(progn
	  (incf (proof-cache-count *proof*))
	  #+dtp-trace
	  (when (and (find :caching *trace*)
		     *cache-size*
		     (= (proof-cache-count *proof*) *cache-size*) )
	    (indent-line)
	    (format *debug-io* "Cache bound reached...")
	    (format *debug-io* "now using :RECURSION caching~%") )
	  (setq bl
	    (dtp-samep-binding-list
	     (literal-terms (slot-value sg 'literal))
	     (literal-terms literal) ))
	  #+dtp-trace
	  (when (find :caching *trace*)
	    (indent-line)
	    (format *debug-io* "Attaching to existing subgoal ")
	    (print-literal-node (slot-value sg 'literal) :s *debug-io*)
	    (format *debug-io* "~%") )
	  (possibly-decrease-subgoal-depth sg conjunct) )
      (progn
	(setq sg (make-new-subgoal conjunct literal))
	(memo-subgoal sg)
	#+dtp-trace
	(when (find :caching *trace*)
	  (indent-line)
	  (format *debug-io* "Subgoal ")
	  (print-literal-node literal :s *debug-io*)
	  (format *debug-io* " not found in cache, so creating new one~%") )))
    (values sg bl) ))

(defun possibly-decrease-subgoal-depth (subgoal new-parent-conjunct)
  "Searching space as a graph, so when new cache link perhaps higher subgoal"
  #+dtp-types (declare (type dtp-subgoal subgoal))
  #+dtp-types (declare (type dtp-conjunct new-parent-conjunct))

  ;; Not useful at the moment, so abort
  (return-from possibly-decrease-subgoal-depth)
  
  (let ((parent-sg (subgoal-parent-of new-parent-conjunct))
	alternate-height )
    (if parent-sg
	(setq alternate-height (1+ (slot-value parent-sg 'depth)))
      (setq alternate-height 0) )
    (when (< alternate-height (slot-value subgoal 'depth))
      (setf (slot-value subgoal 'depth) alternate-height)
      #+dtp-trace
      (when (find :caching *trace*)
	(indent-line)
	(format *debug-io* "And lowering subgoal depth to ~D~%"
		alternate-height ))
      )))

;;;----------------------------------------------------------------------------
;;; Subgoal caching

(defun remember-completed-subgoal (subgoal)
  #+dtp-types (declare (type dtp-subgoal subgoal))
  #+dtp-trace
  (when (find :caching *trace*)
    (indent-line)
    (format *debug-io* "Storing completed subgoal ")
    (print-literal-node (slot-value subgoal 'literal) :s *debug-io*) )
  (let ((c-sg (subgoal-in-cache-p subgoal)))
    (if c-sg
	(progn
	  #+dtp-trace
	  (when (find :caching *trace*)
	    (format *debug-io* "...but ")
	    (print-literal-node (slot-value c-sg 'literal) :s *debug-io*)
	    (format *debug-io* " is already in the cache~%") ))
      (progn
	(push subgoal (proof-subgoal-cache *proof*))
	#+dtp-trace
	(when (find :caching *trace*)
	  (format *debug-io* "~%") ))
      )))

(defun subgoal-in-cache-p (subgoal)
  #+dtp-types (declare (type dtp-subgoal subgoal))
  "Returns matching subgoal or NIL"
  (find (slot-value subgoal 'literal)
	(proof-subgoal-cache *proof*)
	:key #'(lambda (sg) (slot-value sg 'literal))
	:test #'literal-same-or-generalized-p ))

(defun solutions-to-subgoal (subgoal)
  #+dtp-types (declare (type dtp-subgoal subgoal))
  "Returns (1) Matching subgoal or NIL, (2) List of Answers"
  (let (c-sg)
    (setq c-sg
      (find (slot-value subgoal 'literal)
	    (proof-subgoal-cache *proof*)
	    :key #'(lambda (sg) (slot-value sg 'literal))
	    :test #'literal-same-or-generalized-p ))
    (if c-sg
	(progn
	  #+dtp-trace
	  (when (find :caching *trace*)
	    (indent-line)
	    (format *debug-io* "Subgoal ")
	    (print-literal-node (slot-value subgoal 'literal) :s *debug-io*)
	    (format *debug-io* " matches cached subgoal ")
	    (print-literal-node (slot-value c-sg 'literal) :s *debug-io*)
	    (format *debug-io* "~%") )
	  (values c-sg (create-relevant-answers subgoal c-sg)) )
      (values nil nil) )))

;;;----------------------------------------------------------------------------
;;; Answer caching (success and failure)

(defun remember-success (literal)
  #+dtp-types (declare (type literal-node literal))
  #+dtp-trace
  (when (find :caching *trace*)
    (indent-line)
    (format *debug-io* "Storing proven literal ")
    (print-literal-node literal :s *debug-io*) )
  (if (find literal (proof-success-cache *proof*) :test #'literal-samep)
      (progn
	#+dtp-trace
	(when (find :caching *trace*)
	  (format *debug-io* "...but already in the cache~%") ))
    (progn
      (push literal (proof-success-cache *proof*))
      #+dtp-trace
      (when (find :caching *trace*)
	(format *debug-io* "~%") ))
    ))

(defun remember-failure (literal)
  #+dtp-types (declare (type literal-node literal))
  #+dtp-trace
  (when (find :caching *trace*)
    (indent-line)
    (format *debug-io* "Storing unprovable literal ")
    (print-literal-node literal :s *debug-io*) )
  (if (find literal (proof-failure-cache *proof*) :test #'literal-samep)
      (progn
	#+dtp-trace
	(when (find :caching *trace*)
	  (format *debug-io* "...but already in the cache~%") ))
    (progn
      (push literal (proof-failure-cache *proof*))
      #+dtp-trace
      (when (find :caching *trace*)
	(format *debug-io* "~%") ))
    ))

(defun lookup-literal (literal)
  #+dtp-trace (declare (type literal-node literal))
  "Returns (1) Cached literal or NIL, (2) :success, :failure, or NIL"
  (let (c-lit)
    (cond
     ((setq c-lit
	(find literal (proof-success-cache *proof*)
	      :test #'literal-same-or-generalized-p ))
      (values c-lit :success) )
     ((setq c-lit
	(find literal (proof-failure-cache *proof*)
	      :test #'literal-same-or-generalized-p ))
      (values c-lit :failure) )
     (t
      (values nil nil) ))
    ))

(defun flush-answer-failure-cache ()
  (setf (proof-failure-cache *proof*) nil) )

;;;----------------------------------------------------------------------------

(defun make-new-subgoal (conjunct &optional (new-literal nil))
  #+dtp-types (declare (type dtp-conjunct conjunct))
  (unless new-literal
    (setq new-literal (literal-in-context conjunct)) )
  (let (subgoal)
    (with-slots (parent-conjunction) conjunct
      (setq subgoal (make-instance 'dtp-subgoal :literal new-literal))
      (with-slots (parent-subgoal) parent-conjunction
	(setf (slot-value subgoal 'parent-subgoal) parent-subgoal)
	(setf (slot-value subgoal 'parent-conjunct) conjunct)
	(if parent-subgoal
	    (setf (slot-value subgoal 'depth)
	      (1+ (slot-value parent-subgoal 'depth)) )
	  (setf (slot-value subgoal 'depth) 0) ))
      subgoal )))

;;;----------------------------------------------------------------------------
;;;
;;;	Private

;;;----------------------------------------------------------------------------

(defun literal-in-context (conjunct)
  #+dtp-types (declare (type dtp-conjunct conjunct))
  (let (literal)
    (setq literal
      (literal-plug
       (slot-value conjunct 'literal)
       (slot-value conjunct 'binding-list) ))
    (setq literal (nsimplify-terms literal)) ; Hook for term rewriting
    literal ))

;;;----------------------------------------------------------------------------

(defvar *current-recursion-ancestors*)

(defun find-stored-subgoal (literal parent-subgoal)
  #+dtp-types (declare (type literal-node literal))
  #+dtp-types (declare (type (or null dtp-subgoal) parent-subgoal))
  (unless (find *caching* '(:subgoals :recursion :postponement))
    (return-from find-stored-subgoal nil) )
  (let (sg-list)
    (setq sg-list
      (gethash (literal-relation literal) (proof-subgoal-index *proof*)) )
    (if (doing-recursion-caching)
	(let ((*current-recursion-ancestors* nil))
	  (when parent-subgoal
	    (setq *current-recursion-ancestors*
	      (subgoal-ancestors-of parent-subgoal :include-me t) ))
	  (find literal sg-list :test #'recursion-samep) )
      (find literal sg-list
	    :test (case *caching*
		    (:postponement #'postponement-samep)
		    (:subgoals #'subgoals-samep) )))
    ))

;;;----------------------------------------------------------------------------

(defun postponement-samep (literal match-subgoal)
  #+dtp-types (declare (type literal-node literal))
  #+dtp-types (declare (type dtp-subgoal match-subgoal))
  (literal-samep literal (slot-value match-subgoal 'literal)) )

(defun recursion-samep (literal match-subgoal)
  #+dtp-types (declare (type literal-node literal))
  #+dtp-types (declare (type dtp-subgoal match-subgoal))
  (and (literal-samep literal (slot-value match-subgoal 'literal))
       (find match-subgoal *current-recursion-ancestors*) ))

(defun subgoals-samep (literal match-subgoal)
  #+dtp-types (declare (type literal-node literal))
  #+dtp-types (declare (type dtp-subgoal match-subgoal))
  (and (literal-samep literal (slot-value match-subgoal 'literal))
       (exhausted-p match-subgoal) ))

;;;----------------------------------------------------------------------------

(defun doing-recursion-caching ()
  (or (eq *caching* :recursion)
      (and (eq *caching* :postponement)
	   *cache-size*
	   (> (proof-cache-count *proof*) *cache-size*) )))

;;;----------------------------------------------------------------------------

(defun memo-subgoal (subgoal)
  #+dtp-types (declare (type dtp-subgoal subgoal))
  (unless (find *caching* '(:subgoals :recursion :postponement))
    (return-from memo-subgoal) )
  (let ((relation (literal-relation (slot-value subgoal 'literal)))
	(table (proof-subgoal-index *proof*)) )
    (if (gethash relation table)
	(push subgoal (gethash relation table))
      (setf (gethash relation table) (list subgoal)) )
    ))

;;;----------------------------------------------------------------------------

(defun create-relevant-answers (inst-subgoal cache-subgoal)
  #+dtp-types (declare (type dtp-subgoal inst-subgoal cache-subgoal))
  (loop
      with i-terms = (literal-terms (slot-value inst-subgoal 'literal))
      with c-terms = (literal-terms (slot-value cache-subgoal 'literal))
      for c-answer in
	(remove-if #'answer-ae-binding-lists
		   (slot-value cache-subgoal 'answers) )
      for c-sexp = (plug c-terms (answer-binding-list c-answer))
      for new-bl = (matching-bl i-terms c-sexp)
      when new-bl
      collect (let ((new-answer (copy-answer c-answer)))
		(setf (answer-binding-list new-answer) new-bl)
		#+dtp-trace
		(setf (answer-justification new-answer)
		  (make-sg-cache-justification :subgoal cache-subgoal) )
		new-answer )))

(defun matching-bl (new-sexp1 cache-sexp2)
  #+dtp-types (declare (type list new-sexp1 cache-sexp2))
  (let ((cache-vars (remove-duplicates (find-vars cache-sexp2)))
	new-bl )
    (setq new-bl (dtp-unifyp new-sexp1 cache-sexp2))
    (setq new-bl
      (remove-if #'(lambda (binding)
		     (or (find (car binding) cache-vars)
			 (find (cdr binding) cache-vars) ))
		 new-bl ))
    new-bl ))

;;;----------------------------------------------------------------------------
