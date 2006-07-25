;;; -*- Mode: LISP; Syntax: Common-lisp; Package: DTP; Base: 10 -*-

;;;----------------------------------------------------------------------------
;;;
;;;	File		Structures.Lisp
;;;	System		Don's Theorem Prover
;;;
;;;	Written by	Don Geddis (Geddis@CS.Stanford.Edu)

(in-package "DTP")

;;;----------------------------------------------------------------------------

(defstruct (proof
	    (:print-function print-proof) )
  ;; Specified by user
  query
  (theory *theory*)
  (return-form nil)		; NIL => plug in to original query
  (sda nil)			; If true, suppress disjunctive answers

  ;; Internal
  query-conjunctions
  (blocked-conjunctions nil)
  subgoal-agenda
  (new-answers nil)		; Acquired by propagation, will move...
  (answers nil)			; ...to here once processed
  goal-nodes
  (pure-literal-nodes (make-hash-table :test #'eq))
				; Cache for kb nodes to ignore
  (assumables (mapcar #'list-to-literal *assumables*))
				; List of LITERAL-NODEs to match
  (consistency-check *consistency-check*)
  
  ;; Caching
  subgoal-index			; :recursion, :postponement
  (subgoal-cache nil)		; :subgoals
  (success-cache nil)		; :success, :answers
  (failure-cache nil)		; :failure, :answers
  (cache-count 0)		; Number of items in the cache
  
  ;; Iteration and cutoffs
  (subgoal-depth-cutoff *initial-subgoal-depth*)
  (subgoal-depth-skip *subgoal-depth-skip*)
  (subgoal-maximum-depth *subgoal-maximum-depth*)
  subgoal-cutoff-occurred
  (function-depth-cutoff *initial-function-depth*)
  (function-depth-skip *function-depth-skip*)
  (function-maximum-depth *function-maximum-depth*)
  function-cutoff-occurred
  
  ;; Tracing
  #+dtp-trace (used-conjunctions nil)
  #+dtp-trace (conjunction-count 0) )

(defun print-proof (structure stream depth)
  #+dtp-types (declare (type proof structure))
  #+dtp-types (declare (type stream stream))
  (declare (ignore depth))
  (format stream "#<Proof of ~A with ~D answer~:P"
	  (proof-query structure) (length (proof-answers structure)) )
  (when (null (proof-query-conjunctions structure))
    (format stream " [Complete]") )
  (format stream ">") )

;;;----------------------------------------------------------------------------

(defstruct (answer
	    (:print-function print-answer) )
  (binding-list nil)
  (context nil)			; List (possibly empty) of reduction subgoals
  (label nil)
  (residue nil)
  (ae-binding-lists nil)	; Disjunctive answers
  (subgoal nil)			; Subgoal of origin for answer
  #+dtp-trace
  (justification nil)
  #+dtp-trace
  proof )

(defun print-answer (structure stream depth)
  #+dtp-types (declare (type answer structure))
  #+dtp-types (declare (type stream stream))
  (declare (ignore depth))
  (if (answer-context structure)
      (progn
	(format stream "#<R answer [")
	(loop
	    for subgoal in (reverse (answer-context structure))
	    for first = t then nil
	    do (unless first (format stream ","))
	       (print-literal-node (slot-value subgoal 'literal) :s stream) )
	(format stream "]") )
    (format stream "#<Answer") )
  (if (answer-binding-list structure)
      (print-binding-list (answer-binding-list structure) :s stream)
    (format stream " TRUE") )
  (when (answer-label structure)
    (format stream " with label ~A"
	    (label-value (answer-label structure)) ))
  (when (answer-residue structure)
    (format stream " with residue")
    (dolist (literal (answer-residue structure))
      (format stream " ")
      (print-literal-node literal :s stream) ))
  (dolist (ae-bl (answer-ae-binding-lists structure))
    (format stream " or")
    (print-binding-list ae-bl :s stream) )
  (format stream ">") )

;;;----------------------------------------------------------------------------

(defstruct (kb-node
	    (:print-function print-kb-node) )
  id
  (clause nil) )

(defun print-kb-node (structure stream depth)
  #+dtp-types (declare (type kb-node structure))
  #+dtp-types (declare (type stream stream))
  (declare (ignore depth))
  (format stream "#<~A:~A>"
	  (kb-node-id structure) (kb-node-clause structure) ))

;;;----------------------------------------------------------------------------

(defstruct (justification))

(defstruct (l-justification	; Lookup
	    (:include justification)
	    (:conc-name l-just-) )
  id )

(defstruct (s-justification	; Subgoal
	    (:include justification)
	    (:conc-name s-just-) )
  conjunct-number
  subgoal
  answer
  t-bl )

(defstruct (c-justification	; Conjunction
	    (:include justification)
	    (:conc-name c-just-) )
  conjunction
  answer )

(defstruct (res-justification	; Residue
	    (:include justification)
	    (:conc-name res-just-) )
  assumable )

(defstruct (r-justification	; Reduction
	    (:include justification)
	    (:conc-name r-just-) )
  ancestor-subgoal
  leaf-subgoal )

;;;----------------------------------------------------------------------------

(defstruct (cache-justification (:include justification)))

(defstruct (s-cache-justification ; Success Cache
	    (:include cache-justification)
	    (:conc-name s-cache-just-) )
  literal )

(defstruct (f-cache-justification ; Failure Cache
	    (:include cache-justification)
	    (:conc-name f-cache-just-) )
  literal )

(defstruct (sg-cache-justification ; Subgoal Cache
	    (:include cache-justification)
	    (:conc-name sg-cache-just-) )
  subgoal )

;;;----------------------------------------------------------------------------
