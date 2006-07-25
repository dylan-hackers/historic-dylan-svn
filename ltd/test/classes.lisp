;;; -*- Mode: LISP; Syntax: Common-lisp; Package: DTP; Base: 10 -*-

;;;----------------------------------------------------------------------------
;;;
;;;	File		Classes.Lisp
;;;	System		Don's Theorem Prover
;;;
;;;	Written by	Don Geddis (Geddis@CS.Stanford.Edu)

(in-package "DTP")

;;;----------------------------------------------------------------------------

(defclass dtp-object () ()
  (:documentation "Superclass of all DTP objects") )

;;;----------------------------------------------------------------------------

(defclass dtp-proof-node (dtp-object) ()
  (:documentation "Node in the proof space") )

;;;----------------------------------------------------------------------------

(defclass dtp-subgoal (dtp-proof-node)
  ((literal :initarg :literal :initform nil :type (or literal-node null))
   (answers :initform nil :type list)
   (conjuncts-to-propagate-to :initarg :propagate-to :initform nil :type list)
   (assumables :initform :uninitialized :type (or list (eql :uninitialized))
	       :documentation "Used for residue" )
   (inferences :initform :uninitialized :type (or list (eql :uninitialized))
	       :documentation "List of conjunction nodes" )
   (blocked-conjunctions
    :initform nil :type list
    :documentation "Conjunction nodes that are waiting for another subgoal" )
   
   ;; Cached values (could be computed, but expensive)
   (depth :initarg :depth :initform nil :type (or null (integer 0 *))
	  :documentation "Length of minimal path from root to this node" )
   (remaining-ancestor-subgoals
    :initform :uninitialized :type (or list (eql :uninitialized))
    :documentation "Used for reduction computation" )
   (parent-subgoal :initform nil :type (or null dtp-subgoal)
		   :documentation "Upward pointer in proof space" )
   (parent-conjunct :initform nil :type (or null dtp-conjunct)
		   :documentation "Upward pointer in proof space" )

   ;; Tracing / Explanations
   #+dtp-trace (used-inferences :initform nil :type list)
   #+dtp-trace (used-conjuncts
		:initform nil :type list
		:documentation "From conjunctions-to-propagate-to" )
   #+dtp-trace (used-ancestors :initform nil :type list)
   #+dtp-trace (failure-explanation :initform nil)
   #+dtp-trace (conjunction-count :initform 0 :type natural-number)
   )
  (:documentation "Subgoal node in proof space") )

(defmethod print-object ((object dtp-subgoal) stream)
  (with-slots (literal answers inferences) object
    (format stream "#<DTP Subgoal ")
    (if (literal-node-p literal)
	(print-literal-node literal :s stream)
      (format stream "?") )
    (format stream " with ~D answer~:P" (length answers))
    (when (listp inferences)
      (if inferences
	  (format stream " [~D task~:P pending]" (length inferences))
	(format stream " [complete]") ))
    (format stream ">") ))

;;;----------------------------------------------------------------------------

(defclass dtp-conjunction (dtp-proof-node)
  ((list :initarg :list :initform nil :type list
	 :documentation "List of conjuncts" )
   (stack :initform nil)
   (stack-pointer :initform 0 :type (integer -1 *)
		  :documentation "Number of current conjunct" )
   (backtrack-pointer
    :initform -1 :type (integer -1 *)
    :documentation "Conjuncts from 0 to here must not be backjumped over" )
   (parent-subgoal
    :initarg :parent :initform nil :type (or dtp-subgoal null)
    :documentation "Parent of NIL means this is a query-conjunction" )
   (binding-list :initarg :binding-list :initform nil :type binding-list
		 :documentation "From inference" )
   (label :initarg :label :initform nil :type (or label null)
	  :documentation "From inference" )
   (residue :initarg :residue :initform nil :type list
	    :documentation "Only when summarizing for instance conjunction" )
   (ae-binding-list
    :initarg :ae-binding-list :initform nil :type binding-list
    :documentation "Needed for disjunctive answers via answer extraction" )
   #+dtp-trace
   (origin :initarg :origin :type symbol
	   :documentation "KB node ID of parent rule" )
   #+dtp-trace
   (answers :initform nil :type list :documentation "List of answers found")
   #+dtp-trace
   (order :type natural-number :documentation "Original order explored")
   )
  (:documentation "Conjunction (from inference)") )

(defmethod print-object ((object dtp-conjunction) stream)
  #+dtp-types (declare (type stream stream))
  (with-slots (list stack-pointer) object
    (format stream "#<DTP Conjunction")
    (when list
      (format stream " [~D]:" stack-pointer)
      (dolist (conjunct list)
	(when (typep conjunct 'dtp-conjunct)
	  (with-slots (literal answer-count) conjunct
	    (format stream " ")
	    (if (literal-node-p literal)
		(print-literal-node literal :s stream)
	      (format stream "?") )
	    (format stream "/~D" answer-count) ))))
    (format stream ">") ))

;;;----------------------------------------------------------------------------

(defclass dtp-forked-conjunction (dtp-conjunction)
  ((top-conjunct :type natural-number
		 :documentation "Don't backtrack past this conjunct" )
   #+dtp-trace
   (parent-conjunction :documentation "Original conjunction that was forked")
   )
  (:documentation "Forked conjunction") )

(defmethod print-object ((object dtp-forked-conjunction) stream)
  #+dtp-types (declare (type stream stream))
  (with-slots (list stack-pointer top-conjunct) object
    (format stream "#<DTP F [~D] Conjunction" top-conjunct)
    (when list
      (format stream " [~D]:" stack-pointer)
      (loop
	  for conjunct in list
	  for count from 0
	  when (typep conjunct 'dtp-conjunct)
	  do (with-slots (literal binding-list answer-count) conjunct
	       (format stream " ")
	       (cond
		((not (literal-node-p literal))
		 (format stream "?") )
		((< count top-conjunct)
		 (format stream "[")
		 (print-literal-node
		  (literal-plug literal binding-list) :s stream )
		 (format stream "]") )
		(t
		 (print-literal-node literal :s stream)
		 (format stream "/~D" answer-count) )))
	     ))
    (format stream ">") ))

;;;----------------------------------------------------------------------------

(defclass dtp-conjunct (dtp-proof-node)
  ((literal :initarg :literal :initform nil :type (or literal-node null))
   (parent-conjunction
    :initarg :parent :type dtp-conjunction
    :documentation "Must be initialized when first created" )
   (binding-list :initform nil :type list
		 :documentation "Apply to literal, then search for subgoal" )
   (transform-binding-list
    :initform nil :type list
    :documentation "Apply to answers from subgoal before valid" )
   (answer-count :initform 0 :type (integer 0 *))
   (subgoal :initform :uninitialized)
   (nogoods
    :initform :uninitialized :type (or list (eql :uninitialized))
    :documentation "Backjumping dependency analysis" )
   #+dtp-trace
   (used-subgoals :initform nil :type list :documentation "For tracing only")
   )
  (:documentation "Conjunct") )

(defmethod print-object ((object dtp-conjunct) stream)
  (with-slots (literal answer-count) object
    (format stream "#<DTP Conjunct ")
    (if (literal-node-p literal)
	(print-literal-node literal :s stream)
      (format stream "?") )
    (format stream " with ~D answer~:P>" answer-count) ))

;;;----------------------------------------------------------------------------
