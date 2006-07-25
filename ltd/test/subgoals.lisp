;;; -*- Mode: LISP; Syntax: Common-lisp; Package: DTP; Base: 10 -*-

;;;----------------------------------------------------------------------------
;;;
;;;	File		Subgoals.Lisp
;;;	System		Don's Theorem Prover
;;;
;;;	Written by	Don Geddis (Geddis@CS.Stanford.Edu)

(in-package "DTP")

;;;----------------------------------------------------------------------------
;;;
;;;	Public

;;;----------------------------------------------------------------------------

(defmethod expand ((subgoal dtp-subgoal))
  (unless (possibly-find-in-cache subgoal)
    (expand-subgoal subgoal) ))

#+dtp-trace
(defmethod expand :around ((subgoal dtp-subgoal))
  (when (find :subgoals *trace*)
    (indent-line)
    (format *debug-io* "Expanding subgoal ")
    (print-literal-node (slot-value subgoal 'literal) :s *debug-io*)
    (format *debug-io* " [~D]~%" (slot-value subgoal 'depth)) )
  (incf *depth*)
  (prog1
      (call-next-method)
    (decf *depth*) ))

;;;----------------------------------------------------------------------------

(defmethod exhausted-p ((subgoal dtp-subgoal) &key (ignore-blocked nil))
  "True iff no further answers will be coming from this SUBGOAL"
  (with-slots (inferences blocked-conjunctions)
      subgoal
    (and (null inferences)
	 (or ignore-blocked
	     (null blocked-conjunctions) ))
    ))

(defmethod virgin-p ((subgoal dtp-subgoal))
  "True iff SUBGOAL has never been worked on"
  (with-slots (remaining-ancestor-subgoals assumables inferences) subgoal
    (and
     (eq remaining-ancestor-subgoals :uninitialized)
     (eq assumables :uninitialized)
     (eq inferences :uninitialized) )))

(defmethod active-p ((subgoal dtp-subgoal))
  "True iff SUBGOAL has more constructive work to do"
  (slot-value subgoal 'inferences) )

(defmethod blocked-p ((subgoal dtp-subgoal))
  (and (not (slot-value subgoal 'inferences))
       (slot-value subgoal 'blocked-conjunctions) ))

;;;----------------------------------------------------------------------------

(defmethod propagate (answer (subgoal dtp-subgoal))
  (with-slots
      (literal answers answers conjuncts-to-propagate-to parent-subgoal)
      subgoal

    (setq answer (copy-answer answer))
    (setf (answer-subgoal answer) subgoal)

    ;; If a reduction reached the uppermost goal, then not context-dependent
    (setf (answer-context answer)
      (remove subgoal (answer-context answer)) )
    
    ;; Remove bindings that aren't relevant (unless they might be later)
    (unless (answer-context answer)
      (setq answer (nsimplify-binding-list answer subgoal)) )
    
    (if (find answer answers :test #'answer-instance?)
	(progn
	  #+dtp-trace
	  (when (find :answers *trace*)
	    (indent-line)
	    (format *debug-io* "Answer ~A ignored because already know ~A~%"
		    answer (find answer answers :test #'answer-instance?) )))
      (progn
	(add-to-end answer answers)
	(possibly-cache answer subgoal)
	(dolist (conjunct conjuncts-to-propagate-to)
	  (propagate answer conjunct) )))
    ))

#+dtp-trace
(defmethod propagate :around (answer (subgoal dtp-subgoal))
  "For proof tracing"
  (when (find :answers *trace*)
    (indent-line)
    (format *debug-io* "Propagating ~S to subgoal " answer)
    (print-literal-node (slot-value subgoal 'literal) :s *debug-io*)
    (format *debug-io* "~%") )
  (incf *depth*)
  (prog1
    (call-next-method)
    (decf *depth*) ))

;;;----------------------------------------------------------------------------

(defmethod terminate ((subgoal dtp-subgoal) &optional (and-cache t))
  "Let waiting conjuncts know that there are no more answers"
  (when and-cache
    (cond
     ((find *caching* '(:failure :answers))
      (unless (slot-value subgoal 'answers)
	(remember-failure (slot-value subgoal 'literal)) ))
     ((eq *caching* :subgoals)
      (remember-completed-subgoal subgoal) )))
  (dolist (conjunct (slot-value subgoal 'conjuncts-to-propagate-to))
    (propagate :not-an-answer conjunct) ))

;;;----------------------------------------------------------------------------

(defgeneric subgoal-parent-of (object))

(defmethod subgoal-parent-of ((object proof))
  nil )

(defmethod subgoal-parent-of ((object dtp-subgoal))
  (slot-value object 'parent-subgoal) )

(defmethod subgoal-parent-of ((object dtp-conjunction))
  (slot-value object 'parent-subgoal) )

(defmethod subgoal-parent-of ((object dtp-conjunct))
  (slot-value (slot-value object 'parent-conjunction) 'parent-subgoal) )

;;;----------------------------------------------------------------------------

#+dtp-trace
(defgeneric debug-print-subgoal (object))

#+dtp-trace
(defmethod debug-print-subgoal ((object null))
  (format *debug-io* " of query") )

#+dtp-trace
(defmethod debug-print-subgoal ((object dtp-subgoal))
  (format *debug-io* " of subgoal ")
  (print-literal-node (slot-value object 'literal) :s *debug-io*) )

#+dtp-trace
(defmethod debug-print-subgoal ((object dtp-conjunction))
  (debug-print-subgoal (slot-value object 'parent-subgoal)) )

#+dtp-trace
(defmethod debug-print-subgoal ((object dtp-conjunct))
  (debug-print-subgoal (slot-value object 'parent-conjunction)) )

;;;----------------------------------------------------------------------------
;;;
;;;	Private

;;;----------------------------------------------------------------------------

(defun expand-subgoal (subgoal)
  #+dtp-types (declare (type dtp-subgoal subgoal))
  (let (answer)
    #+dtp-types (declare (type (or answer null) answer))
    (cond
     ((setq answer (expand-reduction subgoal))
      (propagate answer subgoal) )
     ((setq answer (expand-residue subgoal))
      (propagate answer subgoal) )
     (t
      (with-slots (inferences blocked-conjunctions #+dtp-trace used-inferences)
	  subgoal
	(when (eq inferences :uninitialized)
	  (setq inferences (compute-inference subgoal))
	  (setq inferences (norder-conjunctions inferences))
	  #+dtp-trace (setq inferences (note-order inferences subgoal)) )
	(if inferences
	    (let ((conjunction (first inferences)))
	      (with-slots (list binding-list label ae-binding-list) conjunction
		(cond
		 ((null list)
		  (pop inferences)
		  #+dtp-trace (add-to-end conjunction used-inferences)
		  (propagate
		   (make-answer
		    :binding-list binding-list :label label
		    :ae-binding-lists
		    (when ae-binding-list (list ae-binding-list))
		    #+dtp-trace :justification
		    #+dtp-trace
		    (make-l-justification :id (slot-value conjunction 'origin))
		    #+dtp-trace :proof #+dtp-trace *proof* )
		   subgoal ))
		 ((exhausted-p conjunction)
		  (pop inferences)
		  #+dtp-trace (add-to-end conjunction used-inferences)
		  (when (exhausted-p subgoal)
		    (terminate subgoal) ))
		 (t
		  (expand conjunction) ))
		))
	  (if (exhausted-p subgoal)
	      (terminate subgoal)
	    #+dtp-trace
	    (when (find :subgoals *trace*)
	      (indent-line)
	      (format *debug-io* "No work possible on subgoal (waiting)~%") ))
	  ))
      ))))

;;;----------------------------------------------------------------------------

(defun expand-reduction (subgoal)
  #+dtp-types (declare (type dtp-subgoal subgoal))
  (unless *use-reduction*
    (return-from expand-reduction nil) )
  (with-slots (remaining-ancestor-subgoals #+dtp-trace used-ancestors) subgoal
    (when (eq remaining-ancestor-subgoals :uninitialized)
      (setq remaining-ancestor-subgoals (subgoal-ancestors-of subgoal)) )
    (loop
	for ancestor = (pop remaining-ancestor-subgoals)
	while ancestor
	for answer = (reduction subgoal ancestor)
	do #+dtp-trace
	   (add-to-end ancestor used-ancestors)
	   (when answer
	     #+dtp-trace
	     (when (find :answers *trace*)
	       (indent-line)
	       (format *debug-io* "Found ~A by reduction~%" answer)
	       (indent-line)
	       (format *debug-io* " with ~A~%" ancestor) )
	     (return answer) ))
    ))

;;;----------------------------------------------------------------------------

(defun expand-residue (subgoal)
  #+dtp-types (declare (type dtp-subgoal subgoal))
  (unless *use-residue*
    (return-from expand-residue nil) )
  (with-slots (literal assumables) subgoal
    (when (eq assumables :uninitialized)
      (setq assumables (proof-assumables *proof*)) )
    (loop
	for assumable = (pop assumables)
	while assumable
	for answer = (residue-answer subgoal assumable)
	when answer
	do #+dtp-trace
	   (when (find :answers *trace*)
	     (indent-line)
	     (format *debug-io* "Assuming residue ")
	     (print-literal-node literal :s *debug-io*)
	     (format *debug-io* " to get ~A~%" answer) )
	   (return answer) )))

;;;----------------------------------------------------------------------------

(defun compute-inference (subgoal)
  "Returns list of conjunctions"
  #+dtp-types (declare (type dtp-subgoal subgoal))
  (unless *use-subgoal-inference*
    (return-from compute-inference nil) )
  (loop
      with sg-literal = (slot-value subgoal 'literal)
      with node-list-1 = (proof-goal-nodes *proof*)
      with node-list-2 =
	(remove-pure-literal-nodes
	 (active-theory-contents
	  (proof-theory *proof*)
	  (literal-relation (slot-value subgoal 'literal)) ))
      for resolving-with-goal-p = node-list-1
      for kb-node =
	(if node-list-1 (pop node-list-1) (pop node-list-2))
      while kb-node
      appending
	(loop
	    with c-bl =
	      (multiple-value-list
		  (clause-rename-all-variables (kb-node-clause kb-node)) )
	    with clause = (first c-bl)
	    with tbl = (second c-bl)
	    for literal in
	      (if *use-contrapositives*
		  (clause-literals clause)
		(list (first (clause-literals clause))) )
	    for mgu = (literal-mgu literal sg-literal)
	    when mgu
	    collect
	      (resolve mgu sg-literal literal clause subgoal
		       (kb-node-id kb-node)
		       (when resolving-with-goal-p tbl) ))
	))

;;;----------------------------------------------------------------------------

(defun resolve (mgu sg-lit kb-lit clause parent kb-parent ans-ext-bl)
  "Returns conjunction"
  #+dtp-types (declare (type binding-list mgu))
  #+dtp-types (declare (type literal-node sg-lit kb-lit))
  #+dtp-types (declare (type clause-node clause))
  #+dtp-types (declare (type dtp-subgoal parent))
  #+dtp-types (declare (type symbol kb-parent))
  #+dtp-types (declare (type binding-list ans-ext-bl))
  #-dtp-trace (declare (ignore kb-parent))
  (setq ans-ext-bl (remove '(t . t) ans-ext-bl :test #'equal))
  (let ((sg-vars (find-vars (literal-terms sg-lit)))
	bl aebl conjunction )
    (setq clause (copy-clause-node clause))
    (setf (clause-literals clause) (remove kb-lit (clause-literals clause)))
    (setq clause (clause-plug clause mgu))
    (nclause-flip-negations clause)
    (setq bl
      (remove-if-not
       #'(lambda (binding)
	   #+dtp-types (declare (type binding binding))
	   (find (binding-variable binding) sg-vars) )
       mgu ))
    (setq aebl (simplify-ae-bl (plug ans-ext-bl mgu)))
    (unless (different-binding-lists bl aebl)
      (setq aebl nil) )
    (setq conjunction
      (make-instance 'dtp-conjunction
	:parent parent
	#+dtp-trace :origin #+dtp-trace kb-parent
	:binding-list bl
	:label (clause-label clause)
	:ae-binding-list aebl ))
    (setf (slot-value conjunction 'list)
      (mapcar
       #'(lambda (lit)
	   #+dtp-types (declare (type literal-node lit))
	   (make-instance 'dtp-conjunct :literal lit :parent conjunction) )
       (clause-literals clause) ))
    conjunction ))

;;;----------------------------------------------------------------------------

(defun simplify-ae-bl (binding-list)
  #+dtp-types (declare (type list binding-list))
  (loop
      for binding in binding-list
      unless (eq (binding-variable binding) (binding-value binding))
      collect binding ))

;;;----------------------------------------------------------------------------

(defmethod subgoal-ancestors-of
    ((subgoal dtp-subgoal) &key (include-me nil) &allow-other-keys)
  "List of all direct parent subgoals above SUBGOAL in the proof graph"
  #+dtp-types (declare (type (or dtp-subgoal null) subgoal))
  (when subgoal (subgoal-ancestors-of-internal subgoal nil include-me)) )

(defmethod subgoal-ancestors-of
    ((conjunct dtp-conjunct) &key &allow-other-keys)
  (let ((parent (subgoal-parent-of conjunct)))
    (when parent
      (subgoal-ancestors-of parent :include-me t) )))

(defun subgoal-ancestors-of-internal (subgoal ancestors-so-far include-me)
  #+dtp-types (declare (type dtp-subgoal subgoal))
  #+dtp-types (declare (type list ancestors-so-far))
  (if (find subgoal ancestors-so-far)
      ancestors-so-far
    (with-slots (parent-subgoal) subgoal
      (when include-me
	(push subgoal ancestors-so-far) )
      (if parent-subgoal
	  (subgoal-ancestors-of-internal parent-subgoal ancestors-so-far t)
	ancestors-so-far ))))

;;;----------------------------------------------------------------------------

#+dtp-trace
(defun note-order (conjunctions subgoal)
  "Record the order that the conjunctions were explored, for explanation"
  #+dtp-types (declare (type list conjunctions))
  (loop
      for conjunction in conjunctions
      for count from 0
      do (setf (slot-value conjunction 'order) count)
      finally
	(setf (slot-value subgoal 'conjunction-count) count)
	(return conjunctions) ))

;;;----------------------------------------------------------------------------

(defun reduction (subgoal ancestor)
  (let ((lit1 (slot-value subgoal 'literal))
	(lit2 (slot-value ancestor 'literal))
	mgu )
    (setq mgu (literal-negated-pair-p lit1 lit2))
    (when mgu
      (make-answer
       :binding-list (dtp-ify-binding-list mgu)
       :context (list ancestor)
       #+dtp-trace :justification
       #+dtp-trace
       (make-r-justification :ancestor-subgoal ancestor :leaf-subgoal subgoal)
       ))
    ))

;;;----------------------------------------------------------------------------
;;;
;;;	Pure Literal Elimination

(defun remove-pure-literal-nodes (nodes)
  #+dtp-types (declare (type list nodes))
  (unless *use-pure-literal-elimination*
    (return-from remove-pure-literal-nodes nodes) )
  (loop
      for node
	  #-lucid of-type #-lucid kb-node
      in nodes
      unless (pure-literal-node-p node)
      collect node ))

(defun pure-literal-node-p (node)
  #+dtp-types (declare (type kb-node node))
  (let (pure-p)
    (setq pure-p
      (gethash (kb-node-id node) (proof-pure-literal-nodes *proof*) :unknown) )
    (when (eq pure-p :unknown)
      (setq pure-p
	(loop
	    named check-pure
	    for literal in (clause-literals (kb-node-clause node))
	    unless (can-find-matching-literal literal :except node)
	    do #+dtp-trace
	       (when (find :proofs *trace*)
		 (format *debug-io* "[Pure literal ")
		 (print-literal-node-as-logic literal :s *debug-io*)
		 (format *debug-io* " detected in ~A...removing~%~7T"
			 (kb-node-id node) )
		 (print-clause-node
		  (kb-node-clause node) :s *debug-io* :as-rule t )
		 (format t "~% for duration of proof]~%") )
	       (return-from check-pure t)
	    finally (return-from check-pure nil) ))
      (setf (gethash (kb-node-id node) (proof-pure-literal-nodes *proof*))
	pure-p ))
    pure-p ))

(defun can-find-matching-literal (literal &key (except nil))
  #+dtp-types (declare (type literal-node literal))
  #+dtp-types (declare (type kb-node except))
  (or
   ;; Matching in query or (active) database...
   (loop
       for kb-node in
	 (append
	  (proof-goal-nodes *proof*)
	  (remove
	   except
	   (active-theory-contents
	    (proof-theory *proof*) (literal-relation literal) )))
       do
	 (loop
	     with test-clause =
	       (clause-rename-all-variables (kb-node-clause kb-node))
	     for kb-literal in (clause-literals test-clause)
	     when (literal-possible-negated-pair-p literal kb-literal)
	     do (return-from can-find-matching-literal kb-node) )
       finally (return nil) )
   ;; ...or assumptions
   (find literal (proof-assumables *proof*) :test #'literal-negated-pair-p)
   ))

;;;----------------------------------------------------------------------------

(defun has-identical-ancestor (subgoal)
  "T iff subgoal has an identical parent"
  #+dtp-types (declare (type dtp-subgoal subgoal))
  (loop
      with sg-literal = (slot-value subgoal 'literal)
      for ancestor in (subgoal-ancestors-of subgoal)
      for ans-literal = (slot-value ancestor 'literal)
      when (literal-equal-p sg-literal ans-literal)
      return t
      finally (return nil) ))

;;;----------------------------------------------------------------------------

(defun possibly-cache (answer subgoal)
  #+dtp-types (declare (type answer answer))
  #+dtp-types (declare (type dtp-subgoal subgoal))
  (cond

   ((find *caching* '(:success :answers))
    (unless (or (answer-label answer)
		(answer-residue answer)
		(answer-ae-binding-lists answer) )
      (remember-success
       (literal-plug
	(slot-value subgoal 'literal) (answer-binding-list answer) ))
      ))

   ))

;;;----------------------------------------------------------------------------

(defun exhaust (subgoal)
  #+dtp-types (declare (type dtp-subgoal subgoal))
  (with-slots (remaining-ancestor-subgoals assumables inferences) subgoal
    (setq remaining-ancestor-subgoals nil)
    (setq assumables nil)
    (setq inferences nil) ))

;;;----------------------------------------------------------------------------

(defun possibly-find-in-cache (subgoal)
  #+dtp-types (declare (type dtp-subgoal subgoal))
  "Returns T if found (and uses cache), NIL if not found"
  (cond

   ((not (virgin-p subgoal))
    nil )
   
   ((and (eq *caching* :iap) (has-identical-ancestor subgoal))
    #+dtp-trace
    (when (find :caching *trace*)
      (indent-line)
      (format *debug-io* "Subgoal ")
      (print-literal-node (slot-value subgoal 'literal) :s *debug-io*)
      (format *debug-io* " pruned because of identical ancestor~%") )
    (agenda-remove subgoal)
    (exhaust subgoal)
    (terminate subgoal nil)
    t )

   ((find *caching* '(:success :failure :answers))
    (multiple-value-bind (c-literal where)
	(lookup-literal (slot-value subgoal 'literal))
      (case where
	(:success
	 #+dtp-trace
	 (when (find :caching *trace*)
	   (indent-line)
	   (format *debug-io* "Found ")
	   (print-literal-node c-literal :s *debug-io*)
	   (format *debug-io* " in the success cache~%") )
	 (exhaust subgoal)
	 (propagate
	  (make-answer
	   #+dtp-trace :justification
	   #+dtp-trace (make-s-cache-justification :literal c-literal)
	   #+dtp-trace :proof #+dtp-trace *proof* )
	  subgoal )
	 t )
	(:failure
	 #+dtp-trace
	 (when (find :caching *trace*)
	   (indent-line)
	   (format *debug-io* "Found ")
	   (print-literal-node c-literal :s *debug-io*)
	   (format *debug-io* " in the failure cache~%") )
	 #+dtp-trace
	 (setf (slot-value subgoal 'failure-explanation)
	   (make-f-cache-justification :literal c-literal) )	 
	 (exhaust subgoal)
	 (terminate subgoal nil)
	 t )
	(otherwise
	 nil ))
      ))
   
   ((eq *caching* :subgoals)
    (multiple-value-bind (c-sg answers) (solutions-to-subgoal subgoal)
      (if c-sg
	  (progn
	    (exhaust subgoal)
	    #+dtp-trace
	    (unless answers
	      (setf (slot-value subgoal 'failure-explanation)
		(make-sg-cache-justification :subgoal c-sg) ))
	    (dolist (answer answers)
	      (propagate answer subgoal) )
	    t )
	nil )))
   
   (t				; No caching, :recursion, or :postponement
    nil )
   ))

;;;----------------------------------------------------------------------------
