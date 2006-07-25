;;; -*- Mode: LISP; Syntax: Common-lisp; Package: DTP; Base: 10 -*-

;;;----------------------------------------------------------------------------
;;;
;;;	File		Clauses.Lisp
;;;	System		Don's Theorem Prover
;;;
;;;	Written by	Don Geddis (Geddis@CS.Stanford.Edu)

(in-package "DTP")

;;;----------------------------------------------------------------------------

(defstruct (clause-node
	    (:conc-name clause-)
	    (:print-function clause-node-print-function) )
  "A clause: List of literals in DNF"
  literals
  (label nil) )

;;;----------------------------------------------------------------------------

(defun clause-node-print-function (structure stream depth)
  (declare (ignore depth))
  (format stream "<")
  (print-clause-node structure :s stream)
  (format stream ">") )

(defun print-clause-node (node &key (s t) (as-rule nil))
  (if *display-logic-as-lists*
      (print-clause-node-as-list node :s s)
    (progn
      (cond
       ((refutation-clause-p node)
	(format s "[True]") )
       (as-rule
	(print-clause-node-as-rule node :s s) )
       (t
	(print-clause-node-as-conjuncts node :s s) ))
      (when (clause-label node)
	(format s " with label ~A" (label-value (clause-label node))) ))
    ))

(defun print-clause-node-as-list (node &key (s t))
  (let ((lits (clause-literals node)))
    (if lits
	(progn
	  (print-literal-node (car lits) :s s)
	  (dolist (lit (cdr lits))
	    (format t " ")
	    (print-literal-node lit :s s) ))
      (format s "Nil") )))

(defun print-clause-node-as-rule (node &key (s t))
  (let ((reverse nil)
	(tail (copy-list (clause-literals node)))
	(head nil) )

    ;; Partition clause into rule head and tail
    (loop
	for lit = (first tail)
	while (and lit (not (literal-negated-p lit)))
	do (push (pop tail) head)
	finally (setq head (nreverse head)) )

    ;; Perhaps a "forward chaining" rule?
    (unless head
      (setq tail (nreverse tail))
      (loop
	  for lit = (first tail)
	  while (and lit (not (literal-negated-p lit)))
	  do (push (pop tail) head) )
      (setq tail (nreverse tail))
      (when head
	(setq reverse t) )
      (rotatef head tail) )
    
    ;; Print the head
    (loop
	for lit = (pop head)
	while lit
	do (print-literal-node lit :s s :flip-negation reverse)
	   (when head
	     (if reverse (format s " and ") (format s " or ")) ))

    ;; Print the tail
    (when tail
      (if reverse
	  (format s " => ")
	(format s " <= ") ))
    (loop
	for lit = (pop tail)
	while lit
	do (print-literal-node lit :s s :flip-negation (not reverse))
	   (when tail
	     (if reverse (format s " or ") (format s " and ")) ))
    ))

(defun print-clause-node-as-conjuncts (node &key (s t))
  (let ((lits (clause-literals node)))
    (print-literal-node (car lits) :s s)
    (dolist (lit (cdr lits))
      (format s " and ")
      (print-literal-node lit :s s) )))

;;;----------------------------------------------------------------------------

(defun refutation-clause-p (clause)
  (null (clause-literals clause)) )

(defun clause-goal (clause)
  (first (clause-literals clause)) )

(defun clause-remaining-literals (clause)
  (rest (clause-literals clause)) )

;;;----------------------------------------------------------------------------

(defun clause-plug (clause binding-list)
  "Return a new clause which is a copy of CLAUSE with BINDING-LIST applied"
  (make-clause-node
   :literals
   (remove-duplicates
    (mapcar
     #'(lambda (old-lit)
	 (literal-plug old-lit binding-list) )
     (clause-literals clause) )
    :from-end t :test #'literal-equal-p )
   :label (clause-label clause)
   ))

(defun nclause-plug (clause binding-list)
  "Destructively modify CLAUSE by applying BINDING-LIST"
  (mapc #'(lambda (old-lit) (nliteral-plug old-lit binding-list))
	(clause-literals clause) )
  (setf (clause-literals clause)
    (remove-duplicates
     (clause-literals clause)
     :from-end t :test #'literal-equal-p ))
  clause )

;;;----------------------------------------------------------------------------

(defun clause-remove (literal clause)
  "Return a new clause which is a copy of CLAUSE with LITERAL removed"
  (make-clause-node
   :literals (remove literal (clause-literals clause))
   :label (clause-label clause) ))

;;;----------------------------------------------------------------------------

(defun clause-rename-all-variables (clause &key (except nil))
  "Return a new clause and binding-list, copy of CLAUSE with vars renamed"
  (let ((bl (clause-rename-binding-list clause :except except)))
    (values (clause-plug clause bl) bl) ))

(defun nclause-rename-all-variables (clause &key (except nil))
  "Destructively modify CLAUSE by renaming all variables, return binding list"
  (let ((bl (clause-rename-binding-list clause :except except)))
    (nclause-plug clause bl)
    bl ))

(defun clause-rename-binding-list (clause &key (except nil))
  (let (orig-vars)
    (setq orig-vars (find-all-variables (clause-literals clause)))
    (if orig-vars
	(append
	 (loop
	     for ov in orig-vars
	     unless (find ov except)
	     collect (cons ov (make-new-variable ov)) )
	 '((t . t)) )
      '((t . t)) )
    ))

(defun find-all-variables (literal-list)
  (remove-duplicates
   (apply #'append
	  (mapcar #'(lambda (lit) (find-vars (literal-terms lit)))
		  literal-list ))
   :from-end t ))

;;;----------------------------------------------------------------------------

(defun clause-merge (lit1 c1 lit2 c2 binding-list)
  "Returns clause c1+c2-l1-l2"
  (let (literal-list)
    (setq literal-list
      (mapcar
       #'(lambda (old-lit) (literal-plug old-lit binding-list))
       (append (remove lit1 (clause-literals c1))
	       (remove lit2 (clause-literals c2)) )))
    (setq literal-list
      (remove-duplicates literal-list :from-end t :test #'literal-equal-p) )
    (make-clause-node
     :literals literal-list
     :label (label-and (clause-label c1) (clause-label c2)) )
    ))

;;;----------------------------------------------------------------------------

(defun clause-is-instance-of-subset-p (c-inst c-super &key (cdr nil))
  "True if every literal in C-INST is an instance of some literal in C-SUPER"
  (loop
      named instance-loop
      with bl = '((t . t))
      with super-lits =
	(if cdr (cdr (clause-literals c-super)) (clause-literals c-super))
      for c-super-list = (mapcar #'copy-literal-node super-lits)
      then (mapcar #'(lambda (x) (nliteral-plug x bl)) c-super-list)
      for lit1 in (clause-literals c-inst)
      finally (return-from instance-loop bl)
      unless (find lit1 c-super-list :test #'literal-equal-p)
      do (loop
	     named superset-loop
	     for lit2 in c-super-list
	     for new-bl = (literal-instance lit2 lit1 bl)
	     when new-bl
	     do (setq bl new-bl)
		(return-from superset-loop)
	     finally (return-from instance-loop nil) )
	 ))

;;;----------------------------------------------------------------------------

(defun clause-equal-p (clause1 clause2)
  (let ((cl1 (clause-literals clause1))
	(l1 (clause-label clause1))
	(cl2 (clause-literals clause2))
	(l2 (clause-label clause2)) )
    (and (or (null l1)
	     (null l2)
	     (and (equal (label-value l1) (label-value l2))
		  (eq (label-structure l1) (label-structure l2)) ))
	 (= (length cl1) (length cl2))
	 (loop
	     for lit1 in cl1
	     for lit2 in cl2
	     unless (literal-equal-p lit1 lit2)
	     return nil
	     finally (return t) ))
    ))

;;;----------------------------------------------------------------------------

(defun clause-to-list (clause)
  (loop
      for literal in (clause-literals clause)
      collect (literal-to-list literal) ))

;;;----------------------------------------------------------------------------

(defun clause-list-equal-p (clause list &key (test #'equal))
  (loop
      for literal in (clause-literals clause)
      for list-literal in list
      unless (literal-list-equal-p literal list-literal :test test)
      return nil
      finally (return t) ))

;;;----------------------------------------------------------------------------

(defun nclause-flip-negations (clause)
  (loop
      for literal in (clause-literals clause)
      do (setf (literal-negated-p literal) (not (literal-negated-p literal)))
      finally (return clause) ))

;;;----------------------------------------------------------------------------
