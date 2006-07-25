;;; -*- Mode: LISP; Syntax: Common-lisp; Package: DTP; Base: 10 -*-

;;;----------------------------------------------------------------------------
;;;
;;;	File		Database.Lisp
;;;	System		Don's Theorem Prover
;;;
;;;	Written by	Don Geddis (Geddis@CS.Stanford.Edu)

(in-package "DTP")

(eval-when (compile load eval)
  (export
   '(empty-theory
     make-theory-from-sentences
     save-sentence-in-theory drop-sentence-from-theory
     sentences-in )))

;;;----------------------------------------------------------------------------

(defvar *kb* nil "List of THEORY structures")
(defvar *id-index* (make-hash-table :test #'eq))

;;;----------------------------------------------------------------------------

(defstruct (theory
	    (:print-function theory-print-function) )
  name
  (nodes nil)
  (indexed-nodes nil) )

(defstruct (node-index
	    (:print-function node-index-print-function) )
  key
  (nodes nil) )

;;;----------------------------------------------------------------------------

(defun theory-print-function (structure stream depth)
  (declare (ignore depth))
  (format stream "<Theory ~A with ~D node~:P>"
	  (theory-name structure) (length (theory-nodes structure)) ))

(defun node-index-print-function (structure stream depth)
  (declare (ignore depth))
  (format stream "<Index ~A with ~D node~:P>"
	  (node-index-key structure) (length (node-index-nodes structure)) ))

;;;----------------------------------------------------------------------------

(defun reset-database ()
  (setq *id-index* (make-hash-table :test #'eq))
  (setq *kb* nil) )

;;;----------------------------------------------------------------------------

(defun empty-theory (theory-name)
  (let ((theory (get-theory-structure theory-name)))
    (when theory
      (dolist (node (theory-nodes theory))
	(unindex-id (kb-node-id node)) )
      (setq *kb* (remove theory *kb*))
      theory-name )))

;;;----------------------------------------------------------------------------

(defun all-kb-theories ()
  (mapcar #'theory-name *kb*) )

;;;----------------------------------------------------------------------------

(defun get-theory-structure (theory-name)
  (find theory-name *kb* :key #'theory-name) )

;;;----------------------------------------------------------------------------

(defun active-theory-contents (theory index-on)
  (loop
      for theory-name in (included-active-theory-names theory)
      append (theory-contents theory-name index-on) ))

;;;----------------------------------------------------------------------------

(defun theory-contents (theory-name &optional (index nil))
  (let ((theory (get-theory-structure theory-name)))
    (cond
     ((null theory)
      nil )
     (index
      (let (ni)
	(setq ni
	  (find index (theory-indexed-nodes theory) :key #'node-index-key) )
	(when ni (node-index-nodes ni)) ))
     (t
      (theory-nodes theory) ))
    ))

;;;----------------------------------------------------------------------------

(defun make-theory-from-nodes (nodes theory-name)
  (empty-theory theory-name)
  (add-to-end
   (make-theory
    :name theory-name :nodes nodes :indexed-nodes (make-index nodes) )
   *kb* )
  (mapc #'(lambda (node) (index-id (kb-node-id node) node))
	nodes )
  theory-name )

;;;----------------------------------------------------------------------------

(defun save-node-in-theory (node theory-name)
  (let ((old-theory (get-theory-structure theory-name)))
    (if old-theory
	(progn
	  (setf (theory-indexed-nodes old-theory)
	    (make-index (list node) (theory-indexed-nodes old-theory)) )
	  (add-to-end node (theory-nodes old-theory))
	  (index-id (kb-node-id node) node) )
      (make-theory-from-nodes (list node) theory-name) )
    (kb-node-id node) ))

;;;----------------------------------------------------------------------------

(defun drop-node-from-theory (node theory-name)
  (unindex-id (kb-node-id node))
  (setf (theory-nodes theory-name) (remove node (theory-nodes theory-name)))
  (dolist (index (theory-indexed-nodes theory-name))
    (setf (node-index-nodes index)
      (remove node (node-index-nodes index)) ))
  (setf (theory-indexed-nodes theory-name)
    (remove-if-not #'node-index-nodes (theory-indexed-nodes theory-name)) )
  (kb-node-id node) )

;;;----------------------------------------------------------------------------

(defun make-index (nodes &optional (indices nil))
  "Return a list of index nodes with a key for each relation in NODES"
  (loop
      for node in nodes
      do (loop
	     for literal in (clause-literals (kb-node-clause node))
	     for relation = (literal-relation literal)
	     for index = (find relation indices :key #'node-index-key)
	     do (if index
		    (add-to-end-if-new node (node-index-nodes index))
		  (add-to-end
		   (make-node-index :key relation :nodes (list node))
		   indices )))
      finally (return indices) ))

;;;----------------------------------------------------------------------------

(defun find-kb-node-with-id (id)
  (gethash id *id-index*) )

(defun index-id (id node)
  (when (gethash id *id-index*)
    (error "Trying to remap id ~A from ~A to ~A"
	   id (gethash id *id-index*) node ))
  (setf (gethash id *id-index*) node) )

(defun unindex-id (id)
  (remhash id *id-index*) )

;;;----------------------------------------------------------------------------

(defun last-id-count (theory-name)
  (let ((theory (get-theory-structure theory-name))
	id str )
    (cond
     (theory
      (setq id (kb-node-id (first (last (theory-nodes theory)))))
      (setq str (symbol-name id))
      (setq str (subseq str (1+ (length (symbol-name theory-name)))))
      (values (read-from-string str)) )
     (t
      0 ))))	

;;;----------------------------------------------------------------------------
;;;
;;;	Logic -> Nodes

;;;----------------------------------------------------------------------------

(defun make-theory-from-sentences (theory-name sentence-label-pairs)
  (let (cnf-label-pairs literal-lists nodes)
    (setq cnf-label-pairs
      (mapcar #'(lambda (slp)
		  (let ((label (cdr slp)))
		    (mapcar #'(lambda (s) (cons s label))
			    (sentence-to-cnf (car slp)) )))
	      sentence-label-pairs ))
    (setq cnf-label-pairs (apply #'append cnf-label-pairs))
    (setq literal-lists
      (mapcar #'(lambda (x)
		  (list (mapcar #'list-to-literal (car x)) (cdr x)) )
	      cnf-label-pairs ))
    (setq nodes
      (loop
	  for (literal-list label) in literal-lists
	  for count from 1
	  collect
	    (make-kb-node
	     :id (make-new-id theory-name count)
	     :clause (make-clause-node :literals literal-list :label label) )))
    (make-theory-from-nodes nodes theory-name)
    theory-name ))

;;;----------------------------------------------------------------------------

(defun save-sentence-in-theory
    (sentence &key (theory-name *theory*) (label nil))
  (loop
      with cnf = (sentence-to-cnf sentence)
      with literal-lists =
	(mapcar #'(lambda (x) (mapcar #'list-to-literal x)) cnf)
      for literal-list in literal-lists
      for count from (1+ (last-id-count theory-name))
      for id = (make-new-id theory-name count)
      for new-node =
	(make-kb-node
	 :id id
	 :clause (make-clause-node :literals literal-list :label label) )
      collect id
      do (save-node-in-theory new-node theory-name) ))

;;;----------------------------------------------------------------------------

(defun drop-sentence-from-theory
    (sentence &key (theory-name *theory*) (test #'equal))
  "Locate the node(s) in the theory corresponding to SENTENCE and remove them"
  (loop
      with theory = (get-theory-structure theory-name)
      while theory
      for dnf in (sentence-to-cnf sentence)
      collect
	(loop
	    for node in (theory-nodes theory)
	    when (clause-list-equal-p (kb-node-clause node) dnf :test test)
	    do (drop-node-from-theory node theory)
	       (return (kb-node-id node)) )
      into success
      finally (return (remove nil success)) ))

;;;----------------------------------------------------------------------------

(defun sentences-in (theory-name &key (with-atom nil))
  "Return list of sentences in theory named THEORY-NAME"
  (loop
      for node in (theory-contents theory-name)
      for clause = (kb-node-clause node)
      collect (simplify-dnf (cons 'or (clause-to-list clause)))
      into sentences
      finally
	(if with-atom
	    (return
	      (remove-if-not
	       #'(lambda (x) (tree-find with-atom x))
	       sentences ))
	  (return sentences) )))

;;;----------------------------------------------------------------------------
