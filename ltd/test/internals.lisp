;;; -*- Mode: LISP; Syntax: Common-lisp; Package: DTP; Base: 10 -*-

;;;----------------------------------------------------------------------------
;;;
;;;	File		Internals.Lisp
;;;	System		Don's Theorem Prover
;;;
;;;	Written by	Don Geddis (Geddis@CS.Stanford.Edu)

(in-package "DTP")

(eval-when (compile load eval)
  (export
   '(reset-dtp) ))

;;;----------------------------------------------------------------------------

(defun reset-dtp (&key (only-internal nil))
  (mapc #'unintern *all-gensymed-variables*)
  (reset-variables :only-internal only-internal)
  (reset-hierarchy)
  (reset-database) )

;;;----------------------------------------------------------------------------

(defmacro add-to-end (item list)
  `(if ,list
       (rplacd (last ,list) (list ,item))
     (setf ,list (list ,item)) ))

(defmacro add-to-end-if-new (item list)
  "If ITEM is not already on LIST, add it to the end"
  `(unless (find ,item ,list)
     (add-to-end ,item ,list) ))

(defmacro add-new-to-beginning (item list)
  "Remove ITEM from LIST and push it on the front"
  `(setf ,list (cons ,item (remove ,item ,list))) )

;;;----------------------------------------------------------------------------

(defun find-vars (list)
  (cond
   ((consp list)
    (append (find-vars (car list))
	    (find-vars (cdr list)) ))
   ((varp list)
    (list list) )))

(defun binding-list-vars (binding-list)
  (remove-duplicates
   (loop
       for binding in binding-list
       collect (car binding)
       collect (cdr binding) )))

;;;----------------------------------------------------------------------------

(defun make-new-variable (var)
  (let (var-name)
    (setq var-name
      (subseq (symbol-name var) 0 (position #\_ (symbol-name var))) )
    (setq var-name (concatenate 'string var-name "_"))
    (push (gentemp var-name) *all-gensymed-variables*)
    (first *all-gensymed-variables*) ))

;;;----------------------------------------------------------------------------

(defun variable-to-string (var)
  "Simplify unless *SHOW-RENAMED-VARIABLES*"
  (if *show-renamed-variables*
      (string var)
    (subseq (symbol-name var)
	    0 (position #\_ (symbol-name var)) )))

;;;----------------------------------------------------------------------------

(defun merge-binding-lists (binding-lists)
  "Merge with a Ginsberg trick: unify variable list with value list"
  (let (bindings new-bl)
    (setq bindings
      (reduce
       #'append
       (mapcar #'(lambda (bl) (remove t bl :key #'binding-variable))
	       binding-lists )))
    (setq new-bl
      (dtp-unifyp
       (mapcar #'binding-variable bindings)
       (mapcar #'binding-value bindings) ))
    (if new-bl
	(dtp-ify-binding-list new-bl)
      :not-a-binding-list )))

(defun dtp-ify-binding-list (binding-list)
  "Eliminate Ginsberg's (T . T) for success"
  (remove t binding-list :key #'binding-variable) )

;;;----------------------------------------------------------------------------

(defun unify-collection (sexp &rest more-sexps)
  (if more-sexps
      (let ((other-sexp (apply #'unify-collection more-sexps))
	    bl )
	(setq bl (dtp-unifyp sexp other-sexp))
	(when bl (plug sexp bl)) )
    sexp ))

;;;----------------------------------------------------------------------------

(defun make-new-id (str &optional (num nil))
  (if num
      (intern (format nil "~A-~D" str num) *dtp-package*)
    (intern
     (format nil "~A-~D"
	     str (incf *node-id-count*) )
     *dtp-package* )))

;;;----------------------------------------------------------------------------

(defun list-rename-variables (list)
  (let ((vars (find-vars list))
	bl )
    (setq bl (mapcar #'(lambda (x) (cons x (make-new-variable x))) vars))
    (plug list bl) ))

;;;----------------------------------------------------------------------------

(defun permutations (list-of-items)
  (if (= (length list-of-items) 1)
      (list list-of-items)
    (loop
	for item in list-of-items
	for remaining = (remove item list-of-items :test #'equal)
	appending
	  (loop
	      for perm in (permutations remaining)
	      collect (cons item perm) ))
    ))

;;;----------------------------------------------------------------------------

(defun tree-find (item tree)
  (cond
   ((or (null tree) (atom tree))
    nil )
   ((find item tree)
    t )
   (t
    (some #'(lambda (x) (tree-find item x)) tree) )))

;;;----------------------------------------------------------------------------

(defun partition (list test)
  "Return list with items satisfying TEST first, others following"
  (let (best others)
    (setq best (remove-if-not test list))
    (setq others (remove-if test list))
    (append best others) ))

;;;----------------------------------------------------------------------------

(defun tree-depth (list &optional (prior-depth 0))
  "Maximum depth of list structure"
  (if (consp list)
      (reduce
       #'max
       (mapcar #'(lambda (l) (tree-depth l (1+ prior-depth))) list) )
    prior-depth ))

;;;----------------------------------------------------------------------------

(defun set-equal (set-1 set-2 &optional (test #'equal))
  (not (set-exclusive-or set-1 set-2 :test test)) )

;;;----------------------------------------------------------------------------

(defun list-of-length-one-p (list)
  #+dtp-types (declare (type list list))
  (and (first list) (null (rest list))) )

(defun list-of-length-more-than-one? (list)
  #+dtp-types (declare (type list list))
  (rest list) )

;;;----------------------------------------------------------------------------
