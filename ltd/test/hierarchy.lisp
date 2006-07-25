;;; -*- Mode: LISP; Syntax: Common-lisp; Package: DTP; Base: 10 -*-

;;;----------------------------------------------------------------------------
;;;
;;;	File		Hierarchy.Lisp
;;;	System		Don's Theorem Prover
;;;
;;;	Written by	Don Geddis (Geddis@CS.Stanford.Edu)

(in-package "DTP")

(eval-when (compile load eval)
  (export
   '(includes unincludes includees decludes
     included-active-theory-names
     show-theory-dag all-theories )))

;;;----------------------------------------------------------------------------

(defvar *include-maps* nil "List of INCLUDE-MAP structures")

;;;----------------------------------------------------------------------------

(defstruct (include-map
	    (:print-function include-map-print-function) )
  theory-name
  included-names )

;;;----------------------------------------------------------------------------

(defun include-map-print-function (structure stream depth)
  (declare (ignore depth))
  (format stream "<Theory ~A includes ~A>"
	  (include-map-theory-name structure)
	  (include-map-included-names structure) ))

;;;----------------------------------------------------------------------------

(defun reset-hierarchy ()
  (setf *include-maps* nil) )

;;;----------------------------------------------------------------------------

(defun get-im-structure (theory-name)
  #+dtp-types (declare (type symbol theory-name))
  (find theory-name *include-maps* :key #'include-map-theory-name) )

;;;----------------------------------------------------------------------------

(defun includes (theory-name-1 theory-name-2)
  #+dtp-types (declare (type symbol theory-name-1 theory-name-2))
  (let ((map (get-im-structure theory-name-1)))
    (if map
	(add-to-end-if-new theory-name-2 (include-map-included-names map))
      (add-to-end
       (setq map
	 (make-include-map
	  :theory-name theory-name-1 :included-names (list theory-name-2) ))
       *include-maps* ))
    (include-map-included-names map) ))

;;;----------------------------------------------------------------------------

(defun unincludes (theory-name-1 theory-name-2)
  #+dtp-types (declare (type symbol theory-name-1 theory-name-2))
  (let ((map (get-im-structure theory-name-1)))
    (when map
      (setf (include-map-included-names map)
	(remove theory-name-2 (include-map-included-names map)) )
      (unless (include-map-included-names map)
	(setf *include-maps* (remove map *include-maps*)) )
      't )))

;;;----------------------------------------------------------------------------

(defun includees (theory-name)
  #+dtp-types (declare (type symbol theory-name))
  (let ((map (get-im-structure theory-name)))
    (when map (include-map-included-names map)) ))

;;;----------------------------------------------------------------------------

(defun decludes (theory-name)
  #+dtp-types (declare (type symbol theory-name))
  (mapc #'(lambda (x) (unincludes theory-name x))
	(includees theory-name) ))

;;;----------------------------------------------------------------------------

(defun included-active-theory-names (theory-name)
  #+dtp-types (declare (type symbol theory-name))
  (loop
      for remaining-names = (list theory-name)
      then (rest remaining-names)
      for name = (first remaining-names)
      until (null remaining-names)
      unless (find name all-names)
      collect name into all-names
      and do (setq remaining-names (append remaining-names (includees name)))
      finally (return all-names) ))

;;;----------------------------------------------------------------------------

(defun all-include-theories ()
  (union
   (mapcar #'include-map-theory-name *include-maps*)
   (reduce #'append
	   (mapcar #'include-map-included-names *include-maps*) )))

;;;----------------------------------------------------------------------------

(defun show-theory-dag ()
  (let (children roots)
    #+dtp-types (declare (type list children roots))
    (setq children (mapcar #'include-map-included-names *include-maps*))
    (when children (setq children (reduce #'union children)))
    (setq roots (remove-if #'(lambda (x) (find x children)) (all-theories)))
    (unless roots (setq roots (all-theories)))
    (dolist (root roots)
      (show-theory-dag-internal root 0 nil) )
    (values) ))

(defun show-theory-dag-internal (name depth already-seen)
  #+dtp-types (declare (type symbol name))
  #+dtp-types (declare (type (integer 0 *) depth))
  (tab-to depth)
  (format t "~:(~A~)" name)
  (when (eq name *theory*)
    (format t "~20T[Active]") )
  (format t "~%")
  (loop
      with children = (includees name)
      with new-seen = (union children already-seen)
      with new-depth = (1+ depth)
      for child in children
      do (show-theory-dag-internal child new-depth new-seen) ))

(defun tab-to (column)
  (dotimes (col (* 3 column))
    (format t " ") ))

;;;----------------------------------------------------------------------------

(defun all-theories ()
  (let (theories)
    (setq theories (all-kb-theories))
    (setq theories (append theories (all-include-theories)))
    (when (boundp '*proof*)
      (setq theories (cons (proof-theory *proof*) theories)) )
    (remove-duplicates theories) ))

;;;----------------------------------------------------------------------------
