;;;; -*- mode:Lisp; syntax:Common-Lisp; package:user -*- ;;;;
;;;; Copyright 1992 Patrick H. Winston.  All rights reserved.
;;;; Version 1.1.1, copied from master file on 23 Apr 93       
;;;; 
;;;; This software is licensed by Patrick H. Winston (licensor) for
;;;; instructional use with the textbooks ``Artificial Intelligence,'' by
;;;; Patrick H. Winston, and ``Lisp,'' by Patrick H. Winston and Berthold
;;;; K. P. Horn.  Your are free to make copies of this software and
;;;; modify it for such instructional use as long as:
;;;; 1. You keep this notice intact.
;;;; 2. You cause any modified files to carry a prominent notice stating
;;;;    that you modified the files and the date of your modifications.
;;;; This software is licensed ``AS IS'' without warranty and the licensor
;;;; shall have no liability for any alleged defect or damages.

;;;; DYNAMIC VARIABLES, CONSTANTS, AND PROCEDURE-IMPLEMENTED TABLES

(defvar *intervals*)

(defconstant *forward-labels*
  '(before during starts finishes meets overlaps =))

(defconstant *inverse-labels*
  '(beforei duringi startsi finishesi meetsi overlapsi))

(defconstant *labels* (append *forward-labels* *inverse-labels*)
  "
  Purpose:	List of allowed time labels.
  "
)

(defun fetch-inverse (label)
  "
  Purpose:	Table of time-relation inverses.
  "
  (case label
    (= '=)
    (before 'beforei)		(beforei 'before)
    (during 'duringi)		(duringi 'during)
    (starts 'startsi)		(startsi 'starts)
    (finishes 'finishesi)	(finishesi 'finishes)
    (meets 'meetsi)		(meetsi 'meets)
    (overlaps 'overlapsi)	(overlapsi 'overlaps)))


(defun fetch-allowed-labels (x-to-y-label y-to-z-label)
  "
  Purpose:	Table of time constraints.
  Arguments:	A lable from x to y and a label from y to z
  Returns:	List of allowed spanning labels from x to z,
		eg. '(before).
  Remarks:	This table has been copied manually from a
		published paper and then copied manually again;
		do not use it to run your nuclear power plant!

		Also, The use of EXPAND-LABEL reduces the number
		of characters typed, thus reducing the probability 
		of typing errors.  Recode if you are interested
		in speed.
  "
  (mapcar #'expand-label
	  (case x-to-y-label
	    (before (case y-to-z-label		     (= *labels*)
		      (before '(b))		     (meets '(b))
		      (overlaps '(b))		     (starts '(b))
		      (during '(b m o s d))	     (finishes '(b m o s d))
		      (beforei *labels*)	     (meetsi '(b m o s d))
		      (overlapsi '(b m o s d))	     (startsi '(b))
		      (duringi '(b))		     (finishesi '(b))))
	    (meets (case y-to-z-label		     (= *labels*)
		     (before '(b))		     (meets '(b))
		     (overlaps '(b))		     (starts '(m))
		     (during '(o s d))		     (finishes '(o s d))
		     (beforei '(bi mi oi si di))     (meetsi '(f fi =))
		     (overlapsi '(o s d))	     (startsi '(m))
		     (duringi '(b))		     (finishesi '(b))))
	    (overlaps (case y-to-z-label	     (= *labels*)
			(before '(b))		     (meets '(b))
			(overlaps '(b m o))	     (starts '(o))
			(during '(o s d))	     (finishes '(o s d))
			(beforei '(bi mi oi si di))  (meetsi '(oi si di))
			(overlapsi '(o s d f oi si di fi =))
						     (startsi '(o di fi))
			(duringi '(b m o di fi))     (finishesi '(b m o))))
	    (starts (case y-to-z-label		     (= *labels*)
		      (before '(b))		     (meets '(b))
		      (overlaps '(b m o))	     (starts '(s))
		      (during '(d))		     (finishes '(d))
		      (beforei '(b))		     (meetsi '(m))
		      (overlapsi '(d f oi))	     (startsi '(s si =))
		      (duringi '(b m o di fi))	     (finishesi '(b m o))))
	    (during (case y-to-z-label		     (= *labels*)
		      (before '(b))		     (meets '(b))
		      (overlaps '(b m o s d))	     (starts '(d))
		      (during '(d))		     (finishes '(d))
		      (beforei '(bi))		     (meetsi '(bi))
		      (overlapsi '(d f bi mi oi))    (startsi '(d f bi mi oi))
		      (duringi *labels*)	     (finishesi '(b m o s d))))
	    (finishes (case y-to-z-label	     (= *labels*)
			(before '(b))		     (meets '(m))
			(overlaps '(o s d))	     (starts '(d))
			(during '(d))		     (finishes '(f))
			(beforei '(bi))		     (meetsi '(bi))
			(overlapsi '(bi mi oi))	     (startsi '(bi mi oi))
			(duringi '(bi mi oi si di))  (finishesi '(f fi =))))
	    (beforei (case y-to-z-label		     (= *labels*)
		       (before *labels*)             (meets '(d f bi mi oi))
		       (overlaps '(d f bi mi oi))    (starts '(d f bi mi oi))
		       (during '(d f bi mi oi))	     (finishes '(bi))
		       (beforei '(bi))		     (meetsi '(bi))
		       (overlapsi '(bi))	     (startsi '(bi))
		       (duringi '(bi))		     (finishesi '(bi))))
	    (meetsi (case y-to-z-label		     (= *labels*)
		      (before '(b o m di fi))	     (meets '(s si =))
		      (overlaps '(d f oi))	     (starts '(d f oi))
		      (during '(d f oi))	     (finishes '(mi))
		      (beforei '(bi))		     (meetsi '(bi))
		      (overlapsi '(bi))		     (startsi '(bi))
		      (duringi '(bi))		     (finishesi '(mi))))
	    (overlapsi (case y-to-z-label	     (= '(bi mi oi))
			 (before '(b m o di fi))     (meets '(o di fi))
			 (overlaps '(o s d f oi si di fi =))
						     (starts '(d f oi))
			 (during '(d f oi))	     (finishes '(oi))
			 (beforei '(bi))	     (meetsi '(bi))
			 (overlapsi '(bi mi oi))     (startsi '(bi mi oi))
			 (duringi '(bi m oi si di))  (finishesi '(oi si di))))
	    (startsi (case y-to-z-label		     (= *labels*)
		       (before '(b m o di fi))	     (meets '(o di fi))
		       (overlaps '(o di fi))	     (starts '(s si =))
		       (during '(d f oi))	     (finishes '(oi))
		       (beforei '(bi))		     (meetsi '(mi))
		       (overlapsi '(oi))	     (startsi '(si))
		       (duringi '(di))		     (finishesi '(di))))
	    (duringi (case y-to-z-label		     (= *labels*)
		       (before '(b m o di fi))	     (meets '(o di fi))
		       (overlaps '(o di fi))	     (starts '(o di fi))
		       (during '(o s d f oi si di fi =))
						     (finishes '(oi di si))
		       (beforei '(bi mi oi di si))   (meetsi '(oi si di))
		       (overlapsi '(oi si di))	     (startsi '(di))
		       (duringi '(di))		     (finishesi '(di))))
	    (finishesi (case y-to-z-label	     (= *labels*)
			 (before '(b))		     (meets '(m))
			 (overlaps '(o))	     (starts '(o))
			 (during '(o s d))	     (finishes '(f fi =))
			 (beforei '(bi mi oi si di)) (meetsi '(oi si di))
			 (overlapsi '(oi si di))     (startsi '(di))
			 (duringi '(di))	     (finishesi '(fi))))
	    (= (case y-to-z-label		     (= *labels*)
		 (before *labels*)		     (meets *labels*)
		 (overlaps *labels*)		     (starts *labels*)
		 (during *labels*)		     (finishes *labels*)
		 (beforei *labels*)		     (meetsi *labels*)
		 (overlapsi *labels*)		     (startsi *labels*)
		 (duringi *labels*)		     (finishesi *labels*))))))

(defun expand-label (label)
  "
  Purpose:	Makes it easier to type constraint table (fewer characters).
  "
  (case label
    (b 'before)     (m 'meets)
    (o 'overlaps)   (s 'starts)
    (d 'during)     (f 'finishes)
    (bi 'beforei)   (mi 'meetsi)
    (oi 'overlapsi) (si 'startsi)
    (di 'duringi)   (fi 'finishesi)
    (t label)))

;;;; ACCESS PROCEDURES

;; Labels are kept on property lists:

(defun read-labels (interval) (get interval 'labels))

(defun write-labels (interval labels) 
  (setf (get interval 'labels) labels))

(defun clear-labels (interval) 
  (setf (get interval 'labels) nil))

;; Each label list is an association list keyed by the target interval:

(defun get-surviving-labels (n1 n2)
  "
  Purpose:	Pick target interval's labels off of association list:
  Arguments:	Source interval and target interval.
  Returns:	List of labels, such as (meets before).
  "
  (rest (get-surviving-label-list n1 n2)))

(defun get-surviving-label-list (n1 n2)
  "
  Purpose:	Helps pick target interval's labels off of association list:
  Returns:	Key plus list of labels, such as (Y meets before).
  Remarks:	Association list for X might look like this:
		((Y meets before) (Z during))
  "
  (let* ((all-n1-labels (read-labels n1))
	 (all-n2-labels (read-labels n2))
	 (n1-to-n2-labels (assoc n2 all-n1-labels))
	 (n2-to-n1-labels (assoc n1 all-n2-labels)))
    (if n1-to-n2-labels
	n1-to-n2-labels
      (progn
	;;If a interval is not on the association list yet, put it there:
	(write-labels n1 (cons (cons n2 *labels*) all-n1-labels))
	(write-labels n2 (cons (cons n1 *labels*) all-n2-labels))
	;;And do it again:
	(get-surviving-label-list n1 n2)))))

;; Create Intervals:

(defmacro make-intervals (&rest intervals)
  `(progn
     (setf *intervals* ',intervals)
     (dolist (n *intervals*) (clear-labels n))
     *intervals*))

;;;; LABEL ESTABLISHING PROCEDURS

(defmacro set-labels (n1 label-or-labels n2)
  "
  Purpose:	Enables specification of labels without quoting.
  Arguments:	Source interval, label or label list, target interval.
  "
  (if (listp label-or-labels)
      `(set-surviving-labels ',n1 ',label-or-labels ',n2)
      `(set-surviving-labels ',n1 (list ',label-or-labels) ',n2)))

(defun set-surviving-labels (n1 labels n2)
  "
  Purpose:	Installs labels on the association lists of both n1 and n2.
  Remarks:	Initiates propagation to other intervals possibly effected.
  "
  ;;Use generalized assignment primitive, SETF, to replace label list:
  (let ((n1-to-n2 (setf (rest (get-surviving-label-list n1 n2)) labels))
	(n2-to-n1 (setf (rest (get-surviving-label-list n2 n1))
			(mapcar #'fetch-inverse labels))))
    ;;Check to see which label set has fewer inverse labels;
    ;;then print that set:
    (if (< (length (remove-if #'(lambda (x) (member x *forward-labels*))
			      n1-to-n2))
	   (length (remove-if #'(lambda (x) (member x *forward-labels*))
			      n2-to-n1)))
	(format t "~%~a to ~a label list set to ~a" n1 n2 n1-to-n2)	
      (format t "~%~a to ~a label list set to ~a" n2 n1 n2-to-n1)))
  ;;Propagate:
  (format t "~%Attempting to propagate...")
  (dolist (n3 (remove n2 (remove n1 *intervals*)))
    (constrain n1 n2 n3))
  (dolist (n0 (remove n2 (remove n1 *intervals*)))
    (constrain n2 n1 n0)))

;;;; LABEL CONSTRAINING PROCEDURE

(defun constrain (n1 nx n2)
  "
  Purpose:	The key constraining procedure.
  Remarks:	Uses n1 to nx labels plus nx to n2 labels
		to constrain n1 to n2 labels.
  "
  (let ((n1-to-nx-labels (get-surviving-labels n1 nx))
	(nx-to-n2-labels (get-surviving-labels nx n2))
	(old-n1-to-n2-labels (get-surviving-labels n1 n2))
	(new-n1-to-n2-labels nil)
	(allowed-labels nil))
    ;;Gather labels allowed by ANY n1-nx nx-n2 pair:
    (dolist (n1-to-nx-label n1-to-nx-labels)
      (dolist (nx-to-n2-label nx-to-n2-labels)
	(setf allowed-labels
	      (union allowed-labels
		     (fetch-allowed-labels n1-to-nx-label nx-to-n2-label)))))
    ;;Eliminate any existing label that is NOT allowed: 
    (setf new-n1-to-n2-labels
	  (intersection old-n1-to-n2-labels allowed-labels))
    ;;If any are eliminated, then reset the label list:
    (when (> (length old-n1-to-n2-labels)
	     (length new-n1-to-n2-labels))
      (set-surviving-labels n1 new-n1-to-n2-labels n2))))


