;;; -*- Mode: LISP; Syntax: Common-lisp; Package: DTP; Base: 10 -*-

;;;----------------------------------------------------------------------------
;;;
;;;	File		Output.Lisp
;;;	System		Don's Theorem Prover
;;;
;;;	Written by	Don Geddis (Geddis@CS.Stanford.Edu)

(in-package "DTP")

(eval-when (compile load eval)
  (export
   '(show settings possible-settings #+dtp-trace show-proof-graph) ))

;;;----------------------------------------------------------------------------
;;;
;;;	Public

;;;----------------------------------------------------------------------------

(defmethod show ((object symbol))
  (format t "~&")
  (dolist (node (theory-contents object))
    (format t "~A~15T" (kb-node-id node))
    (print-clause-node (kb-node-clause node) :as-rule t)
    (format t "~%") )
  (values) )

;;;----------------------------------------------------------------------------

(defun settings ()
  "Display (and someday change) all the user-setable variables"
  (let ((count 0))
    (dolist (section *user-setable-variables*)
      (format t "~A~%" (first section))
      (dolist (var (rest section))
	(format t "   ~2D: ~:(~A~)" (incf count) var)
	(if (atom (symbol-value var))
	    (format t "~40T ~S~%" (symbol-value var) )
	  (format t "~%       ~S~%" (symbol-value var)) )))
    (values) ))

(defun possible-settings ()
  "Display all the possible values for user-setable variables"
  (let ((count 0)
	possibles )
    (dolist (section *user-setable-variables*)
      (format t "~A~%" (first section))
      (dolist (var (rest section))
	(format t "   ~2D: ~:(~A~)" (incf count) var)
	(setq possibles (possible-values var))
	(cond
	 (possibles
	  (if (stringp possibles)
	      (format t "~40T ~A~%" possibles)
	    (format t "~%       ~:(~S~)~%" possibles) ))
	 ((integerp (symbol-value var))
	  (format t "~40T A non-negative integer~%") )
	 ((or (eq (symbol-value var) t)
	      (eq (symbol-value var) nil) )
	  (format t "~40T Nil or T~%") )
	 ((symbolp (symbol-value var))
	  (format t "~40T A symbol~%") )
	 (t
	  (format t "~40T [Unknown, but currently ~S]~%" (symbol-value var)) )
	 )))
    (values) ))

;;;----------------------------------------------------------------------------

#+dtp-trace
(defun show-proof-graph (&optional (*proof* *proof*))
  (format t "~%")
  (if (or (proof-subgoal-agenda *proof*) (proof-query-conjunctions *proof*))
      (format t "Proof tree [in progress] ")
    (format t "Complete proof tree ") )
  (format t "using theory ~A for query~%" (proof-theory *proof*))
  (format t "     ~A~%" (proof-query *proof*))
  (if (proof-answers *proof*)
      (format t "Found ~R answer~:P" (length (proof-answers *proof*)))
    (format t "Found no answers") )
  (when (or (proof-subgoal-agenda *proof*) (proof-query-conjunctions *proof*))
    (format t " so far") )
  (if (proof-answers *proof*)
      (format t ":~%")
    (format t ".~%") )
  (dolist (answer (proof-answers *proof*))
    (format t "     ~A"
	    (plug (proof-query *proof*) (answer-binding-list answer)) )
    (when (answer-label answer)
      (format t " with label ~A" (label-value (answer-label answer))) )
    (format t "~%") )
  (format t "~%")

  (let ((*proof-line-count* 0)
	(*depth* 0)
	(*subgoal-map* nil) )
    (dolist (conjunction (proof-used-conjunctions *proof*))
      (show-obj conjunction :fringe nil) )
    (dolist (conjunction (proof-query-conjunctions *proof*))
      (show-obj conjunction :fringe t) )
    )

  (format t "~%")
  (values) )

;;;----------------------------------------------------------------------------

#+dtp-trace
(defun indent-line (&key (number nil) (s *standard-output*))
  (when number
    (format s "~3,'0D  " (incf *proof-line-count*)) )
  (dotimes (i *depth*)
    (format s "..") ))

;;;----------------------------------------------------------------------------

(defun print-binding-list (binding-list &key (s t))
  (dolist (binding-pair binding-list)
    (unless (eq (binding-variable binding-pair) 't)
      (format s " ~A->" (variable-to-string (binding-variable binding-pair)))
      (term-to-string (binding-value binding-pair) s) )))

;;;----------------------------------------------------------------------------
;;;
;;;	Private

;;;----------------------------------------------------------------------------

#+dtp-trace
(defgeneric show-obj (object &key fringe &allow-other-keys))

;;;----------------------------------------------------------------------------

#+dtp-trace
(defmethod show-obj
    ((conjunction dtp-conjunction) &key (fringe nil) (recurse t))
  (with-slots (list binding-list label) conjunction
    (indent-line :number t)
    (format t "Conj: ")
    (cond
     ((null list)
      (format t "True") )
     (t
      (print-literal-node (slot-value (first list) 'literal))
      (dolist (conjunct (rest list))
	(format t " and ")
	(print-literal-node (slot-value conjunct 'literal)) )))
    (when binding-list
      (format t " with")
      (print-binding-list binding-list) )
    (when label (format t " with label ~A" (label-value label)))
    (when (and fringe
	       (or (null list)
		   (and list
			(eq (slot-value (first list) 'subgoal)
			    :uninitialized ))))
      (format t "~50T [Fringe]") )
    (format t "~%")
    (when recurse
      (dolist (conjunct list)
	(with-slots (subgoal used-subgoals) conjunct
	  (incf *depth*)
	  (dolist (m-subgoal used-subgoals)
	    (show-obj m-subgoal :fringe nil) )
	  (unless (eq subgoal :uninitialized)
	    (show-obj subgoal :fringe t) )
	  (decf *depth*) )))
    ))

;;;----------------------------------------------------------------------------

#+dtp-trace
(defmethod show-obj
    ((subgoal dtp-subgoal) &key (fringe nil) (tbl nil) (recurse t))
  (indent-line :number t)
  (format t "Subg: ")
  (if (assoc subgoal *subgoal-map*)
      (progn
	(format t "[#~3,'0D" (cdr (assoc subgoal *subgoal-map*)))
	(when tbl
	  (format t " with")
	  (print-binding-list tbl) )
	(format t "]~%")
	(return-from show-obj) )
    (push (cons subgoal *proof-line-count*) *subgoal-map*) )
  (with-slots (literal inferences used-inferences) subgoal
    (print-literal-node literal)
    (when (and fringe (eq inferences :uninitialized))
      (format t "~50T [Fringe]") )
    (format t "~%")
    (when recurse
      (incf *depth*)
      (dolist (conjunction used-inferences)
	(show-obj conjunction :fringe nil) )
      (unless (eq inferences :uninitialized)
	(dolist (conjunction inferences)
	  (show-obj conjunction :fringe t) ))
      (decf *depth*) )))

;;;----------------------------------------------------------------------------
