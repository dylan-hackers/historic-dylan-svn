;;; -*- Mode: LISP; Syntax: Common-lisp; Package: DTP; Base: 10 -*-

;;;----------------------------------------------------------------------------
;;;
;;;	File		Textify.Lisp
;;;	System		Don's Theorem Prover
;;;
;;;	Written by	Don Geddis (Geddis@CS.Stanford.Edu)
;;;
;;;	Note		This entire file is conditional on #+dtp-trace

(in-package "DTP")

;;;----------------------------------------------------------------------------

(defgeneric textify (object &key answer &allow-other-keys))

;;;----------------------------------------------------------------------------

(defmethod textify ((object proof) &key &allow-other-keys)
  (format *show-stream* "Proof of ~A:~%:" (proof-query object)) )

;;;----------------------------------------------------------------------------

(defmethod textify ((object answer) &key &allow-other-keys)
  (format *show-stream* "~A:~%" (apply-answer object)) )

;;;----------------------------------------------------------------------------

(defmethod textify ((object dtp-subgoal) &key (answer nil) &allow-other-keys)
  (unless answer
    (indent-line :s *show-stream*)
    (print-literal-node (slot-value object 'literal))
    (format *show-stream* "~%")
    (return-from textify) )
  (indent-line :s *show-stream*)
  (print-literal-node
   (literal-plug (slot-value object 'literal) (answer-binding-list answer))
   :s *show-stream* )
  (dolist (bl (answer-ae-binding-lists answer))
    (format *show-stream* " or ")
    (print-literal-node
     (literal-plug (slot-value object 'literal) bl) :s *show-stream* ))
  (format *show-stream* "~%") )

;;;----------------------------------------------------------------------------

(defmethod textify
    ((object dtp-conjunction) &key (answer nil) &allow-other-keys)
  (unless answer (return-from textify))
  (let (lookup-justifications)
    (setq lookup-justifications
      (remove-if-not #'l-justification-p (answer-justification answer)) )
    (dolist (l-just lookup-justifications)
      (indent-line :s *show-stream*)
      (format *show-stream* "~:(~A~): " (l-just-id l-just))
      (print-clause-node-as-rule
       (kb-node-clause (find-kb-node-with-id (l-just-id l-just)))
       :s *show-stream* )
      (format *show-stream* "~%") )))

;;;----------------------------------------------------------------------------

(defmethod textify ((object dtp-conjunct) &key (answer nil) &allow-other-keys)
  (declare (ignore answer)) )

;;;----------------------------------------------------------------------------

(defmethod textify ((object r-justification) &key &allow-other-keys)
  (indent-line :s *show-stream*)
  (format *show-stream* "Reduction with ~A~%"
	  (slot-value (r-just-ancestor-subgoal object) 'literal) ))

;;;----------------------------------------------------------------------------

(defmethod textify ((object res-justification) &key &allow-other-keys)
  (indent-line :s *show-stream*)
  (format *show-stream* "Residue of ~A~%" (res-just-assumable object)) )

;;;----------------------------------------------------------------------------

(defmethod textify ((object l-justification) &key &allow-other-keys)
  (indent-line :s *show-stream*)
  (format *show-stream* "Lookup of ~:(~A~): " (l-just-id object))
  (let ((node (find-kb-node-with-id (l-just-id object))))
    (when (null node)
      (dolist (gn (proof-goal-nodes *proof*))
	(when (eq (l-just-id object) (kb-node-id gn))
	  (setq node gn) )))
    (when node
      (print-clause-node-as-rule (kb-node-clause node) :s *show-stream*)
      ))
  (format *show-stream* "~%") )

;;;----------------------------------------------------------------------------

(defmethod textify ((object c-justification) &key &allow-other-keys)
  "No output for a c-justification, because nodes below"
  nil )

;;;----------------------------------------------------------------------------

(defmethod textify ((object s-cache-justification) &key &allow-other-keys)
  (indent-line :s *show-stream*)
  (format *show-stream* "Success cache lookup of ")
  (print-literal-node (s-cache-just-literal object) :s *show-stream*)
  (format *show-stream* "~%") )

;;;----------------------------------------------------------------------------
(defmethod textify ((object f-cache-justification) &key &allow-other-keys)
  (indent-line :s *show-stream*)
  (format *show-stream* "Failure cache lookup of ")
  (print-literal-node (f-cache-just-literal object) :s *show-stream*)
  (format *show-stream* "~%") )

;;;----------------------------------------------------------------------------
(defmethod textify ((object sg-cache-justification) &key &allow-other-keys)
  (indent-line :s *show-stream*)
  (format *show-stream* "Subgoal cache lookup of ")
  (print-literal-node
   (slot-value (sg-cache-just-subgoal object) 'literal) :s *show-stream* )
  (format *show-stream* "~%") )

;;;----------------------------------------------------------------------------

(defmethod textify ((object justification) &key &allow-other-keys)
  (indent-line :s *show-stream*)
  (format *show-stream* "Unknown justification: ~A~%" object) )

;;;----------------------------------------------------------------------------
