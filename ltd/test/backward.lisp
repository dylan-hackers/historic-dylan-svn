;;;;; -*- mode:Lisp; syntax:Common-Lisp; package:user -*- ;;;;
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

;;;; SPECIAL VARIABLES

(defvar *assertions*)

(defvar *rules*)

;;;; BACKWARD CHAINING

(defun chain (&rest patterns)
  "
  Purpose:	Initiate backward chaining.
  "
  (let ((binding-stream
          (apply-filters patterns
                         (stream-cons nil 'empty-stream)))
        (variables (list-variables patterns))
        (displayed-answers nil))
    (if (endp variables)
        (if (stream-endp binding-stream)
            'no
            'yes)
        (do ((binding-stream binding-stream
                             (stream-rest binding-stream)))
            ((stream-endp binding-stream) 'no-more)
          (let ((answer
                  (make-answer variables
                               (stream-first binding-stream))))
            (unless (member answer displayed-answers
                            :test #'equal)
              (display-answer answer)
              (setf displayed-answers
                    (cons answer displayed-answers))
	      ;;Only change needed to produce PROLOGlike version:
	      (if (char= #\, (read-char))
		  (format t ",")
		(return 'no-more))))))))

;;;; AUXILIARIES BORROWED FROM FORWARD CHAINING

(defun apply-filters (patterns initial-input-stream)
  "
  Purpose:	Tries to match all patterns to all assertions using
		all binding lists.
  "
  (if (endp patterns)
      initial-input-stream
      (apply-filters (rest patterns)
                     (filter-binding-stream (first patterns)
                                            initial-input-stream))))

(defun match-pattern-to-assertions (pattern bindings)
  "
  Purpose:	Tries to match one pattern to all assertions using
		one binding list.
  "
  (stream-concatenate
    (stream-transform
      #'(lambda (assertion) (try-assertion pattern
                                           assertion
                                           bindings))
      *assertions*)))

(defun try-assertion (pattern assertion bindings)
  "
  Purpose:	Tries to match one pattern to one assertion.
  "
  (let ((result (match pattern assertion bindings)))
    (if (eq 'fail result)
        'empty-stream
        (stream-cons result 'empty-stream))))

;;;; AUXILIARIES BORROWED, WITH MODIFICATIONS, FROM FORWARD CHAINING

(defun filter-binding-stream (pattern stream)
  "
  Purpose:	Tries to match one pattern to all assertions,
		and all rule consequents, using
		all binding lists.
  Remarks:	Tries rules as well as assertions.
  "
  (stream-concatenate
    (stream-transform
      #'(lambda (bindings)
          (stream-concatenate
            (stream-cons
              (match-pattern-to-assertions pattern bindings)
              (stream-cons
                (match-pattern-to-rules pattern bindings)
                'empty-stream))))
      stream)))

(defun instantiate-variables (pattern a-list)
  "
  Purpose:	Replaces variables by their bindings.
  Remarks:	More complicated than forward chaining version
		because variables may be matched to variables.
  "
  (cond ((atom pattern) pattern)
        ((eq '? (first pattern))
         (let ((binding (find-binding pattern a-list)))
           (if binding
               (instantiate-variables (extract-value binding) a-list)
               pattern)))
        (t (cons (instantiate-variables (first pattern) a-list)
                 (instantiate-variables (rest pattern) a-list)))))

;;;; RULE HANDLING AUXILIARIES

(defun match-pattern-to-rules (pattern bindings)
  "
  Purpose:	Tries to match one pattern to all rules using
		one binding list.
  "
  (stream-concatenate
    (stream-transform
      #'(lambda (rule) (try-rule pattern rule bindings))
      *rules*)))

(defun try-rule (pattern rule bindings)
  "
  Purpose:	Tries to match one pattern to one rule.
  "
  (let* ((rule (make-variables-unique rule))
         (result (unify pattern (rule-then rule) bindings)))
    (unless (eq 'fail result)
      (format t "~%Trying to establish")
      (dolist (l (instantiate-variables (rule-then rule) result))
	(format t " ~a" l))
      (format t " using rule ~a." (rule-name rule)))
    (if (eq 'fail result)
	'empty-stream
      (let ((new-stream (apply-filters (rule-ifs rule)
				       (stream-cons result
						    'empty-stream))))
	(if (stream-endp new-stream)
	    (format t "~%Rule ~a fails to establish" (rule-name rule))
	  (format t "~%Rule ~a establishes" (rule-name rule)))
	(dolist (l (instantiate-variables (rule-then rule) result))
	  (format t " ~a" l))
	(format t ".")
	new-stream))))

(defun hypothesize (assertion)
  "
  Purpose:	Backward chains from one hypothesis.
  "
  (case (chain assertion)
    (yes (format t "~%Evidently")
	 (dolist (l assertion) (format t " ~a" l))
	 (format t " is true.")
	 t)
    (no  (format t "~%Evidently")
	 (dolist (l assertion) (format t " ~a" l))
	 (format t " is false.")
	 nil)
    (t (values))))

;;;; MISCELLANEOUS AUXILIARY PROCEDURES

(defun make-variables-unique (rule)
  "
  Purpose:	Avoids conflict resulting from having the same variable
		names appear in more than one rule.
  "
  (let* ((variables (list-variables rule))
	 (unique-variables
	   ;;Make substitution list:
	   (mapcar #'(lambda (variable)
		       ;;Generate a new name, based on first character of
		       ;;the given variable, plus a unique number; I could
		       ;;have used GENSYM, but this approach makes it easier
		       ;;to test the code on a variety of Lisp systems:
		       (with-input-from-string
			 (input (format nil "~c-~a" 
					(aref (format nil "~a" variable) 0)
					(incf *name-counter*)))
			 (read input)))
		   variables)))
    (instantiate-variables
	  rule
	  ;;Make binding list:
	  (mapcar #'(lambda (v l) (list v (list '? l)))
		  variables
		  unique-variables))))

(defun make-variables-unique (rule)
  "
  Purpose:	Avoids conflict resulting from having the same variable
		names appear in more than one rule.
  "
  (let* ((variables (list-variables rule))
	 (unique-variables (mapcar #'(lambda (variable)
				       (gensym (format nil "~a" variable)))
				   variables)))
    (instantiate-variables
	  rule
	  (mapcar #'(lambda (v l) (list v (list '? l)))
		  variables
		  unique-variables))))

(defun list-variables (pattern &optional names)
  "
  Purpose:	Creates a list of variables appearing in a pattern.
  "
  (cond ((atom pattern) names)
        ((eq '? (first pattern))
         ;;Ignore anonymous variable:
         (if (or (eq '\_ (second pattern))
                 (member (second pattern) names))
             names
             (append names (rest pattern))))
        (t (list-variables (rest pattern)
                           (list-variables (first pattern)
                                           names)))))

;;;; ANSWER PREPARATION

(defun display-answer (answers)
  (format t "~&-->")
  (dolist (answer answers)
    (format t " ~a = ~a" (first answer) (second answer))))

(defun make-answer (variables bindings)
  (instantiate-variables 
    (mapcar #'(lambda (variable) (list variable (list '? variable)))
            variables)
    bindings))

;;;; ASSERTION AND RULE ACCESS FUNCTIONS

(defun remember-assertion (assertion)
  (stream-remember assertion *assertions*))

(defun remember-rule (rule)
  (stream-remember rule *rules*))

(defun clear-assertions () (setf *assertions* (make-empty-stream)))

(defun clear-rules () (setf *rules* (make-empty-stream)))

(defun display-assertions (&optional (stream *assertions*))
  (unless (stream-endp stream)
    (print (stream-first stream))
    (display-assertions (stream-rest stream))))

;;;; ACCESS FUNCTIONS FOR RULE ELEMENTS

(defun rule-name (rule) (first rule))

(defun rule-ifs (rule) (extract-from-rule 'if rule))

(defun rule-thens (rule) (extract-from-rule 'then rule))

(defun rule-then (rule) (first (rule-thens rule)))

(defun extract-from-rule (marker rule)
  (up-to-atom (rest (member marker rule))))

(defun up-to-atom (rule)
  (cond ((atom (first rule)) nil)
	(t (cons (first rule) (up-to-atom (rest rule))))))



