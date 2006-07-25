;; Copyright 1994, Brown University, Providence, RI
;; See end of file for full copyright information

;; (in-package 'user)

;; Least commitment planning

;; In the following, a plan consists of a set of steps corresponding to
;; operations, constraints on the order in which these steps are to be
;; carried out, and information regarding how the steps depend on one
;; another.  The dependency information is of three different types.
;; First, there are records called requirements indicating propositions
;; that are required in order that certain operators have their desired
;; effect. Next, there are records called links describing how the
;; consequences of one operator are used to satisfy the requirements of
;; another. Finally, there are records called conflicts describing
;; potential undesirable interactions between operators. As an example of
;; a conflict, one operator might delete a proposition that another
;; operator adds, where the deleted proposition is required for the plan
;; to succeed.
(defun make-plan (steps constraints conflicts links requirements)
  (list steps constraints conflicts links requirements))
(defun plan-steps (plan) (car plan))
(defun plan-constraints (plan) (cadr plan))
(defun plan-conflicts (plan) (caddr plan))
(defun plan-links (plan) (cadddr plan))
(defun plan-requirements (plan) (cadddr (cdr plan)))

;; A link involves two steps and a condition. One step, called the
;; producer, has the condition in the list of additions for its
;; associated operator.  The producer makes the condition true. The other
;; step, called the consumer, has the condition in the list of
;; preconditions for its associated operator.  The consumer requires that
;; the condition be true. Links are created to satisfy requirements.
(defun make-link (producer condition consumer)
  (list producer condition consumer))
(defun link-producer (link) (car link))
(defun link-condition (link) (cadr link))
(defun link-consumer (link) (caddr link))

;; A conflict involves a link and a step. The step, called the clobberer,
;; has the condition of the link in the list of deletions for its
;; associated operator. A conflict arises when a step is added to a plan
;; and that step might be carried out after the producer of a link and
;; before the consumer, preventing the link from satisfying the
;; requirement that the link was introduced to satisfy. When a conflict
;; occurs the clobberer is said to threaten link.
(defun make-conflict (link clobberer)
  (list link clobberer))
(defun conflict-link (conflict) (car conflict))
(defun conflict-clobberer (conflict) (cadr conflict))

;; We represent constraints in terms of their begin and end steps, and 
;; assign each step a unique integer to distinguish different steps 
;; employing the same operator.
(let ((n 0))
  (defun make-step (operator) (list (setq n (+ n 1)) operator)))
(defun step-id (step) (car step))
(defun step-operator (step) (cadr step))

;; As in previous section, we describe a state as a set of conditions.
;; A requirement corresponds to a step and a condition that must be true
;; immediately prior to that step. The condition of the requirement
;; corresponds to a precondition of the operator indicated in the step.
(defun make-requirement (step condition) (list step condition))
(defun requirement-step (req) (car req))
(defun requirement-condition (req) (cadr req))

(defun make-operator (preconditions additions deletions)
  (list preconditions additions  deletions))
(defun preconditions (operator) (car operator))
(defun additions (operator) (car (cdr operator)))
(defun deletions (operator) (car (cdr (cdr operator))))

;; The Lisp implementation for refinements is provided as follows. Recall
;; that, unless the plan satisfies the goal, it has either conflicts or
;; requirements.
(defun refinements (plan operators)
  (if (not (null (plan-conflicts plan)))
      (resolve-conflict (car (plan-conflicts plan)) plan)
    (append (resolve-req-new-step (car (plan-requirements plan)) 
				  operators plan)
	    (resolve-req-existing-step (car (plan-requirements plan)) 
				       plan))))

;; Conflicts are resolved by constraining the clobberer to occur before
;; the producer of the associated link or after the consumer of the link.
(defun resolve-conflict (conflict plan)
  (let ((link (conflict-link conflict))
	(step (conflict-clobberer conflict)))
    (append (constrain step (link-producer link) plan)
	    (constrain (link-consumer link) step plan))))

;; A new plan is created with one step to occur before another if the two
;; steps are not already constrained to occur in the opposite order.
(defun constrain (step1 step2 plan)
  (if (precedes step2 step1 (plan-constraints plan)) nil
    (list (make-plan (plan-steps plan)
		     (adjoin (list step1 step2)
			     (plan-constraints plan)
			     :test #'equal)
		     (cdr (plan-conflicts plan))
		     (plan-links plan)
		     (plan-requirements plan)))))

;; Precedes determines if one step precedes another given a set of 
;; constraints.
(defun precedes (step1 step2 constraints)
  (or (equal step1 step2) 
      (some #'(lambda (c)
		(and (equal step1 (car c))
		     (precedes (cadr c) step2 constraints)))
	    constraints)))

;; To eliminate a requirement by adding a new step, create a new plan for
;; each applicable operator.
(defun resolve-req-new-step (req operators plan)
  (mapcan #'(lambda (p) (applicablep p req plan)) operators))

;; An operator is applicable just in case the condition of the
;; requirement is added by the operator. If applicable, create a new plan
;; from the old one by adding a new step, constraining the new step to
;; precede the step of the resolved requirement, elimiating this
;; requirement, adding a link resolving the requirement with the new step
;; as producer, adding a new set of requirements corresponding to the
;; preconditions of the operator, and updating the set of conflicts.
;; Conflicts can arise when the deletions of the new step threaten
;; existing links or when existing steps threaten the new link.
(defun applicablep (operator req plan)
  (if (not (member (requirement-condition req) 
		   (additions operator) :test #'equal)) nil
    (let* ((step (make-step operator))
	   (constraint (list step (requirement-step req)))
	   (link (make-link step (requirement-condition req)
			    (requirement-step req))))
      (list (make-plan (cons step (plan-steps plan))
		       (adjoin constraint (plan-constraints plan)
			       :test #'equal)
		       (append (link-conflicts link plan)
			       (step-conflicts step plan)
			       (plan-conflicts plan))
		       (cons link (plan-links plan))
		       (append (generate-requirements operator step)
			       (cdr (plan-requirements plan))))))))

(defun resolve-req-existing-step (req plan)
  (mapcan #'(lambda (s) (linkablep s req plan)) (plan-steps plan)))

;; An existing step can be linked to satisfy a requirement just in case
;; its associated operator adds the condition of the requirement and the
;; existing step is not constrained to follow the step of the
;; requirement. If this criterion is met, create a new plan from the old
;; one by constraining the existing step to the step of the resolved
;; requirement, eliminating this requirement, adding a link resolving the
;; requirement with the existing step as producer, and updating the set
;; of conflicts.
(defun linkablep (step req plan) 
  (if (or (not (member (requirement-condition req) 
		       (additions (step-operator step)) :test #'equal))
	  (precedes (requirement-step req) step 
		    (plan-constraints plan)))  nil
    (let ((link (make-link step
			   (requirement-condition req) 
			   (requirement-step req)))
	  (constraint (list step (requirement-step req))))
      (list (make-plan (plan-steps plan)
		       (adjoin constraint (plan-constraints plan)
			       :test #'equal)
		       (append (link-conflicts link plan)
			       (plan-conflicts plan))
		       (cons link (plan-links plan))
		       (cdr (plan-requirements plan)))))))

;; When a link is added, we find all steps that might conflict with it.
;; This function extends the data abstraction for objects of type LINK.

(defun link-conflicts (link plan)
  (mapcan #'(lambda (step) (conflictp link step))
	  (plan-steps plan)))

;; When a step is added, we find all links that might conflict with it.
;; This function extends the data abstraction for objects of type STEP.

(defun step-conflicts (step plan)
  (mapcan #'(lambda (link) (conflictp link step)) 
	  (plan-links plan)))

;; A link and a step conflict whenever the operator of the step deletes 
;; the condition of the link, unless the step is the consumer of the 
;; link.
(defun conflictp (link step)
  (if (and (not (equal step (link-consumer link)))
	   (member (link-condition link) 
		   (deletions (step-operator step))
		   :test #'equal))
      (list (make-conflict link step)) (list)))

;; The operator of a step has associated with it one requirement for each
;; of its preconditions.
(defun generate-requirements (operator step)
  (mapcar #'(lambda (p) (make-requirement step p)) 
	  (preconditions operator)))

(defun best (states goalp next comparator)
  (cond ((null states) nil)
        ((funcall goalp (car states)) (car states))
        (t (best (sort (append (funcall next (car states))
                               (cdr states))
                       comparator)
                 goalp
                 next
                 comparator))))

;; Simple abstract test:

;; (setq start (make-step (make-operator () '(P Q) ())))
;; (setq finish (make-step (make-operator '(R) () ())))
;; (setq reqs (list (make-requirement finish 'R)))
;; (setq operators (list (make-operator '(P) '(R) ())
;; 		      (make-operator '(Q) '(S) ())))
;; (setq plan (make-plan (list start finish)
;; 		      (list (list start finish)) () () 
;; 		      reqs))

;; Sussman's anomaly:

;; The start step uses a pseudo operator to encode the initial 
;; conditions.
(setq start 
      (make-step (make-operator () '((ON C A) (ON A TABLE) (ON B TABLE) 
				      (CLEAR C) (CLEAR B)) ())))

;; The finish step uses a pseudo operator to encode the goal conditions
(setq finish 
      (make-step (make-operator '((ON A B) (ON B C)) () ())))

;; The initial requirements correspond to the goal conditions
(setq requirements (list (make-requirement finish '(ON A B))
			 (make-requirement finish '(ON B C))))

;; The initial plan steps consists of the start and the finish steps.
(setq plan (make-plan (list start finish)
		      (list (list start finish)) 
		      () () 
		      requirements))

;; We show only the operators needed for the example.
(setq operators (list (make-operator '((on C A) (clear C))
				     '((on C table) (clear A))
				     '((on C A)))
		      (make-operator '((on A table) (clear A) (clear B))
				     '((on A B))
				     '((on A table) (clear B)))
		      (make-operator '((on B table) (clear B) (clear C))
				     '((on B C))
				     '((on B table) (clear C)))))

;; Test function:

(defun test ()
  (if (best (list plan)
	    #'(lambda (p) 
		(if (and (null (plan-conflicts p))
			 (null (plan-requirements p)))
		    (progn 
		      (plan-print p) t) nil))
	    #'(lambda (p) (refinements p operators))
	    #'(lambda (p q) (< (length (plan-requirements p))
			       (length (plan-requirements q)))))
      t nil))

(defun partial (plan)
  (mapcar #'(lambda (step1)
	      (cons (step-id step1)
		    (mapcan #'(lambda (step2)
				(and (not (precedes step1 step2
						    (plan-constraints plan)))
				     (not (precedes step2 step1
						    (plan-constraints plan)))
				     (list (step-id step2))))
			    (plan-steps plan))))
	  (sort (plan-steps plan) 
		#'(lambda (x y)
		    (precedes x y (plan-constraints plan))))))

;; Pretty printer for plans:

(defun plan-print (p)
  (setq *print-pretty* t) (terpri)
  (princ "Steps:") (terpri)
  (do ((steps (plan-steps p) (cdr steps)))
      ((null steps) nil)
      (princ (car steps))
      (terpri))
  (princ "Constraints:") (terpri)
  (do ((constraints (plan-constraints p) (cdr constraints)))
      ((null constraints) nil)
      (princ (step-id (car (car constraints))))
      (princ " precedes ")
      (princ (step-id (cadr (car constraints))))
      (terpri))
  (princ "Links:") (terpri)
  (do ((links (plan-links p) (cdr links)))
      ((null links) nil)
      (link-print (car links))
      (terpri))
  (princ "Conflicts:") (terpri)
  (do ((conflicts (plan-conflicts p) (cdr conflicts)))
      ((null conflicts) nil)
      (princ (step-id (conflict-clobberer (car conflicts))))
      (princ " conflicts with ")
      (link-print (conflict-link (car conflicts)))
      (terpri))
  (princ "Requirements:") (terpri)
  (do ((reqs (plan-requirements p) (cdr reqs)))
      ((null reqs) nil)
      (princ (step-id (requirement-step (car reqs))))
      (princ " requires ")
      (princ (requirement-condition (car reqs)))
      (terpri))
  (princ "Partial order:") (terpri)
  (princ (partial p)))

(defun link-print (link)
  (princ (step-id (link-producer link)))
  (princ " produces ")
  (princ (link-condition link))
  (princ " for ")
  (princ (step-id (link-consumer link))))


;; > (test)
;; 
;; Steps:
;; (5 (((ON B TABLE) (CLEAR B) (CLEAR C)) 
;;     ((ON B C))
;;     ((ON B TABLE) (CLEAR C))))                                   
;; (4 (((ON C A) (CLEAR C)) 
;;     ((ON C TABLE) (CLEAR A))
;;     ((ON C A))))
;; (3 (((ON A TABLE) (CLEAR A) (CLEAR B)) 
;;     ((ON A B))
;;     ((ON A TABLE) (CLEAR B))))
;; (2 (((ON A B) (ON B C)) 
;;     NIL 
;;     NIL))
;; (1 (NIL 
;;     ((ON C A) (ON A TABLE) (ON B TABLE) (CLEAR C) (CLEAR B))
;;     NIL))
;; Constraints:
;; 5 precedes 3
;; 1 precedes 5
;; 4 precedes 5
;; 5 precedes 2
;; 1 precedes 4
;; 4 precedes 3
;; 1 precedes 3
;; 3 precedes 2
;; 1 precedes 2
;; Links:
;; 1 produces (ON B TABLE) for 5
;; 1 produces (CLEAR B) for 5
;; 1 produces (CLEAR C) for 5
;; 5 produces (ON B C) for 2
;; 1 produces (ON A TABLE) for 3
;; 1 produces (ON C A) for 4
;; 1 produces (CLEAR C) for 4
;; 4 produces (CLEAR A) for 3
;; 1 produces (CLEAR B) for 3
;; 3 produces (ON A B) for 2
;; Conflicts:
;; Requirements:


;; Copyright 1994, Brown University, Providence, RI
;; Permission to use and modify this software and its documentation
;; for any purpose other than its incorporation into a commercial
;; product is hereby granted without fee.  Permission to copy and
;; distribute this software and its documentation only for
;; non-commercial use is also granted without fee, provided, however
;; that the above copyright notice appear in all copies, that both
;; that copyright notice and this permission notice appear in
;; supporting documentation, that the name of Brown University not
;; be used in advertising or publicity pertaining to distribution
;; of the software without specific, written prior permission, and
;; that the person doing the distribution notify Brown University
;; of such distributions outside of his or her organization. Brown
;; University makes no representations about the suitability of this
;; software for any purpose. It is provided "as is" without express
;; or implied warranty.  Brown University requests notification of
;; any modifications to this software or its documentation.
;;
;; Send the following redistribution information
;;
;; 	Name:
;; 	Organization:
;; 	Address (postal and/or electronic):
;;
;; To:
;; 	Software Librarian
;; 	Computer Science Department, Box 1910
;; 	Brown University
;; 	Providence, RI 02912
;;
;; 		or
;;
;; 	brusd@cs.brown.edu
;;
;; We will acknowledge all electronic notifications.
