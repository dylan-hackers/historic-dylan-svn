;;; -*- Mode: LISP; Syntax: Common-lisp; Package: DTP; Base: 10 -*-

;;;----------------------------------------------------------------------------
;;;
;;;	File		Answers.Lisp
;;;	System		Don's Theorem Prover
;;;
;;;	Written by	Don Geddis (Geddis@CS.Stanford.Edu)

(in-package "DTP")

;;;----------------------------------------------------------------------------
;;;
;;;	Public

;;;----------------------------------------------------------------------------

(defun answer-equal-p (ans1 ans2)
  #+dtp-types (declare (type answer ans1 ans2))
  (and (label-equal-p (answer-label ans1) (answer-label ans2))
       (residue-equal-p (answer-residue ans1) (answer-residue ans2))
       (set-equal
	(cons (answer-binding-list ans1) (answer-ae-binding-lists ans1))
	(cons (answer-binding-list ans2) (answer-ae-binding-lists ans2))
	#'same-binding-list )
       (set-equal (answer-context ans1) (answer-context ans2)) ))

;;;----------------------------------------------------------------------------

(defun answer-instance? (instance general)
  "True IFF INSTANCE is more specific (or equal) to GENERAL"
  #+dtp-types (declare (type answer instance general))
  (and (label-instance? (answer-label instance) (answer-label general))
       (residue-instance? (answer-residue instance) (answer-residue general))
       (subsetp (answer-context general) (answer-context instance))
       (subsetp
	(cons (answer-binding-list general)
	      (answer-ae-binding-lists general) )
	(cons (answer-binding-list instance)
	      (answer-ae-binding-lists instance) )
	:test #'binding-list-more-general? )))

;;;----------------------------------------------------------------------------

(defun merge-answers (answer-list)
  "Return single answer that is the combination of all answers in the list"
  #+dtp-types (declare (type list answer-list))
  (let (new-bl new-label new-residue new-ae-binding-lists
	new-reduction-subgoals )

    ;; Binding list
    (let ((bls (mapcar #'answer-binding-list answer-list)))
      (setq bls (remove nil bls))
      (if (cdr bls)
	  (progn
	    (setq new-bl (merge-binding-lists bls))
	    (when (eq new-bl :not-a-binding-list)
	      (return-from merge-answers :not-an-answer) ))
	(setq new-bl (first bls)) ))
    
    ;; Label
    (let ((labels (mapcar #'answer-label answer-list)))
      (setq labels (remove nil labels))
      (if (cdr labels)
	  (setq new-label (reduce #'label-and labels))
	(setq new-label (first labels)) ))
    
    ;; Residue
    (let ((residues (mapcar #'answer-residue answer-list)))
      (setq residues (remove nil residues))
      (if (cdr residues)
	  (progn
	    (setq new-residue (reduce #'residue-merge residues))
	    (when (eq new-residue :not-a-residue)
	      (return-from merge-answers :not-an-answer) ))
	(setq new-residue (first residues)) ))
    
    ;; Answer extraction binding lists
    (let ((ae-bls (mapcar #'answer-ae-binding-lists answer-list)))
      (setq new-ae-binding-lists (reduce #'append ae-bls)) )
    
    ;; Reduction subgoals
    (let ((r-subgoals (mapcar #'answer-context answer-list)))
      (setq new-reduction-subgoals
	(remove-duplicates (reduce #'append r-subgoals)) ))
    
    ;; Construct new answer
    (make-answer
     :binding-list new-bl :context new-reduction-subgoals
     :label new-label :residue new-residue
     :ae-binding-lists new-ae-binding-lists
     #+dtp-trace :proof #+dtp-trace *proof* )
    ))

;;;----------------------------------------------------------------------------

(defun nanswer-merge-binding-list (answer binding-list)
  "Merge BINDING-LIST on to the binding list of ANSWER, or :NOT-AN-ANSWER"
  #+dtp-types (declare (type answer answer))
  #+dtp-types (declare (type binding-list binding-list))
  (if (answer-binding-list answer)
      (progn
	(setf (answer-binding-list answer)
	  (merge-binding-lists
	   (list (answer-binding-list answer) binding-list) ))
	(if (eq (answer-binding-list answer) :not-a-binding-list)
	    :not-an-answer
	  answer ))
    (progn
      (setf (answer-binding-list answer) binding-list)
      answer )))

;;;----------------------------------------------------------------------------

(defun nanswer-merge-label (answer label)
  "Merge LABEL with answer label"
  #+dtp-types (declare (type answer answer))
  #+dtp-types (declare (type label label))
  (if (answer-label answer)
      (setf (answer-label answer) (label-and (answer-label answer) label))
    (setf (answer-label answer) label) )
  answer )

;;;----------------------------------------------------------------------------

(defun nanswer-merge-residue (answer residue)
  "Merge RESIDUE with answer residue"
  #+dtp-types (declare (type answer answer))
  #+dtp-types (declare (type list residue))
  (if (answer-residue answer)
      (progn
	(setf (answer-residue answer)
	  (residue-merge (answer-residue answer) residue) )
	(if (eq (answer-residue answer) :not-a-residue)
	    :not-an-answer
	  answer ))
    answer ))

;;;----------------------------------------------------------------------------

(defun answer-binds-var-p (answer variable)
  #+dtp-types (declare (type answer answer))
  #+dtp-types (declare (type symbol variable))
  (find variable (answer-binding-list answer) :key #'binding-variable) )

;;;----------------------------------------------------------------------------

(defun nsimplify-binding-list (answer subgoal)
  "Remove any bindings for variables not in the subgoal literal"
  #+dtp-types (declare (type answer answer))
  #+dtp-types (declare (type dtp-subgoal subgoal))
  (let ((good-vars (literal-vars-in (slot-value subgoal 'literal)))
	new-abl new-ae-bls )
    (setq new-abl
      (remove-if-not
       #'(lambda (binding)
	   #+dtp-types (declare (type binding binding))
	   (find (binding-variable binding) good-vars) )
       (answer-binding-list answer) ))
    (setq new-ae-bls
      (remove new-abl (answer-ae-binding-lists answer)
	      :test #'same-binding-list ))
    (setf new-ae-bls
      (remove-duplicates new-ae-bls :test #'same-binding-list) )
    (setf (answer-binding-list answer) new-abl)
    (setf (answer-ae-binding-lists answer) new-ae-bls)
    answer ))

;;;----------------------------------------------------------------------------
;;;
;;;	Private

;;;----------------------------------------------------------------------------

(defun different-binding-lists (bl1 bl2)
  "True IFF the bindings are semantically distinct"
  #+dtp-types (declare (type list bl1 bl2))
  (set-exclusive-or
   (remove t bl1 :key #'car)
   (remove t bl2 :key #'car)
   :test #'equal ))

;;;----------------------------------------------------------------------------

(defun same-binding-list (bl1 bl2)
  "True IFF the binding lists are semantically identical"
  (not (different-binding-lists bl1 bl2)) )

;;;----------------------------------------------------------------------------

(defun binding-list-more-general? (general instance)
  "True IFF INSTANCE is more specific (or equal) to GENERAL"
  #+dtp-types (declare (type binding-list instance general))
  (subsetp general instance :test #'equal-binding?) )

(defun equal-binding? (b1 b2)
  "True IFF B1 and B2 are identical bindings"
  #+dtp-types (declare (type binding b1 b2))
  (and (eq (binding-variable b1) (binding-variable b2))
       (equal (binding-value b1) (binding-value b2)) ))

;;;----------------------------------------------------------------------------

(defun disjunctive-p (answer)
  "True IFF ANSWER is not disjuctive"
  #+dtp-type (declare (type answer answer))
  (answer-ae-binding-lists answer) )

;;;----------------------------------------------------------------------------
