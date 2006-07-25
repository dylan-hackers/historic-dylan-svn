;; Copyright 1994, Brown University, Providence, RI
;; See end of file for full copyright information

;; (in-package 'user)

;; This file includes all of the code for implementing the 
;; version spaces method described in the learning chapter.
;; The code also includes a solution to the exercise that 
;; prunes redundant concepts from the boundaries.

;; Abstract data types

;; A FEATURE is an attribute together with a value.
(defun FEATURE-attribute (feature) (car feature))
(defun FEATURE-value (feature) (cadr feature))

;; A DIMENSION is an attribute together with a set of possible values.
(defun make-DIMENSION (attribute values) (list attribute values))
(defun DIMENSION-attribute (dimension) (car dimension))
(defun DIMENSION-values (dimension) (cadr dimension))

;; An EXAMPLE is an identifier together with a set of features 
;; that describe the example and a class indicator.
(defun make-EXAMPLE (id features class) (list id features class))
(defun EXAMPLE-id (example) (car example))
(defun EXAMPLE-features (example) (cadr example))
(defun EXAMPLE-class (example) (caddr example))

;; The function FINDALL takes a list and a function and returns 
;; all items in the list such that the function returns non NIL 
;; when applied to the item.
(defun findall (items test)
  (do ((items items (cdr items)) (results (list)))
      ((null items) results)
      (if (funcall test (car items)) 
	  (setq results (adjoin (car items) results
				:test #'equal)))))

;; REFINE the general and specific version-space boundaries.  In the
;; following, we provide code for modifying the boundaries in a version
;; space given a new training example. The code provided below is not
;; guaranteed to converge to a unique concept. In the exercises, you are
;; shown how to modify the code to implement the complete algorithm given
;; in the text. Refine takes a single training example and two lists of
;; concepts representing the general and specific boundaries. A
;; conjunctive concept is represented as a list of features. Refine
;; determines whether the training example is positive, yes, or not, no,
;; and then proceeds to, respectively, generalize the concepts in the
;; specific bounary or specialize the concepts in the general boundary.
(defun refine (example general specific) 
  (if (eq (EXAMPLE-class example) 'yes)
      (simplify-specific (prune-general example general)
			 (generalize-specific example specific))
    (simplify-general (specialize-general example general)
		      (prune-specific example specific))))

;; Here is a simplified version of REFINE that produces boundaries
;; that include redundant and useless concepts.  This version does 
;; not guarantee convergence.  An example is within the boundaries
;; just in case it is consistent with one concept in the general 
;; boundary and one concept in the specific boundary such that 
;; the specific concept specializes the general concept.
;; (defun refine (example general specific) 
;;   (if (eq (EXAMPLE-class example) 'yes)
;;       (list general (generalize-specific example specific))
;;     (list (specialize-general example general) specific)))

;; Include in the general boundary only those concepts that 
;; are consistent with the positive examples.
(defun prune-general (example general)
  (findall general
	   #'(lambda (concept)
	       (consistent (EXAMPLE-features example) concept))))

;; Include in the specific boundary only those concepts that are 
;; not consistent with the negative examples.
(defun prune-specific (example specific)
  (findall specific
	   #'(lambda (concept)
	       (not (consistent (EXAMPLE-features example) concept)))))

;; Positive examples serve to generalize the specific boundary.
;; Generalizing the specific boundary consists of generalizing each 
;; concept in the boundary until it is consistent with the (positive) 
;; example.
(defun generalize-specific (example boundary) 
  (mapcan #'(lambda (c) 
	      (aux-generalize-specific example c))
	  boundary))
(defun aux-generalize-specific (example concept)
  (if (consistent (EXAMPLE-features example) concept) 
      (list concept)
    (generalize-specific example (generalize concept))))

;; Negative examples serve to specialize the general boundary.
;; Specializing the general boundary consists of specializing 
;; each concept in the boundary until it is not consistent 
;; with the (negative) example.
(defun specialize-general (example boundary) 
  (mapcan #'(lambda (c) 
	      (aux-specialize-general example c))
	  boundary))
(defun aux-specialize-general (example concept)
  (if (not (consistent (EXAMPLE-features example) concept))
      (list concept)
    (specialize-general example (specialize concept))))

;; Determine whether a description is consistent with a given concept.  
;; A concept is consistent with an example if for each feature (conjunct)
;; in the concept, either the attribute of the feature is not mentioned
;; in the list of features describing the example, or, if it is
;; mentioned, the corresponding feature has the same attribute value. 
;; For instance, if an example corresponds to an office on the third 
;; floor, then any concept consistent with that example must either 
;; not mention a floor or indicate the third floor.
(defun consistent (features concept) 
  (every #'(lambda (f)
	     (or (not (assq (FEATURE-attribute f) features))
		 (member f features :test #'equal)))
	 concept))

;; To generalize a concept you might add a disjunct or drop a conjunct.
;; Since we are restricted to conjunctions we only drop conjuncts.
;; For the restricted hypothesis space consisting of conjunctions of 
;; positive literals, generalization corresponds to removing features 
;; from a concept represented as a list of features.
(defun generalize (concept) 
  (mapcar #'(lambda (feature) 
	      (remove feature concept :test #'equal))
	  concept))

;; To specialize a concept you add a conjunct or drop a disjunct.
;; Since we are restricted to conjunctions we only add conjuncts.
;; Specialization corresponds to adding features that are consistent 
;; with the concept. Both specialize and generalize return concepts 
;; that are, respectively, minimally more specific and minimally more
;; general than the concept provided as an argument to the function.
(defun specialize (concept) 
  (mapcar #'(lambda (feature) (cons feature concept))
	  (findall (features)
		   #'(lambda (feature) 
		       (and (consistent (list feature) concept)
			    (not (member feature concept
					 :test #'equal)))))))

;; Construct the set of all possible features.
(defun features ()
  (mapcan #'(lambda (dim)
	      (mapcar #'(lambda (value)
			  (list (DIMENSION-attribute dim) 
				value))
		      (DIMENSION-values dim)))
	  (dimensions)))

;; Simplification for the special case of conjunctions of 
;; positive literals:
(defun simplify-general (general specific)
  ;; Include only concepts that are generalizations of 
  ;; some concept in the specific boundary.
  (setq general
	(findall general
		 #'(lambda (concept1)
		     (some #'(lambda (concept2)
				(or (more-specific concept2 concept1)
				    (equivalent concept1 concept2)))
			    specific))))
  ;; Eliminate any concepts that are specializations of other 
  ;; concepts in the general boundary.
  (setq general
	(findall general
		 #'(lambda (concept1) 
		     (not (some #'(lambda (concept2)
				    (more-specific concept1 concept2))
				general)))))
  (list general specific))

(defun simplify-specific (general specific)
  ;; Include only concepts that are specializations of 
  ;; some concept in the general boundary.
  (setq specific
	(findall specific
		 #'(lambda (concept1)
		     (some #'(lambda (concept2)
				(or (more-specific concept1 concept2)
				    (equivalent concept1 concept2)))
			    general))))
  ;; Eliminate any concepts that are generalizations of other 
  ;; concepts in the specific boundary.
  (setq specific
	(findall specific
		 #'(lambda (concept1) 
		     (not (some #'(lambda (concept2)
				    (more-specific concept2 concept1))
				specific)))))
  (list general specific))

(defun more-specific (concept1 concept2)
  (or (equal concept1 '(or))
      (and (subsetp concept2 concept1 :test #'equal)
	   (not (subsetp concept1 concept2 :test #'equal)))))

(defun equivalent (concept1 concept2)
  (and (subsetp concept2 concept1 :test #'equal)
       (subsetp concept1 concept2 :test #'equal)))

;; Example from the text

(defun dimensions ()
  (list (make-DIMENSION 'status '(faculty staff))
        (make-DIMENSION 'floor '(four five))
        (make-DIMENSION 'dept '(cs ee))))

(defun classes () '(yes no))

;; Versions takes a set of training examples and a pair of initial
;; boundaries and refines the boundaries by processing each training 
;; example in turn.
(defun versions (examples boundaries)
  (mapc #'(lambda (example)
            (setq boundaries (refine example 
                                     (first boundaries) 
                                     (second boundaries)))
            (format t "Example: ~A~%General: ~A~%Specific: ~A~%" 
                    example (first boundaries) (second boundaries)))
        examples))

;; Test function.
(let* ((examples
	(list (make-EXAMPLE '412 '((status faculty) (floor four) 
				   (dept cs)) 'yes)
	      (make-EXAMPLE '509 '((status staff) (floor five) 
				   (dept cs)) 'no)
	      (make-EXAMPLE '517 '((status faculty) (floor five) 
				   (dept cs)) 'yes)
	      (make-EXAMPLE '507 '((status faculty) (floor five) 
				   (dept ee)) 'no)))
       (boundaries 
	(list (list (list)) 
	      (mapcar #'EXAMPLE-features examples))))
  (defun test ()
    (versions examples boundaries)))


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

