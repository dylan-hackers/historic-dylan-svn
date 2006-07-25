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

;;;; STRUCTURES AND ACCESSORS

(defstruct node
  "
  Purpose:	Capture the information needed for each node in the kd tree.
  "
  samples				;The samples seen at the node.
  count					;The number of samples.
  dimension				;The dimension that the node looks at.
  left-max				;The maximum of the "small" samples.
  left-samples				;The "small" samples or branch.
  right-min				;The minimum of the "large" samples.
  right-samples				;The "large" samples or branch.
  )

;;; Access Functions for Records Stored in List of Samples

(defun record-name (record) (first record))
(defun record-attribute (record) (second record))
(defun record-position (record) (nthcdr 2 record))

;;; Constructor for an Unknown

(defmacro make-record-for-unknown (name &rest size)
  `(setf ,name '(,name ? ,@size)))

;;; Access Functions for Discovered Answers

(defun make-answer (record distance2)
  (list (record-attribute record) distance2))
(defun answer-attribute (answer) (first answer))
(defun answer-distance2 (answer) (second answer))

;;;; IDENTIFICATION

(defun identify (unknown tree &optional (count 1) (weighting-function #'/))
  "
  Purpose:	To guess an attribute using nearest neighbor idea.
  Arguments:	UNKNOWN: a record, made using MAKE-RECORD-FOR-UNKNOWN.
		TREE: a KD tree.
		COUNT: the number of nearest neighbors to be used.
		WEIGHTING-FUNCTION: a function that establishes the way
				    a neighbor's influence is diminished
				    by distance.
  Returns:	The best guess for an unknown's unknown attribute.
  "
  (let ((candidates (find-answers unknown tree count)))
    (if (zerop (answer-distance2 (first candidates)))
	;;If the answer is at the same place exactly, report it:
	(let ((winner (answer-attribute (first candidates))))
	  (format t "~%The winner is ~a (exact match)." winner)
	  winner)
      ;;Otherwise, tally up votes, weighted by inverse distance:
      (let* ((best-pair (tally-votes candidates weighting-function))
	     (winner (first best-pair))
	     (score (second best-pair)))
	(format t "~%The winner is ~a with ~a votes." winner score)
	winner))))

(defun tally-votes (attribute-distance-pairs weighting-function)
  "
  Purpose:	Combines evidence when there are multiple nearest neighbors.
  Returns:	Attribute with the most votes.
  "
  (let ((attribute-weight-pairs nil) (attribute-score-pairs nil))
    ;;Make a-list pairs in which first element is an object's attribute 
    ;;and the second element is the distance-determined influence of
    ;;that object attribute.
    (setf attribute-weight-pairs
	  (mapcar #'(lambda (e)
		      (list (answer-attribute e)
			    (float
			      (funcall weighting-function
				       (answer-distance2 e)))))
		  attribute-distance-pairs))
    #+comment
    (format t "~%The votes are ~a" attribute-weight-pairs)
    ;;Make a-list pairs in which first element is an attribute and the
    ;;second element is the sum of the influences of the objects
    ;;with that attribute:
    (dolist (item attribute-weight-pairs)
      (let ((handle (assoc (first item) attribute-score-pairs)))
	(if handle
	    (incf (second handle) (second item))
	  (push item attribute-score-pairs))))
    ;;Sort, with the most recommended attribute in front:
    (setf attribute-score-pairs
	  (sort attribute-score-pairs
		#'(lambda (x y) (> (second x) (second y)))))
    ;;Pick the winner off the front:
    #+comment
    (format t "~%The results are ~a" attribute-score-pairs)
    (first attribute-score-pairs)))

;;;; FIND NEAREST NEIGHBORS

(defun find-answers (unknown tree &optional (count 1) (level 0))
  "
  Purpose:	Find N nearest neighbors using KD tree.
  Returns:	N answer structures; each has a block slot and a distance slot.
  Remarks:	This procedure is complicated, in part, because
		it has to deal with multiple nearest neighbors.
  "
  (let* ((dimension (node-dimension tree))
	 (projection (nth dimension (record-position unknown)))
	 (left-delta2 (delta2 projection (node-left-max tree)))
	 (right-delta2 (delta2 projection (node-right-min tree)))
	 threshold-delta2 winning-branch losing-branch)
    ;;Decide which branch has won and set variables accordingly:
    (if (< right-delta2 left-delta2)
	(progn (format t "~&~aTurn toward large numbers in dimension ~a: ~
		       ~a is closer to ~a than to ~a."
		       (indent level)
		       dimension projection
		       (node-right-min tree)
		       (node-left-max tree))
	       (setf threshold-delta2 left-delta2
		     winning-branch (node-right-samples tree)
		     losing-branch (node-left-samples tree)))
      (progn (format t "~&~aTurn toward small numbers in dimension ~a: ~
		     ~a is closer to ~a than to ~a."
		     (indent level)
		     dimension projection 
		     (node-left-max tree)
		     (node-right-min tree))
	     (setf threshold-delta2 right-delta2
		   winning-branch (node-left-samples tree)
		   losing-branch (node-right-samples tree))))
    ;;At this point, it looks like the winning direction is known.
    ;;This may prove wrong later, of course, because the decision
    ;;is based on comparison in one dimension only, not on actual distance.
    (let (winning-answers losing-answers nearest-winning-distance2)
      ;;If the winning branch is a node, then find the
      ;;closest neighbor by calling FIND-ANSWERS recursively;
      ;;Otherwise the winning branch is NOT a node, and the winning
      ;;branch IS the closest neighbor:
      (setf winning-answers
	    (if (node-p winning-branch)
		(find-answers unknown winning-branch count (1+ level))
	      (list (make-answer winning-branch
				 (distance2 unknown winning-branch))))
	    nearest-winning-distance2
	    (answer-distance2 (first (last winning-answers))))
      ;;At this point, FIND-ANSWERS needs to check the nearest answer
      ;;by comparing the actual distance between the unknown and
      ;;the nearest answer with the one-dimensional distance between
      ;;the unknown and the nearest answer on the wrong side of
      ;;the gap between the left and right groups; also, there
      ;;may not yet be enough answers:
      (if (and (<= nearest-winning-distance2 threshold-delta2)
	       (>= (length winning-answers) count))
	  ;;If the answer holds up, done:
	  winning-answers
	;;Otherwise, find the best answers on the other side too:
	(progn
	  ;;Indicate why there is more work to do:
	  (if (<= nearest-winning-distance2 threshold-delta2)
	      (format t "~&~aTrying alternate branch because too few answers ~
		      [~a <= ~a]."
		      (indent level)
		      (length winning-answers) count)
	    (format t "~&~aTrying other branch because worst answer ~
		    is not good enough [~a > ~a]."
		    (indent level)
		    nearest-winning-distance2 threshold-delta2))
	  ;;Establish best answers on the losing branch of the tree:
	  (setf losing-answers
		(if (node-p losing-branch)
		    (find-answers unknown losing-branch count (1+ level))
		  (list (make-answer losing-branch
				     (distance2 unknown losing-branch)))))
	  ;;Combine all answers and sort:
	  (setf winning-answers
		(sort (append winning-answers losing-answers)
		      #'(lambda (x y) (< (answer-distance2 x)
					 (answer-distance2 y)))))
	  ;;Prune if too many answers:
	  (setf winning-answers
		(butlast winning-answers
			 (max 0 (- (length winning-answers) count))))
	  ;;Finally, announce and return the answers:
	  #+comment
	  (format t "~&~aThe best answers are ~a."
		  (indent level)
		  (mapcar #'(lambda (answer)
			      (list (answer-attribute answer)
				    (answer-distance2 answer)))
			  winning-answers))
	  winning-answers)))))

;;;; CONSTRUCT TREE

(defun grow-tree (samples &optional (level 1) &aux winner)
  "
  Purpose:	Construct a KD tree from samples.
  Returns:	A tree node (a structure).
  Arguments:	Level set to 1 above to ensure comparison along y axis first.
  "
  (if (< (length samples) 2)
      (first samples)
    (let ((dimension-count (length (record-position (first samples))))
	  (possibilities nil))
      (setf winner (make-node-for-samples
		     ;;Select dimension for comparison:
		     (multiple-value-bind (ignore remainder)
			 (floor level dimension-count)
		       remainder)
		     samples))
      ;;Increment level and recurse along both branches:
      (incf level)
      (setf (node-left-samples winner)
	    (grow-tree (node-left-samples winner) level))
      (setf (node-right-samples winner)
	    (grow-tree (node-right-samples winner) level))
      winner)))

(defun make-node-for-samples (dimension samples)
  "
  Purpose:	Sorts samples along dimension supplied and creates a node.
  Returns:	A tree node (a structure).
  "
  (setf samples
	(sort samples #'(lambda (x y)
			  (< (nth dimension (record-position x))
			     (nth dimension (record-position y))))))
  (let* ((count (length samples))
	 (left-size (floor count 2))
	 (node
	   (make-node :samples samples
		      :dimension dimension
		      :count count
		      :left-max
		      (nth dimension
			   (record-position (nth (1- left-size) samples)))
		      :right-min
		      (nth dimension (record-position (nth left-size samples)))
		      :left-samples (butlast samples left-size)
		      :right-samples (nthcdr left-size samples))))
    node))

;;;; DISPLAY TREE

(defun display-tree (node &optional (level 0) branch)
  "
  Purpose:	Display a tree using indentation to indicate level.
  Arguments:	The root node of the tree.
  "
  (format t "~&~aNode splits ~a ~a samples ~a [~a ~a ~a]"
	  (indent level)
	  (if branch branch "top")
	  (node-count node)
	  (if (zerop (node-dimension node))
	      "vertically"
	    "horizontally")
	  (node-left-max node)
	  (float (/ (+ (node-left-max node) (node-right-min node)) 2))
	  (node-right-min node))
  (when (node-p (node-left-samples node))
    (display-tree (node-left-samples node) (1+ level) "left branch"))
  (when (node-p (node-right-samples node))
    (display-tree (node-right-samples node) (1+ level) "right branch")))

;;;; AUXILIARIES

(defun distance2 (a b &aux (a-size (record-position a))
			   (b-size (record-position b)))
  "
  Purpose:	Computes distance between two records.
  "
  (reduce #'+ (mapcar #'delta2 a-size b-size)))

(defun delta2 (x1 x2) (let ((delta (- x1 x2))) (* delta delta)))

(defun indent (depth) (make-string depth :initial-element #\space))

(defun one-no-matter-what (&rest ignore) 1)

