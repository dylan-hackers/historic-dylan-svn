
#|-----------------------------------------------------------------------------
Artificial Intelligence, Second Edition
Elaine Rich and Kevin Knight
McGraw Hill, 1991

This code may be freely copied and used for educational or research purposes.
All software written by Kevin Knight.
Comments, bugs, improvements to knight@cs.cmu.edu
----------------------------------------------------------------------------|#

#|----------------------------------------------------------------------------
		            GRAPH UNIFICATION
			   "graph-unify.lisp"
----------------------------------------------------------------------------|#

#|----------------------------------------------------------------------------

The main calls are:

	(graph-unify d1 d2)		unifies two graph structures
	(tree->graph s-expression)	creates a tree-like graph
	(print->graph graph-expr)	creates a graph

See examples at the end of this file.

----------------------------------------------------------------------------|#


;;;----------------------------------------------------------------------
;;; DAG-STRUCT
;;;----------------------------------------------------------------------

;; This section contains structure definitions for graphs, nodes, and arcs.
;; Also included are print functions, constructor functions, 
;; de-constructor functions, copier functions, accessor functions,
;; and modifier functions.

;; STRUCTURES

(defstruct (graph-node (:print-function print-graph-node))
	class
	arcs
	mark
	mfset)

(defstruct (arc (:print-function print-arc))
	label
	destination)

;; NODE-CLASSES

(defconstant *variable* 'VAR)
(defconstant *fail* 'FAIL)

;; PRINT FUNCTIONS

(defun print-graph-node (n &rest ignore)
  (declare (ignore ignore))
  (princ (graph->tree n)))
; (princ (graph->print n)))    will display reentrancy

(defun print-arc (a &rest ignore)
  (declare (ignore ignore))
  (format *standard-output* 
          "#<Graph-Arc, LABEL: ~A>" 
	  (arc-label a)))

;; MEMORY MANAGEMENT

(defvar *graph-node-pool* nil)
(defvar *graph-arc-pool* nil)

;; CONSTRUCTORS

(defun create-graph-node (&key (class *variable*)
			       (arcs nil)
			       (mark nil)
			       (mfset nil))
  (let ((n (or (pop *graph-node-pool*) (make-graph-node))))
    (setf (graph-node-class n) class)
    (setf (graph-node-arcs n) arcs)
    (setf (graph-node-mark n) mark)
    (setf (graph-node-mfset n) (or mfset (list n)))
    n))

(defun create-arc (&key (label nil)
			(destination nil))
  (let ((n (or (pop *graph-arc-pool*) (make-arc))))
    (setf (arc-label n) label)
    (setf (arc-destination n) destination) 
    n))

(defun create-null-graph () (create-graph-node))

;; DE-CONSTRUCTORS

(defun dispose-graph-node (node) 
  (when (not (member node *graph-node-pool*))
        (push node *graph-node-pool*)))

(defun dispose-arc (arc) 
  (when (not (member arc *graph-arc-pool*))
        (push arc *graph-arc-pool*)))
        
(defun dispose-graph (node)
  (mapc #'dispose-graph-node (nodes-in-graph node)))

;; COPIERS

(defun copy-graph-node (node)
  (create-graph-node :class (graph-node-class node)
	       :arcs (graph-node-arcs node)
	       :mark (graph-node-mark node)
	       :mfset (graph-node-mfset node)))

(defun copy-arc (arc)
  (create-arc  :label (arc-label arc)
	       :destination (arc-destination arc)))

(defun copy-graph (node)
  (let* ((n1 (nodes-in-graph node)))
    (mapc #'(lambda (n) (setf (graph-node-mark n) (create-null-graph))) n1)
    (mapc #'(lambda (n)
	      (setf (graph-node-class (graph-node-mark n)) 
		    (copy-tree (graph-node-class n)))
	      (setf (graph-node-mark (graph-node-mark n)) nil)
	      (setf (graph-node-mfset (graph-node-mark n)) 
		    (if (mf-root-class? n) 
			(mapcar #'graph-node-mark (graph-node-mfset n))
			(graph-node-mark (graph-node-mfset n))))
	      (setf (graph-node-arcs (graph-node-mark n))
	            (mapcar #'(lambda (a) 
				(create-arc
				  :label (arc-label a)
				  :destination 
				    (graph-node-mark (arc-destination a))))
			    (graph-node-arcs n))))
          n1)
    (graph-node-mark node)))

;; ACCESSORS

(defun graph-node-arc (node label)
  (find label (graph-node-arcs node) 
        :key #'(lambda (a) (arc-label a))))

(defun graph-node-subnode (node label)
  (arc-destination (graph-node-arc node label)))

(defun graph-node-arc-labels (node)
  (mapcar #'arc-label (graph-node-arcs node)))

;; MODIFIERS

(defun add-arc (node arc)
  (unless (graph-node-arc node (arc-label arc))
    (push arc (graph-node-arcs node))))

(defun add-arc-in-order (node arc)
  (unless (graph-node-arc node (arc-label arc))
    (setf (graph-node-arcs node)
          (merge 'list (list arc) (graph-node-arcs node)
            #'(lambda (x y) (string< (string (arc-label x)) 
				     (string (arc-label y))))))))

;;;----------------------------------------------------------------------
;;; DAG-FNS
;;;----------------------------------------------------------------------

;; This section contains various functions over dag structures.

;;
;; Function MARK-GRAPH
;;
;; Takes a root node of a graph and a marker.  Sets the mark field of 
;; every node in the graph equal to the marker.  Uses a gensym'd 
;; temporary marker name.

(defun mark-graph (node mark)
  (let ((marker (gensym "MARKER-")))
    (mark-graph-1 node marker)
    (mark-graph-1 node mark)))

(defun mark-graph-1 (node sym)
  (when (not (eq (graph-node-mark node) sym))
        (setf (graph-node-mark node) sym)
        (mapc #'(lambda (a) (mark-graph-1 (arc-destination a) sym))
	      (graph-node-arcs node))))

;;
;; Function DEPTH-FIRST-TRAVERSAL
;;
;; Takes a root node of a graph and returns a list of nodes in the
;; graph.  Assumes that all nodes begin with MARK = NIL.

(defun depth-first-traversal (node) 
  (setf (graph-node-mark node) t)
  (cond ((null (graph-node-arcs node)) (list node))
        (t (cons node
                 (mapcan #'(lambda (n) 
                              (if (null (graph-node-mark n)) 
                                  (depth-first-traversal n) 
                                  nil))
                         (mapcar #'arc-destination 
				 (graph-node-arcs node)))))))

;;
;; Function NODES-IN-GRAPH
;;
;; Takes a root node of a graph and returns a list of all nodes in
;; the graph.  Uses a standard marking procedure to avoid traversing the
;; same portion of the graph more than once.

(defun nodes-in-graph (node)
  (mark-graph node nil)
  (depth-first-traversal node))

;;;----------------------------------------------------------------------
;;; DAG-PRINT
;;;----------------------------------------------------------------------

;; This section contains functions to read and write arbitrary graphs.
;; Graphs are coded as lists with variables to mark reentrancy.  Thus,
;; graph structures can effectively (1) be printed on the the screen, 
;; and (2) be written to files and read back in.

(defvar *dag-variables*
  '($0000 $0001 $0002 $0003 $0004 $0005 $0006 $0007 $0008 $0009
    $0010 $0011 $0012 $0013 $0014 $0015 $0016 $0017 $0018 $0019
    $0020 $0021 $0022 $0023 $0024 $0025 $0026 $0027 $0028 $0029
    $0030 $0031 $0032 $0033 $0034 $0035 $0036 $0037 $0038 $0039
    $0040 $0041 $0042 $0043 $0044 $0045 $0046 $0047 $0048 $0049
    $0050 $0051 $0052 $0053 $0054 $0055 $0056 $0057 $0058 $0059))

;; Function GRAPH->TREE
;;
;; Takes a dag-structure and returns a tree in list format.  Loses
;; reentrancy of dag (copies are made).  

(defun graph->tree (d)
  (cond ((null (graph-node-arcs d)) (graph-node-class d))
        (t (mapcar #'(lambda (a) (list (arc-label a) 
                                       (graph->tree (arc-destination a))))
   	           (graph-node-arcs d)))))

;; Function GRAPH-NODE-TYPE
;;
;; Return the type of an graph node stored in tree (s-expression) format.

(defun graph-node-type (t1)
  (cond ((atom t1) :atomic)
	(t (case (car t1)
	     (*OR* (if (atom (cadr t1)) 
                       :atomic-disjunction :complex-disjunction))
	     (*NOT* (if (atom (cadr t1))
		        :atomic-negation :complex-negation))
	     (*MULT* (if (atom (cadr t1))
	                :atomic-multiple-value :complex-multiple-value))
	     (otherwise :complex)))))

;; Function TREE->GRAPH
;;
;; Takes a tree in list format and returns 
;; a dag-structure.  The structure will of course have no reentrancy.

(defun tree->graph (t1)
 (let ((k (graph-node-type t1)))
  (cond ((member k (list :atomic :atomic-disjunction
			 :atomic-negation :atomic-multiple-value))
	 (create-graph-node :class t1 :arcs nil))
	((eq k :complex)
         (let ((n (create-graph-node :class *variable* :arcs nil)))
              (mapc #'(lambda (a) 
                        (add-arc-in-order n
                           (create-arc 
          		     :label (car a) 
                             :destination (tree->graph (cadr a)))))
                    t1)
              n))
	((eq k :complex-disjunction)
	 (create-graph-node :class *variable*
                      :arcs (cons '*OR* 
				      (mapcar #'(lambda (n) (tree->graph n)) 
				              (cdr t1)))))
	((eq k :complex-negation)
	 (create-graph-node :class *variable*
                      :arcs (cons '*NOT*
				      (mapcar #'(lambda (n) (tree->graph n)) 
				              (cdr t1)))))
	((eq k :complex-multiple-value)
	 (create-graph-node :class *variable*
                      :arcs (cons '*MULT*
				      (mapcar #'(lambda (n) (tree->graph n)) 
				              (cdr t1))))))))

;; Function GRAPH->PRINT
;;
;; Takes a dag-structure and returns a dag coded in list format.
;; For each node in the dag-structure, there is a corresponding element
;; in the list.  Each element has the form:
;;
;;      (<node-variable> <node-class> <node-subnodes>)
;; 
;; The order of elements corresponds to the order of a depth-first
;; traversal of the dag-structure.  If the arcs of the nodes are
;; ordered lexicographically, each dag-structure will have a well-defined
;; canonical list-format code.

(defun graph->print (d)
  (let ((n (nodes-in-graph d)))
    (mapc #'(lambda (n1 dv) (setf (graph-node-mark n1) (list dv))) 
          n *dag-variables*)
    (mapc #'(lambda (p)
	      (setf (graph-node-mark p)
	            (append
		      (graph-node-mark p)
		      (list (graph-node-class p))
		      (list 
                       (mapcar 
                        #'(lambda (a)
			    (list (arc-label a) 
                                  (car (graph-node-mark (arc-destination a)))))
		        (graph-node-arcs p))))))
          n)
    (mapcar #'graph-node-mark n)))

;; Function PRINT->GRAPH
;;
;; Takes a dag coded in list format and returns a dag-structure.
;; (The list format code described above is decoded back into a 
;; structure).  This function orders the arcs leaving a node 
;; lexicographically, so that exactly the same structure will appear
;; no matter how many times it is coded and decoded.

(defun print->graph (dp) 
  (let ((n (mapcar #'(lambda (n1) (create-graph-node :mark n1)) dp)))
    (mapc #'(lambda (p)
 	      (setf (graph-node-mfset p) nil)
	      (setf (graph-node-class p) (second (graph-node-mark p)))
              (mapc 
		#'(lambda (a)
		    (add-arc-in-order p
	 	      (create-arc 
			:label (first a)
			:destination 
			  (find (second a) n 
				:key 
			        #'(lambda (x) (car (graph-node-mark x)))))))
	  	(third (graph-node-mark p))))
          n)
    (car n)))

;;;----------------------------------------------------------------------
;;; DAG-MFSET
;;;----------------------------------------------------------------------

;; This section contains functions for performing disjoint set operations
;; on dag nodes.

;; Function MF-ROOT-CLASS?
;;
;; Takes a node and returns T if the node is the root of its equivalence
;; class tree.

(defun mf-root-class? (n) (listp (graph-node-mfset n)))

;; Function MF-FIND
;;
;; Performs the FIND operation for UNION-FIND disjoint sets.  Given
;; a node in a dag-structure, it returns another node, namely the root 
;; of the equivalence class tree for the input node.  After the FIND,
;; the tree is made more shallow by adjustment of pointers to the root.
  
(defun mf-find (x)
  (do ((q1 nil) (t1 x))
      ((mf-root-class? t1)                ; do path compression
       (progn () 
              (mapc #'(lambda (n) (setf (graph-node-mfset n) t1)) q1) 
              t1))
      (push t1 q1)
      (setq t1 (graph-node-mfset t1))))

;; Function MF-UNION
;;
;; Performs the UNION operation for UNION-FIND disjoint sets.  Given
;; two nodes in dag-structures, it joins their equivalence class trees
;; and returns the new root.  Smaller trees are merged into larger ones,
;; helping keep balance.

(defun mf-union (x y) 
  (let ((x1 (mf-find x)) 
        (y1 (mf-find y)))
    (if (eq x1 y1)             ; already in the same equivalence class
        x1
        (cond ((< (length (graph-node-mfset x1)) 
                  (length (graph-node-mfset y1)))   
	       (setf (graph-node-mfset y1) 
		     (nconc (graph-node-mfset y1) (graph-node-mfset x1)))
       	       (setf (graph-node-mfset x1) y1)
	       y1)
	      (t
	       (setf (graph-node-mfset x1)
		     (nconc (graph-node-mfset x1) (graph-node-mfset y1)))
	       (setf (graph-node-mfset y1) x1)
	       x1)))))
 
;; Function MF-INIT
;;
;; Takes the root of a dag-structure and initializes the graph for
;; UNION-FIND operations.  Each node is essentially placed into a
;; singleton equivalence class.

(defun mf-init (x)
  (mapc #'(lambda (n) (setf (graph-node-mfset n) (list n)))
        (nodes-in-graph x)))

;; Function CREATE-RESULT-GRAPH
;;
;; Takes a dag-structure in which some of the nodes may have been
;; UNION'd together.  Returns a new dag-structure in which nodes in
;; the same equivalence classes have been merged together into single
;; nodes.  All of the nodes in the result graph are allocated anew.

(defun create-result-graph (classes)
  (mapc #'(lambda (n)
	    (mapc #'(lambda (a) 
			(setf (arc-destination a)
			      (mf-find (arc-destination a))))
	          (graph-node-arcs n)))
        classes)
  (car classes))

(defun create-result-graph-1 (d)
  (let* ((nodes (nodes-in-graph d))
         (classes (remove-if-not #'mf-root-class? nodes))
         (res (create-result-graph classes)))
    (mapc #'dispose-graph-node (set-difference nodes classes))
    res))

(defun create-result-graph-2 (d1 d2)
  (let* ((nodes (let ((n1 (nodes-in-graph d1))
	              (n2 (nodes-in-graph d2)))
	             (append (list (car n1) (car n2))
                             (cdr n1)
          	             (cdr n2))))
	 (classes (remove-if-not #'mf-root-class? nodes))
	 (res (create-result-graph classes)))
    (mapc #'dispose-graph-node (set-difference nodes classes))
    res))

;;;----------------------------------------------------------------------
;;; DAG-UNIFY
;;;----------------------------------------------------------------------

;; This section contains functions for unifying two dag-structures.

;; Function CARRY-LABELS
;;
;; Adds the arcs of n1 to n2.

(defun carry-labels (n1 n2)
  (mapc #'(lambda (l) (add-arc n2 l))
        (graph-node-arcs n1)))

;; Functions for testing if a class is atomic or disjunctive, etc.
;;

(defun atomic-class (c) (atom c))
(defun disj-class (c) (and (list c) (eq (car c) '*OR*)))
(defun neg-class (c) (and (list c) (eq (car c) '*NOT*)))
(defun mult-class (c) (and (list c) (eq (car c) '*MULT*)))

;; Function UNIFY-CLASSES
;;
;; Unifies two classes.  The labels may be atomic, disjunctive, negated, 
;; or multiple.  

(defun unify-classes (c1 c2)
  (cond ((eq c1 *variable*) c2)
        ((eq c2 *variable*) c1)
        ((and (atomic-class c1) (atomic-class c2)) 
	 (if (eq c1 c2) c1 *fail*))
	((and (atomic-class c1) (disj-class c2))
	 (if (member c1 (cdr c2)) c1 *fail*)) 
	((and (atomic-class c1) (neg-class c2))
         (if (member c1 (cdr c2)) *fail* c1))
	((and (atomic-class c1) (mult-class c2))
	 (cons (car c2) (union (list c1) (cdr c2))))
 	((and (disj-class c1) (atomic-class c2))
	 (if (member c2 (cdr c1)) c2 *fail*)) 
   	((and (disj-class c1) (disj-class c2))
	 (let ((new (intersection (cdr c1) (cdr c2))))
           (cond ((null new) *fail*)
                 ((null (cdr new)) (car new))
                 (t (cons (car c1) new))))) 
	((and (disj-class c1) (neg-class c2))
	 (let ((new (set-difference (cdr c1) (cdr c2))))
           (cond ((null new) *fail*)
                 ((null (cdr new)) (car new))
                 (t (cons (car c1) new)))))
	((and (disj-class c1) (mult-class c2))
	 *fail*)
	((and (neg-class c1) (atomic-class c2))
         (if (member c2 (cdr c1)) *fail* c2))
	((and (neg-class c1) (disj-class c2))
	 (let ((new (set-difference (cdr c2) (cdr c1))))
           (cond ((null new) *fail*)
                 ((null (cdr new)) (car new))
                 (t (cons (car c2) new)))))
	((and (neg-class c1) (neg-class c2))
	 (cons (car c1) (union (cdr c1) (cdr c2))))
	((and (neg-class c1) (mult-class c2))
	 *fail*)
	((and (mult-class c1) (atomic-class c2))
	 (cons (car c1) (union (list c2) (cdr c1))))
	((and (mult-class c1) (disj-class c2))
	 (cons (car c1) (cons c2 (cdr c1))))
	((and (mult-class c1) (neg-class c2))
	 *fail*)
	((and (mult-class c1) (mult-class c2))
	 (cons (car c1) (union (cdr c1) (cdr c2))))))

;; Function GRAPH-UNIFY
;;
;; Unifies two graphs.  Congruence closure algorithm, runs in 
;; O(n log n) time, where n is the number of nodes in the input graphs.

(defun graph-unify (d1 d2)
 (mf-init d1)
 (mf-init d2)
 (let ((e1 (copy-graph d1)) (e2 (copy-graph d2)))
  (do ((pairs (list (cons e1 e2)))
       (current) (u) (v) (newclass) (w))
      ((null pairs) (create-result-graph-2 e1 e2))
    (setq current (pop pairs))
    (setq u (mf-find (car current)))
    (setq v (mf-find (cdr current)))
    (setq newclass (unify-classes (graph-node-class u) (graph-node-class v)))
    (when (eq newclass *fail*) (return *fail*))
    (when (or (and 
                   (not (equal (graph-node-class u) *variable*))
                   (not (null (graph-node-arcs v))))
              (and 
                   (not (equal (graph-node-class v) *variable*))
                   (not (null (graph-node-arcs u)))))
          (return *fail*))
    (setq w (mf-union u v))
    (setf (graph-node-class w) newclass)
    (if (eq w v) 
        (carry-labels u v) 
        (carry-labels v u))
    (mapc #'(lambda (l)
	      (push (cons (graph-node-subnode u l) 
                          (graph-node-subnode v l)) pairs))
          (intersection (graph-node-arc-labels u) 
                        (graph-node-arc-labels v))))))


;;----------------------------------------------------------------------
;;
;; Examples
;;
;;----------------------------------------------------------------------

(defvar g1)
(defvar g2)
(defvar g3)
(defvar g4)
(defvar g5)
(defvar g6)
(defvar g7)
(defvar g8)
(defvar g9)
(defvar g10)
(defvar g11)
(defvar g12)
(defvar g13)
(defvar g14)

(setq g1 (tree->graph '((a 1) (b 2))))
(setq g2 (tree->graph '((b 2) (c 3))))
(setq g3 (tree->graph '((b 3) (d 4))))

;; (graph-unify g1 g2) --> ((a 1) (b 2) (c 3))
;; (graph-unify g1 g3) --> FAIL

(setq g4 (tree->graph '((a (*OR* 1 2 3)) (b 5))))
(setq g5 (tree->graph '((a (*OR* 2 3 4)) (b 5))))

;; (graph-unify g4 g5) --> ((a (*OR* 2 3)) (b 5))

(setq g6 (tree->graph '((a (*NOT* 1 3)) (b 5))))
(setq g7 (tree->graph '((a (*OR* 1 2 3)) (b 5))))


(setq g8 (tree->graph '((a (*MULT* 1 2 3)) (b 5))))
(setq g9 (tree->graph '((a (*MULT* 2 3 4)) (b 5))))

;; (graph-unify g8 g9) --> ((a (*MULT* 4 1 2 3)) (b 5))

(setq g10 (tree->graph '((a ((b ((c 4) (d 6))))) (e 7))))
(setq g11 (tree->graph '((a ((b ((d 6))))) (e 7) (f 9))))

;; (graph-unify g10 g11) --> ((a ((b ((c 4) (d c)))) (e 7) (f 9))

(setq g12 (print->graph '(($0000 VAR ((a $0001) (b $0001))) 
                          ($0001 VAR nil))))
(setq g13 (print->graph '(($0000 VAR ((a $0001) (b $0002))) 
                          ($0001 4 nil)
                          ($0002 4 nil))))
(setq g14 (print->graph '(($0000 VAR ((a $0001) (b $0002))) 
                          ($0001 4 nil)
                          ($0002 5 nil))))

;; (graph-unify g12 g13) --> ((a 4) (b 4))

;; (graph-unify g12 g14) --> FAIL

;; (graph->print (graph-unify g12 g13)) --> 
;;			(($0000 VAR ((a $0001) (b $0001) ($0001 4 nil))

;;----------------------------------------------------------------------
;;
;; NATURAL LANGUAGE EXAMPLE
;;
;;----------------------------------------------------------------------

(defvar np-graph)
(defvar vp-graph)
(defvar constituents)
(defvar s-to-np-vp-rule)

;; Graph representing the noun phrase "the man".  The category arc has
;; the value NP, and the head arc contains information about determiner,  
;; root, and agreement.

(setq np-graph (print->graph
  '(($0001 VAR ((category $0002) (head $0003)))
    ($0002 NP nil)
    ($0003 VAR ((det $0004) (root $0005) (agreement $0006)))
    ($0004 the nil)
    ($0005 man nil)
    ($0006 singular nil))))

;; Graph representing the verb phrase "kills bugs".

(setq vp-graph (print->graph
  '(($0007 VAR ((category $0008) (head $0009)))
    ($0008 VP nil)
    ($0009 VAR ((root $0010) (tense $0011)
		(agreement $0012) (object $0013)))
    ($0010 kill nil)
    ($0011 present nil)
    ($0012 singular nil)
    ($0013 VAR ((category $0014) (head $0015)))
    ($0014 NP nil)
    ($0015 VAR ((root $0016) (agreement $0017)))
    ($0016 bug nil)
    ($0017 plural nil))))

;; Graph tying NP-GRAPH and VP-GRAPH into a single constituent graph with
;; arcs labeled CONSTIT1 and CONSTIT2.

(setq constituents
  (create-graph-node :arcs (list (create-arc :label 'constit1 
					     :destination np-graph)
				 (create-arc :label 'constit2
					     :destination vp-graph))))

;; Graph representing the augmented context-free grammar rule S -> NP VP,
;; enforcing subject-object number agreement, and building the resulting 
;; structure for a sentence.

(setq s-to-np-vp-rule (print->graph
  '(($0000 VAR ((constit1 $0001) (constit2 $0002) (build $0003)))
    ($0001 VAR ((category $0004) (head $0007)))
    ($0002 VAR ((category $0005) (head $0008)))
    ($0003 VAR ((category $0006) (head $0008)))
    ($0004 NP nil)
    ($0005 VP nil)
    ($0006 S nil)
    ($0007 VAR ((agreement $0009)))
    ($0008 VAR ((subject $0007) (agreement $0009) (mood $0010)))
    ($0009 VAR nil)
    ($0010 declarative nil))))

;; Unify rule with constituents...
;;
;; (graph-unify s-to-np-vp-rule constituents)

;; Unify rule with constituents, and retrieve result...
;;
;; (graph-node-subnode (graph-unify s-to-np-vp-rule constituents) 'build)


;; (graph-unify s-to-np-vp-rule constituents) ->
;;
;; ((BUILD
;;   ((CATEGORY S)
;;    (HEAD
;;     ((TENSE PRESENT) (ROOT KILL)
;;      (OBJECT
;;       ((CATEGORY NP) (HEAD ((AGREEMENT PLURAL) (ROOT BUG)))))
;;      (AGREEMENT SINGULAR) (MOOD DECLARATIVE)
;;      (SUBJECT ((ROOT MAN) (DET THE) (AGREEMENT SINGULAR)))))))
;;  (CONSTIT1
;;   ((CATEGORY NP) (HEAD ((ROOT MAN) (DET THE) (AGREEMENT SINGULAR)))))
;;  (CONSTIT2
;;   ((CATEGORY VP)
;;    (HEAD
;;     ((TENSE PRESENT) (ROOT KILL)
;;      (OBJECT
;;       ((CATEGORY NP) (HEAD ((AGREEMENT PLURAL) (ROOT BUG)))))
;;      (AGREEMENT SINGULAR) (MOOD DECLARATIVE)
;;      (SUBJECT ((ROOT MAN) (DET THE) (AGREEMENT SINGULAR))))))))


;; (graph-node-subnode (graph-unify s-to-np-vp-rule constituents) 'build) ->
;;
;; ((CATEGORY S)
;;  (HEAD
;;   ((TENSE PRESENT) (ROOT KILL)
;;    (OBJECT ((CATEGORY NP) (HEAD ((AGREEMENT PLURAL) (ROOT BUG)))))
;;    (AGREEMENT SINGULAR) (MOOD DECLARATIVE)
;;    (SUBJECT ((ROOT MAN) (DET THE) (AGREEMENT SINGULAR))))))
