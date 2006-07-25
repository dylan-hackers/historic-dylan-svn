;;; -*- Mode: LISP; Syntax: Common-lisp; Package: DTP; Base: 10 -*-

;;;----------------------------------------------------------------------------
;;;
;;;	File		Literals.Lisp
;;;	System		Don's Theorem Prover
;;;
;;;	Written by	Don Geddis (Geddis@CS.Stanford.Edu)

(in-package "DTP")

;;;----------------------------------------------------------------------------
;;;
;;;	Data Structures

(defstruct (literal-node
	    (:conc-name literal-)
	    (:print-function literal-node-print-function) )
  "A literal"
  negated-p
  relation
  terms )

;;;----------------------------------------------------------------------------

(defun literal-node-print-function (structure stream depth)
  (declare (ignore depth))
  (format stream "<")
  (print-literal-node structure :s stream)
  (format stream ">") )

(defun print-literal-node (node &key (s t) (flip-negation nil))
  #+dtp-types (declare (type literal-node node))
  (cond
   (*display-logic-as-lists*
    (print-literal-node-as-list node :s s :flip-negation flip-negation) )
   (t
    (print-literal-node-as-logic node :s s :flip-negation flip-negation) )))

(defun print-literal-node-as-list (node &key (s t) (flip-negation nil))
  #+dtp-types (declare (type literal-node node))
  (unless (eq (literal-negated-p node) flip-negation)
    (format s "(not ") )
  (format s "(")
  (format s "~:(~A~)" (literal-relation node))
  (when (literal-terms node)
    (format s "~{ ~S~}" (literal-terms node)) )
  (format s ")")
  (unless (eq (literal-negated-p node) flip-negation)
    (format s ")") ))

(defun print-literal-node-as-logic (node &key (s t) (flip-negation nil))
  #+dtp-types (declare (type literal-node node))
  (unless (eq (literal-negated-p node) flip-negation)
    (format s "~~") )
  (format s "~:(~A~)" (literal-relation node))
  (when (literal-terms node)
    (let (term-strings)
      (setq term-strings
	(mapcar #'(lambda (term)
		    (with-output-to-string (s)
		      (term-to-string term s) ))
		(literal-terms node) ))
      (format s "(~A~{,~A~})" (car term-strings) (cdr term-strings)) )))

(defun term-to-string (term &optional (s t))
  "Variable terms -> lowercase string, Constant terms -> capitalized string"
  (cond
   ((varp term)
    (format s "~(~A~)" (variable-to-string term)) )
   ((consp term)
    (format s "~:(~A~)" (first term))
    (when (rest term) (format s "("))
    (format s "~A"
	    (with-output-to-string (str)
	      (loop
		  for remaining-terms on (rest term)
		  for subterm = (first remaining-terms)
		  do (term-to-string subterm str)
		  when (rest remaining-terms)
		  do (format str ",") )))
    (when (rest term) (format s ")")) )
   ((stringp term)
    (format s "~S" term) )
   (t
    (format s "~:(~A~)" term) )))

;;;----------------------------------------------------------------------------

(defun list-to-literal (list &optional (replacement-bindings nil))
  #+dtp-types (declare (type list list))
  #+dtp-types (declare (type binding-list replacement-bindings))
  (let ((lit (make-literal-node)))
    (when (eq 'not (first list))
      (setf (literal-negated-p lit) t)
      (setq list (second list)) )
    (setf (literal-relation lit) (first list))
    (if replacement-bindings
	(setf (literal-terms lit) (plug (cdr list) replacement-bindings))
      (setf (literal-terms lit) (cdr list)) )
    lit ))

;;;----------------------------------------------------------------------------

(defun literal-to-list (literal &key (ignore-negation nil))
  #+dtp-types (declare (type literal-node literal))
  (let ((new-list (cons (literal-relation literal) (literal-terms literal))))
    (unless ignore-negation
      (when (literal-negated-p literal)
	(setq new-list (list 'not new-list)) ))
    new-list ))

;;;----------------------------------------------------------------------------

(defun literal-list-equal-p (literal list &key (test #'equal))
  "True iff the literal is equivalent to the list representation"
  #+dtp-types (declare (type literal-node literal))
  #+dtp-types (declare (type list list))
  (let ((list-negation (eq (first list) 'not)))
    (when list-negation (setq list (second list)))
    (and (eq (literal-negated-p literal) list-negation)
	 (eq (literal-relation literal) (first list))
	 (funcall test (literal-terms literal) (rest list)) )))

;;;----------------------------------------------------------------------------

(defun literal-plug (literal binding-list)
  "Return a new literal, which is a copy of LITERAL with BINDING-LIST applied"
  #+dtp-types (declare (type literal-node literal))
  #+dtp-types (declare (type binding-list binding-list))
  (let ((copy (copy-literal-node literal)))
    (setf (literal-terms copy) (plug (literal-terms copy) binding-list))
    copy ))

(defun nliteral-plug (literal binding-list)
  "Destructively modify LITERAL by applying BINDING-LIST"
  #+dtp-types (declare (type literal-node literal))
  #+dtp-types (declare (type binding-list binding-list))
  (setf (literal-terms literal) (plug (literal-terms literal) binding-list))
  literal )

;;;----------------------------------------------------------------------------

(defun literal-flip-negation (literal)
  "Return a copy of LITERAL with opposite sign"
  #+dtp-types (declare (type literal-node literal))
  (let ((copy (copy-literal-node literal)))
    (setf (literal-negated-p copy) (not (literal-negated-p copy)))
    copy ))

(defun nliteral-flip-negation (literal)
  "Destructively modify LITERAL to have opposite sign"
  #+dtp-types (declare (type literal-node literal))
  (setf (literal-negated-p literal) (not (literal-negated-p literal)))
  literal )

;;;----------------------------------------------------------------------------

(defun query-to-answer-literal (query)
  (make-literal-node :relation 'answer_ :terms (find-vars query)) )

;;;----------------------------------------------------------------------------

(defun nliteral-rename-all-variables (literal)
  #+dtp-types (declare (type literal-node literal))
  (let ((bl (literal-rename-binding-list literal)))
    (nliteral-plug literal bl) ))

(defun literal-rename-binding-list (literal)
  #+dtp-types (declare (type literal-node literal))
  (mapcar
   #'(lambda (x) (cons x (make-new-variable x)))
   (remove-duplicates
    (find-vars (literal-terms literal)) )))

;;;----------------------------------------------------------------------------

(defun literal-vars-in (literal)
  "Returns list (set) of variables in terms of literal"
  #+dtp-types (declare (type literal-node literal))
  (remove-duplicates (find-vars (literal-terms literal))) )

;;;----------------------------------------------------------------------------

(defun function-depth (literal)
  "The maximum depth of function application to any term in LITERAL"
  #+dtp-types (declare (type literal-node literal))
  (if (literal-terms literal)
      (1- (tree-depth (literal-terms literal)))
    0 ))

;;;----------------------------------------------------------------------------
;;;
;;;	Literal equality tests

;;;----------------------------------------------------------------------------

(defun literal-possible-negated-pair-p (lit1 lit2)
  "Negated pair, but knows about EVAL"
  #+dtp-types (declare (type literal-node lit1 lit2))
  (and (not (eq (literal-negated-p lit1) (literal-negated-p lit2)))
       (eq (literal-relation lit1) (literal-relation lit2))
       (dtp-unifyp
	(mapcar #'eval-to-var (literal-terms lit1))
	(mapcar #'eval-to-var (literal-terms lit2)) )))

(defun eval-to-var (term)
  (if (and (consp term)
	   (eq (first term) 'eval) )
      (make-new-variable '?eval)
    term ))

;;;----------------------------------------------------------------------------

(defun literal-negated-pair-p (lit1 lit2 &key (test #'dtp-unifyp))
  "Returns unifying binding list, if negated pair, else nil"
  #+dtp-types (declare (type literal-node lit1 lit2))
  (and lit1 lit2
       (not (eq (literal-negated-p lit1) (literal-negated-p lit2)))
       (eq (literal-relation lit1) (literal-relation lit2))
       (funcall test (literal-terms lit1) (literal-terms lit2)) ))

;;;----------------------------------------------------------------------------

(defun literal-mgu (lit1 lit2 &key (ignore-sign nil))
  "Returns most general unifier of terms of both literals, if exists, else nil"
  #+dtp-types (declare (type literal-node lit1 lit2))
  (when (and (or ignore-sign
		 (eq (literal-negated-p lit1) (literal-negated-p lit2)) )
	     (eq (literal-relation lit1) (literal-relation lit2)) )
    (dtp-unifyp (literal-terms lit1) (literal-terms lit2)) ))

;;;----------------------------------------------------------------------------

(defun literal-instance
    (general-literal instance-literal &optional (old-binding-list nil))
  "True iff INSTANCE-LITERAL is an instance of GENERAL-LITERAL"
  #+dtp-types (declare (type literal-node general-literal instance-literal))
  #+dtp-types (declare (type binding-list old-binding-list))
  (and (eq (literal-negated-p general-literal)
	   (literal-negated-p instance-literal) )
       (eq (literal-relation general-literal)
	   (literal-relation instance-literal) )
       (dtp-instp
	(literal-terms general-literal)
	(literal-terms instance-literal)
	old-binding-list )))

;;;----------------------------------------------------------------------------

(defun literal-instance? (instance general)
  "True IFF INSTANCE is more specific (or equal) to GENERAL"
  #+dtp-type (declare (type literal-node instance general))
  (literal-instance general instance) )

;;;----------------------------------------------------------------------------

(defun literal-same-or-generalized-p (instance-literal general-literal)
  "True iff I-LITERAL is same as or an instance of G-LITERAL"
  #+dtp-types (declare (type literal-node instance-literal general-literal))
  (and (eq (literal-negated-p instance-literal)
	   (literal-negated-p general-literal) )
       (eq (literal-relation instance-literal)
	   (literal-relation general-literal) )
       (dtp-instp
	(literal-terms general-literal) (literal-terms instance-literal) )))

;;;----------------------------------------------------------------------------

(defun literal-equal-p (lit1 lit2)
  #+dtp-types (declare (type literal-node lit1 lit2))
  (and (eq (literal-negated-p lit1) (literal-negated-p lit2))
       (eq (literal-relation lit1) (literal-relation lit2))
       (equal (literal-terms lit1) (literal-terms lit2)) ))

;;;----------------------------------------------------------------------------

(defun literal-samep (lit1 lit2)
  #+dtp-types (declare (type literal-node lit1 lit2))
  (and (eq (literal-negated-p lit1) (literal-negated-p lit2))
       (eq (literal-relation lit1) (literal-relation lit2))
       (samep (literal-terms lit1) (literal-terms lit2)) ))

;;;----------------------------------------------------------------------------
