;;; PROVER.CL
;;; A verifier for propositions using Wang's algorithm.

;;; (C) Copyright 1995 by Steven L. Tanimoto.
;;; This program is described in Chapter 6 ("Logical Reasoning") of
;;; "The Elements of Artificial Intelligence Using Common Lisp," 2nd ed.,
;;; published by W. H. Freeman, 41 Madison Ave., New York, NY 10010.
;;; Permission is granted for noncommercial use and modification of
;;; this program, provided that this copyright notice is retained
;;; and followed by a notice of any modifications made to the program.

;;; PROVER is the interactive top-level shell.
(defun prover ()
  "Top-level loop for a propositional validator using Wang's algorithm."
  (let (s)
    (loop
      (format t "~%Please enter proposition or HELP or RETURN.~%")
      (setf s (read))
      (cond ((eql s 'help)
             (format t "Here's an example: ")
             (format t "((a and (not b)) implies a) ~%") )
            ((eql s 'return) (return))
            ((setf s (catch 'syntax-error (reformat s)))
             (if (valid nil (list s))
                 (format t " is valid. ~%")
                 (format t " is NOT valid. ~%") ) )
            (t (format t ": Syntax error ~%"))
             ) ) ) )

;;; VALID is the main recursive workhorse that verifies
;;; the propositional logic "theorems" with Wang's rules.
;;; Pattern-matching results are passed back as and stored
;;; locally as the value of B -- the "bindings".
;;; Arguments L and R are the left and right sides of a sequent.
(defun valid (l r)
  "Returns T if the conjunction of the formulas in L implies
   any of the formulas in R."
  (let (b)
    (cond
      ; Test for axiom:
      ((intersection l r) t)
      ; NOT on the left:
      ((setf b (match '((* x) (not-wff y) (* z)) l))
       (valid (append (val 'x b) (val 'z b))
              (append r (rest (val 'y b))) ) )
      ; NOT on the right:
      ((setf b (match '((* x) (not-wff y) (* z)) r))
       (valid (append l (rest (val 'y b)))
              (append (val 'x b) (val 'z b)) ) )
      ; OR on the right:
      ((setf b (match '((* x) (or-wff y) (* z)) r))
       (valid l 
              (append (val 'x b) 
                      (list (first (val 'y b)))
                      (rest (rest (val 'y b)))
                      (val 'z b) ) ) )
      ; AND on the left:
      ((setf b (match '((* x) (and-wff y) (* z)) l))
       (valid (append (val 'x b)
                      (list (first (val 'y b)))
                      (rest (rest (val 'y b)))
                      (val 'z b) )
              r) )
      ; OR on the left:
      ((setf b (match '((* x) (or-wff y) (* z)) l))
       (and (valid (append (val 'x b)
                           (list (first (val 'y b)))
                           (val 'z b) )
                   r)
            (valid (append (val 'x b)
                           (rest (rest (val 'y b)))
                           (val 'z b) )
                   r) ) )
      ; AND on the right:
      ((setf b (match '((* x) (and-wff y) (* z)) r))
       (and (valid l
                   (append (val 'x b)
                           (list (first (val 'y b)))
                           (val 'z b) ) )
            (valid l
                   (append (val 'x b)
                           (rest (rest (val 'y b)))
                           (val 'z b) ) ) ) ) ) ) )

(defun or-wff (x)
  "Returns T if X if of the form (f1 OR f2)."
  (cond ((atom x) nil)
        (t (eql (second x) 'or)) ) )

(defun and-wff (x)
  "Returns T if X is of the form (f1 AND f2)."
  (cond ((atom x) nil)
        (t (eql (second x) 'and)) ) )

(defun not-wff (x)
  "Returns T if X is of the form (NOT f)."
  (cond ((atom x) nil)
        (t (eql (first x) 'not)) ) )

(defun wff (x)
  "Returns T if X is a well-formed formula."
  (cond ((atom x) t)
        ((match '(not (wff dum)) x) t)
        ((match '((wff dum) (op dum) (wff dum)) x) t)
        (t nil) ) )

(defun op (x)
  "Returns T if X is a recognized logical operator."
  (member x '(and or implies)) )

;;; REFORMAT checks syntax and eliminates IMPLIES.
(defun reformat (x)
  "Either returns the formula X with all occurrences
   of IMPLIES eliminated, or performs a THROW with the
   indication of a syntax-error."
  (cond ((atom x) x)
        ((null (wff x)) (throw 'syntax-error nil))
        ((not-wff x) (list 'not (reformat (second x))))
        ((equal (second x) 'implies)
         (list (list 'not (reformat (first x)))
               'or
               (reformat (third x)) ) )
        (t (list (reformat (first x))
                 (second x)
                 (reformat (third x)) )) ) )

;;; The following code is from MATCH2.CL
(defun match (p s)
  "Attempts to find a correspondence between P and S, utilizing
   any special constructs appearing in P.  Return an association
   list of bindings if successful; NIL otherwise."
  (cond
    ((handle-both-null p s))
    ((handle-normal-recursion p s))
    ((atom (first p)) nil)
    ((handle-? p s))
    ((handle-* p s))
    ((handle-restrict-pred p s))
    (t nil) ) )

(defun handle-both-null (p s)
  "Test for and handle case when both P and S are null."
  (if (and (null p)(null s))
      '((yes . yes)) ) )

(defun handle-normal-recursion (p s)
  "Test for and handle case when the first elements of
    P and S are EQL."
  (if (atom (first p))
      (if (eql (first p)(first s))
          (match (rest p)(rest s)) ) ) )

(defun handle-? (p s)
  "Test for and handle the case when (FIRST P) is of
   the form (? X)."
  (if s ; S must not be null
      (if (eql (first (first p)) '?)
          (let ((rest-match (match (rest p)(rest s))))
            (if rest-match
                (acons (first (rest (first p)))
                       (first s)
                       rest-match) ) ) ) ) )

(defun handle-* (p s)
  "Test for and handle the case when (FIRST P) is of
   the form (* X)."
  (if (eql (first (first p)) '*)
      (let ((pattern-variable (first (rest (first p))))
            rest-match)
        (cond ; subcase 1 --match one element of S:
              ((and s
                    (setf rest-match
                          (match (rest p)(rest s)) ) )
               (acons pattern-variable
                      (list (first s))
                      rest-match) )

              ; subcase 2 --match no elements of S:
              ((setf rest-match (match (rest p) s))
               (acons pattern-variable nil rest-match) )

              ; subcase 3 --match more than one element of S:
              ((and s
                    (setf rest-match
                          (match p (rest s)) ) )
               (acons pattern-variable
                      (cons (first s)
                            (val pattern-variable
                                 rest-match) )
                      (rest rest-match)) )
              (t nil) ) ) ) )

(defun handle-restrict-pred (p s)
  "Handle case when (FIRST P) is of the form (PREDICATE X)."
  (if s ; S must not be null
    (if (member (first (first p)) '(? *)) ; Don't apply '? or '*.
        nil
      (if (apply (caar p) (list (first s)))
          (let ((rest-match (match (rest p) (rest s)))
                (pattern-variable (first (rest (first p)))) )
            (if rest-match
                (acons pattern-variable
                       (first s)
                       rest-match) ) ) ) ) ) )

;;; The function VAL provides convenient access to
;;; something matched by a variable after matching with MATCH.
(defun val (variable alist)
  "Returns the value associated with VARIABLE on ALIST."
  (rest (assoc variable alist)) )
;;; end of code from MATCH2.CL

;;; Now invoke the program and provide test data...

(prover)

;(a or (not a))
;((a or (not a)) and (b or (not b)))


