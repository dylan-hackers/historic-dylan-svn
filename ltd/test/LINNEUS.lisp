;;; LINNEUS.CL

;;; (C) Copyright 1995 by Steven L. Tanimoto.
;;; This program is described in Chapter 4 ("Knowledge Representation") of
;;; "The Elements of Artificial Intelligence Using Common Lisp," 2nd ed.,
;;; published by W. H. Freeman, 41 Madison Ave., New York, NY 10010.
;;; Permission is granted for noncommercial use and modification of
;;; this program, provided that this copyright notice is retained
;;; and followed by a notice of any modifications made to the program.

;;; The function LINNEUS embodies the main loop.
(defun linneus ()
  "Top-level loop for interaction with LINNEUS."
  (format t "(THIS IS LINNEUS. PLEASE TALK TO ME)~%")
  (loop (format t "--> ")  ; Print prompt symbol.
        (if (eql (interpret (read)) 'bye)
            (return '(goodbye)) ) ) )

;;; INTERPRET contains the production rules.
(defun interpret (text)
  "Applies production rules embedded in a COND form to
   interpret and process the user's input sentence."
  (cond
    ((handle-assertion text))
    ((handle-what-is text))
    ((handle-is-a text))
    ((handle-why text))
    ;; rule for session termination:
    ((match '(bye) text) 'bye)
    ;; rule for all other inputs:
    (t (answer '(i do not understand))) ) )

(defun answer (answer-list)
  "Prints out the answer and returns T."
  (prin1 answer-list)
  (terpri)
  t)

(defun handle-assertion (text)
  "Tests for and handles assertion of fact by the user."
  ;; i.e., rule for statements, e.g., (A DOG IS A MAMMAL):
  (let ((b (match '((match-article article1)(? x) is
                    (match-article article2)(? y) )
                    text)))
    (if b
      (let ((x (val 'x b))
            (y (val 'y b))
            (article1 (val 'article1 b))
            (article2 (val 'article2 b)) )
        (add-superset x y)
        (add-subset y x)
        (set-article x article1)
        (set-article y article2)
        (answer '(i understand)) ) ) ) )

(defun handle-what-is (text)
  "Tests for and handles questions e.g., (WHAT IS A DOG)."
  (let ((b (match '(what is (match-article article1)(? x))
                  text) )
        (isaflag nil)
        (includeflag nil) )
    (if b
      (let ((x (val 'x b))
            (y (val 'y b)) )
        (cond ((setf y (get-isa x))(setf isaflag t))
              ((setf y (get-includes x))(setf includeflag t))
              (t (format t "(I DON'T KNOW)~%")
                 (return-from handle-what-is t) ) )
        (answer (append
                  (list (get-article x))
                  (list x)
                  (if isaflag '(is)
                      '(is something more general than) )
                  (make-conj y) ) ) ) ) ) )

(defun handle-is-a (text)
  "Tests for and handles queries of the form (IS A DOG AN ANIMAL)."
  (let ((b (match '(is (match-article article1) (? x)
                       (match-article article2) (? y))
                  text)))
    (if b
      (let ((x (val 'x b))
            (y (val 'y b)) )
        (cond ((isa-test x y 10)
               (answer (append '(yes indeed)
                               (tell x y) )) )
              (t (answer '(sorry not that i know of))) ) ) ) ) )

(defun handle-why (text)
  "Tests for and handles WHY questions."
  (let ((b (match '(why is (match-article article1) (? x)
                           (match-article article2) (? y))
                  text)))
    (if b
      (let ((x (val 'x b))
            (y (val 'y b)) )
        (cond
          ((isa-test x y 10)
           (answer (cons 'because
                         (explain-links x y) )) )
          (t (answer
               '(but as far as i know it is not!) )) ) ) ) ) )

;;; Create hash tables to store the knowledge
;;; base components, and define the functions
;;; for storing and retrieving knowledge.
(let ((isa-base (make-hash-table :size 20))
      (includes-base (make-hash-table :size 20))
      (article-base (make-hash-table :size 20)) )
  (defun set-isa (x y) (setf (gethash x isa-base) y))
  (defun get-isa (x) (gethash x isa-base))
  (defun set-includes (x y)
    (setf (gethash x includes-base) y) )
  (defun get-includes (x) (gethash x includes-base))
  (defun set-article (x article)
    (setf (gethash x article-base) article) )
  (defun get-article (x) (gethash x article-base))
 )

;;; ADD-SUPERSET makes X one of the supersets of ANAME:
(defun add-superset (aname x)
  "Establishes X as a superset of ANAME."
  (set-isa aname (adjoin x (get-isa aname))) )

;;; ADD-SUBSET makes X one of the subsets of ANAME:
(defun add-subset (aname x)
  "Establishes X as a subset of ANAME."
  (set-includes aname (adjoin x (get-includes aname))) )

(defun match-article (x)
  "Returns T if X is in the list of articles."
  (member x '(a an the that this those these)) )

;;; MAKE-CONJ takes a list, e.g., (X Y Z) and inserts
;;; appropriate articles in front of each element, and
;;; inserts the atom AND between successive elements,
;;; e.g., (AN X AND A Y AND A Z):
(defun make-conj (lst)
  "Expresses LST as a conjunctive phrase."
  (cond
    ((null lst) nil)
    ((null (rest lst))
     (cons (get-article (first lst)) lst))
    (t (cons (get-article (first lst))
             (cons (first lst)
                   (cons 'and
                         (make-conj (rest lst)) ) ) )) ) )

;;; ISA-TEST returns T if there is a path from X to Y
;;; consisting of ISA links, with path length no more
;;; than N.
(defun isa-test (x y n)
  "Tests whether an X is a Y."
  (catch 'isa (isa-test1 x y n)) )

;;; ISA-TEST1 is the recursive slave of ISA-TEST.
(defun isa-test1 (x y n)
  "Implements the depth-first search for ISA-TEST."
  (cond ((eql x y) t)
        ((zerop n) nil)
        ((member y (get-isa x)) (throw 'isa t))
        (t (dolist (xx (get-isa x) nil)
          (isa-test1 xx y (1- n)) )) ) )

(defun explain-links (x y)
  "EXPLAIN-LINKS answers WHY questions."
  (cond
    ((eql x y) '(they are identical))      ; 1st special case
    ((member y (get-isa x)) '(you told me)); 2nd special case
    (t (explain-chain x (get-isa x) y)) ) ); general case

;;; The recursive function EXPLAIN-CHAIN is called by
;;; EXPLAIN-LINKS.  EXPLAIN-CHAIN produces an
;;; explanation of the first chain from X to Y that passes
;;; through a member of L.
(defun explain-chain (x l y)
  "Recursively helps EXPLAIN-LINKS."
  (cond
    ((null l) nil)              ; L should never be null.
    ((member y l)               ; See if last link --
     (cons 'and (tell x y)) )   ; Yes, precede by AND.
    ((isa-test (first l) y 10)  ; Does chain go through first L?
     (append (tell x (first l)) ; Yes, explain this link, etc.
             (explain-chain (first l)
                            (get-isa (first l))
                            y) ) )
    (t (explain-chain x (rest l) y)) ) ) ;else try next in L.

;;; TELL is a helping function for EXPLAIN-CHAIN.
(defun tell (x y)
  "Explains the (single) link from X to Y."
  (list (get-article x) x 'is (get-article y) y) )

