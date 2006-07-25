;; -*- mode:common-lisp; package: mma; -*-
(provide 'ucons1)
(eval-when (eval compile load)
  (proclaim '(optimize (speed 3)(safety 0)(debug 0))))
(eval-when (eval load)
    (load "hash.fasl"))

;; (c) 1990, 1991, Richard J. Fateman
;; (c) 1994 Richard J. Fateman

(in-package :mma)
;; alternative to ucons1 file of 1990, 91. using new hash table extensions
;; in Allegro 4.2 ++ (must have patch file hash.fasl installed)

;; non-standard hash table feature used below

(defmacro eq-hash (object)
  "Gives us a hashing of an object such that (eq a b) implies
   (= (eq-hash a) (eq-hash b))"
  `(the fixnum (excl::pointer-to-positive-fixnum ,object)))

(defun car-cdr-eq (key1 type &optional key2)
  ;; special hash-code function for the unique-cons table
  (declare (optimize (speed 3)(safety 0)(debug 0)))
  (if type
      ;; this is the hash-code for a single cons
      (logxor (eq-hash (car key1))(eq-hash (cdr key1)))
    ;; this is the test to see if two conses have eq cars and eq cdrs
    (and (eq (eq-hash (car key1))(eq-hash(car key2)))
	 (eq (eq-hash (cdr key1))(eq-hash(cdr key2))))))


(defvar *uniq-table* (make-hash-table :test 'car-cdr-eq))

(defvar *uniq-atom-table* (make-hash-table :test #'eql))

  
(defun uniq (x)
  "Return a canonical representation that is EQUAL to x,
  such that (equal x y) => (eq (uniq x) (uniq y))"
  (typecase x
    ((or fixnum symbol) x)
    (atom (or (gethash x *uniq-atom-table*)
              (setf (gethash x *uniq-atom-table*) x)))
    (cons (ucons (uniq (car x))   ; this could check in 
                                  ; *uniq-table* first...
                 (uniq (cdr x))))))

(defvar *fakecons* '(car . cdr))

(defun ucons (x y)
  "Unique cons: (eq (ucons x y) (ucons x y)) is always true."
  (declare (special *fakecons* *uniq-table*)
	   (optimize (speed 3)(safety 0)(debug 0)))
  (let((temp *fakecons*)(tt *uniq-table*))
    (setf (car temp) x (cdr temp) y) ;don't allocate yet.
    (cond ((gethash temp  tt))
	  ;;If already there, great. 
	  (t (setf (gethash temp  tt) temp)
	     (setf *fakecons* (cons 'car 'cdr))
	     temp))))


(defun umapcar(f x)(cond((null x)nil)
			(t (ucons (funcall f (car x))(umapcar f (cdr x))))))

(defmacro ulist(&rest l)(cond ((null l)nil)
			      (t `(ucons ,(car l) (ulist ,@(cdr l))))))

(defun uappend(r s)(cond ((null r)s)
			 (t (ucons (car r)(uappend (cdr r) s)))))





