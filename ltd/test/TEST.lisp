(defclass point-record (graphics-record)
        ((x :initarg :x)		;---*** all required-init-arg
         (y :initarg :y)))

;;;; Many of these tests are from the DIRM (Dylan Interim Reference Manual)

(progn foo #'= 123 1.5 -4.0 +57 "abc" :hello '(1 2 3) 
      #(1 2 3) #\m 
      ;these are from pages 7 and 8
      t
      ;a multi-line
      ;comment
      nil)

(defvar my-variable 25) 


(let ((x 50))
  (+ x x)) 

;following from pages 12-14
((lambda (x) (+ x 1)) 99)


#'(lambda (a b) (declare (number a b)) (list (- a b) (+ a b)))


(if (eq moon-phase :full)
    (progn
      (wolf-form werewolf)
      (howl-at-moon werewolf))
    (human-form werewolf)) 


(defconstant vect #(7 8 9)) 

;page 21
(let ((foo 20)) (let ((foo 50)) (+ foo foo)))


(let ((foooooooooooooooooo 20))
  (let ((foooooooooooooooooo 50))
    (+ foooooooooooooooooo foooooooooooooooooo))) 


(let ((x (sqrt 2))) (declare (integer x)) x)


(multiple-value-bind (foo bar baz) (values 1 2 3)
  (list foo bar baz)) 


(defmethod opposite-sides ((center number) (radius number))
  (multiple-value-bind (min max) (edges center radius)
    (values max min)))


;;page 29
(unless (detect-gas? nose) (light match))


(cond ((<= (slot-value player1 'money) 0) (end-game player1))
      ((<= (slot-value player2 'money) 0) (end-game player2))
      (t (move player1) (move player2)))



;page 34
(progn
  (loop for thing = first-thing then (next thing)
	until (done? thing)
	do (do-some thing))
  (loop for city in olympic-cities
	for year from start-year by 4
	do (schedule-olympics city year)
	finally (notify press) (sell tickets))
  (loop for i from 0 below 100
	 for zombies from 0 below 100
	 for normals from 100 above 0 by -1
	 do (:= (element population i) (+ zombies normals))))


(defconstant foo
  (block bar
    #'(lambda (n)
        (return-from bar n)))) 

(defun double (my-method)
  (declare (type function my-method))
  #'(lambda (&rest args)
      (apply my-method args)
      (apply my-method args)
      nil))

(defmethod root-mean-square ((s sequence))
  (labels ((average (nums)
		    (/ (reduce #'+ nums) (size nums)))
	   (square (n) (* n n)))
	  (sqrt (average (mapcar #'square s)))))

(defclass sentient (life-form))
(defclass vulcan (:intelligent humanoid))

(defclass animal (object)
  ((n-legs :initform 4)))

(defclass spider (animal)
  ((n-legs :initform 0)))

;; page 160
(unwind-protect
 (progn (open-files) result)
 (close-files))

;; Not from the DIRM

(defun f (a b) (declare (values integer)) 42)
(defun g (a b) (declare values integer integer) (values 42 (+ a b)))

(literal '(1 :key (2 3) #\c 3.0 2/3 #C(2 3) #(4 2/3 #C(2 3) sym :key "string")))

(and (eq (+ (/ a (+ n 1)) (* b (f (~ x) (- y))))
	    (expt 2 (expt 2 n)))
	(or (> (element a i) (aref M i j)) (= (slot-value a b) (slot-value c d))))

(check-keyword-args (f :only-arg) :x 1 :y 2 :z :last-one)

(progn (setf v1 ((always (+ a b)) x))
       (setf (slot-value v2 slot) ((compose f g) x))
       (setf (aref A i j) 0))

(loop until (eq 3 4) do
      (loop while (eq 5 6) do
	    (unless (eq 7 8)
		(cond ((eq 9 10) (eleven) (twelve))
		      ((eq 13 14) (say "huh?") (change 13 14))
		      ((or (> a b) (< c d)) (f a b c d))
		      (t 42 (give up))))))
(cond
 ((eq a b) (f a b) (g a b))
 ((or  (> a b) (< a b)) (f (g a b) (g b a))))


(loop while (and (> (* c (+ a b)) z) (<= (expt z 2) 0)) do
       (f (aref m i j) (element a i) (+ 3 (* 4 (- xyz 34)))))

(progn (fff 1 2 (element a i :default 0))
	    (progn ; inner
	      (search #\c "string")))

#| This is a multiple
line comment.
This is a test to see what happens when the comment is very long and goes over the right margin. |#

(write-string "a string with a tab (	), a newline (
), a backslash (\\), and a \"funny\" character ().")

(defstruct struct
  slot1 slot2)

(defstruct (struct2 (:include struct) (:conc-name struck-))
  (slot3) (slot4 42 :type fixnum))

;; Test for read errors:

  (test1
   ;;a
   ;;b
   )
  ( #|1|# #|2|# #|3|# )
  ( #|1|# #|2|# #|3|# test2)
  (test3
   ;;c
   #|4|#
   ;;e
   #|5|#
   )
 '(;;a
   1
   ;;b
   .
   ;;c
   2)

()
nil

;; Packages:

(in-package :system)

exit




