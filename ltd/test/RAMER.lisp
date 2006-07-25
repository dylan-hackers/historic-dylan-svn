;;; RAMER.CL
;;; A Common Lisp implementation of Ramer's recursive algorithm
;;; for approximating a polygon with another polygon.

;;; (C) Copyright 1995 by Steven L. Tanimoto.
;;; This program is described in Chapter 12 ("Vision") of
;;; "The Elements of Artificial Intelligence Using Common Lisp," 2nd ed.,
;;; published by W. H. Freeman, 41 Madison Ave., New York, NY 10010.
;;; Permission is granted for noncommercial use and modification of
;;; this program, provided that this copyright notice is retained
;;; and followed by a notice of any modifications made to the program.

;;; Here is a test polygon with 8 vertices (7 sides).
(defparameter *test-polygon* 
  '((0.0 0.0)(0.0 4.0)(2.0 6.0)(4.0 6.0)
    (6.0 4.0)(6.0 0.0)(4.0 -2.0)(2.0 -2.0) ) )

;;; declare global variable:
(defparameter *tolerance* 5.0)

;;; RAMER is the top-level function.
(defun ramer (poly)
  "Returns the entire polygonal approximation of POLY."
  (append (ramer1 poly)(last poly)) )

;;; RAMER1 is the recursive function which computes the
;;; current error of approximation and if exceeded, splits
;;; POLY and calls itself recursively on the two pieces.
(defun ramer1 (poly)
  "Returns polygonal approx. of poly, without last pt."
  (let* ((error-list (errors poly))
         (max-error (apply #'max error-list))
         two-halves)
    (cond ((> *tolerance* max-error)
           (list (first poly)) ) ; approx. ok, return 1st point.
          (t (setf two-halves
                   (split poly error-list max-error) )
             (append (ramer1 (first two-halves))
                     (ramer1 (second two-halves)) ) ) ) ) )

(defun split (poly error-list max-error)
  "Returns a list of two lists, obtained by breaking POLY
   at the vertex corresponding to the value MAX-ERROR
   on ERROR-LIST."
  (if (= (first error-list) max-error)
      (list (list (first poly)) poly)
      (let ((temp
              (split (rest poly)(rest error-list) max-error) ))
        (cons (cons (first poly)(first temp))
              (rest temp) ) ) ) )

(defun errors (poly )
  "Returns a list of the approximation errors at each
   of the internal points of POLY."
  (let* ((lastpoint (first (last poly)))
         (x1 (caar poly))
         (y1 (cadar poly))
         (x2 (first lastpoint))
         (y2 (second lastpoint)) )
    (cons 0.0 ; error for first point is clearly 0.
          (mapcar
            #'(lambda (p)
                (dist3 (first p)(second p) x1 y1 x2 y2) )
            (rest poly) ) ) ) )

(defun dist3 (x0 y0 x1 y1 x2 y2)
  "Returns the square of the minimum distance between
   the point (X0 Y0) and the line segment whose endpoints
   are (X1 Y1) and (X2 Y2)."
  (min (distsq x0 y0 x1 y1)
       (distsq x0 y0 x2 y2)
       (pdist  x0 y0 x1 y1 x2 y2) ) )

(defun pdist (x0 y0 x1 y1 x2 y2)
  "Computes and returns the square of the perpendicular distance
   from the point (X0 Y0) to the line that passes through
   points (X1 Y1) and (X2 Y2)."
  (let* ((a (- x1 x2))
         (b (- y1 y2))
         (c (- x0 x1))
         (d (- y0 y1))
         (temp (- (* a d)(* b c))) )
    (/ (* temp temp)
       (+ (* a a)(* b b)) ) ) )

(defun distsq (x1 y1 x2 y2)
  "Computes and returns the square of the Euclidean distance
   between the two points (X1 Y1) and (X2 Y2)."
  (let ((a (- x1 x2))
        (b (- y1 y2)) )
    (+ (* a a)(* b b)) ) )

;;; Here is a simple test routine.
(defun test () 
  "Calls RAMER on *TEST-POLYGON*."
  (format t "~%Approximation of test polygon:~% ~s"
          (ramer *test-polygon*) ) )

(trace ramer split)

(test)

; test data for the exercise in the text:
; (defparameter *sine*
;   '((0.0 0.0)(1.0 2.0)(3.0 -2.0)(5.0 2.0)
;     (7.0 -2.0)(9.0 2.0)(10.0 0.0) ) )
; (defparameter *cosine*
;   '((0.0 2.0)(2.0 -2.0)(4.0 2.0)(6.0 -2.0)
;     (8.0 2.0)(10.0 -2.0) ) )




