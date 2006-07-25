;;; -*- Mode: LISP; Syntax: Common-lisp; Package: DTP; Base: 10 -*-

;;; This file was stolen from Matt Ginsberg's MVL 4/1/93
;;; All the dag stuff at the end was commented out.
;;; Only need binding list routines

(in-package "DTP")

;;;----------------------------------------------------------------------------

;; this file defines the dag of binding lists.  That means we need an
;; equality check, a function that appends two binding lists (possibly
;; returning many answers), and an inheritance function.

;; #-ANSI (proclaim '(declaration dynamic-extent)) ; Changed by Don Geddis
#-ANSI (eval-when (compile load eval) (proclaim '(declaration dynamic-extent)))

;; The equality check is easy; the bindings have to be equal.  This
;; works because ((? . a)) is *not* more specific than ((? . ?1)) --
;; consider the result of applying both bindings to (x ? ?1).  (x a ?1)
;; is not an instance of (x ?1 ?1).  Similarly, ((? . ?1)) and ((? . ?2))
;; are not equal bindings.

(defun equal-binding (b1 b2)
  (set-equal b1 b2 #'equal))

;; the dot function is a lot harder.  Before we get to it, we define
;; append-binding-lists as the singular version.  (Many functions assume
;; that the result is unique and only use the first value returned.)

(defun append-binding-lists (b1 b2)
  (car (dot-binding b1 b2)))

;; compute the glb's of two binding lists.

;; If either binding list is NIL, it's easy -- we just return the other
;; one.  If neither list is NIL, it's quite complicated because of
;; potential variable interactions between the two lists.  The way we do
;; it can best be demonstrated by an example.  Suppose the lists are
;; ((?a . x) (?b . ?c)) and ((?a . x) (?c . d)), so that the adjoined
;; binding list should be ((?a . x) (?b . d) (?c . d)).  Now consider
;; the two expressions
;;  (?a ?b ?a ?c)		[the variables appearing in both lists]
;;  ( x ?c  x  d)		[the bindings appearing in both lists]
;; It is clear that the combined binding list is just the result of
;; unifying these two expressions!  So that's how we do it -- we make
;; lists of the variables and of their bindings, and then unify the
;; results.  The only trick is * variables, which should appear inside
;; *lists* in the two expressions (because they will be bound to lists
;; by unifyp).

(defun dot-binding (bl1 bl2)
  (cond ((null bl1) (list bl2))
	((null bl2) (list bl1))
	(t (let (exp1 exp2)
	     (dolist (item bl1)
	       (push (list-if-* (car item)) exp1)
	       (push (cdr item) exp2))
	     (dolist (item bl2)
	       (push (list-if-* (car item)) exp1)
	       (push (cdr item) exp2))
	     (unifyp exp1 exp2)))))

(defun list-if-* (x)
  (if (var-is-* (symbol-name x)) (list x) x))

;; we are often interested in whether or not one binding list is less
;; than another.  This function is faster than computing the dot and
;; then checking to see if it's equal to the first.  We still compute
;; the dot, but now just check to make sure that it's no longer than
;; the first argument.

(defun binding-le (b1 b2 &aux (dot (dot-binding b1 b2)))
  (declare (dynamic-extent dot))
  (and (not (cdr dot))
       (= (length (car dot)) (length b1))))

#|					; *** Commented out by Geddis

;; the inheritance functions are also subtle, since they interact with
;; the plugging function on the bilattice being used.  If a dag-list takes
;; a value x at a binding list b and b' is a binding list below b, then
;; the value at b' is the result of plugging into x with the
;; "difference" between b' and b.  However, the value at b presumably
;; already included the result of plugging in b, and it won't hurt to
;; plug it in again.  So instead of computing the difference between the
;; two binding lists, we just ignore bdg1 and plug in with bdg2.

(defun binding-inherit-fn (bilattice bdg1 bdg2 val)
  (declare (ignore bdg1))
  (mvl-plug val bdg2 bilattice))

(defparameter binding-dag
	      (make-dag :root nil :eq #'equal-binding :leq #'binding-le
			:dot #'dot-binding :inherit #'binding-inherit-fn
			:vars #'vars-in :plug #'plug
			:long "Dag of instantiation lists."
			:short "Instantiation"))

;; finally, we have to set up the modal operators corresponding to
;; quantification.  These operators are of the form (forall ?var ?prop)
;; for (exists ?var ?prop) and are pretty simple.  The only catch is
;; that the second arg can be either a variable or a list of variables.

(defun all-fn (var dag-fn)
  (quantifier-fn var dag-fn #'mvl-and))

(defun exists-fn (var dag-fn)
  (quantifier-fn var dag-fn #'mvl-or))

;; Do quantification a variable at a time ...

(defun quantifier-fn (var dag-fn fn)
  (if (listp var)
      (dolist (item var dag-fn) (setq dag-fn (qf-1 item dag-fn fn)))
    (qf-1 var dag-fn fn)))

;; The actual manipulation is easy; we just accumulate answers that
;; are the same except for the quantified variable.

(defun qf-1 (var dag-fn fn &aux bdgs entry temp ans
				(bilattice (dag-fn-bilattice dag-fn)))
  (dolist (item (all-dag-pts dag-fn))
    (setq entry (find-entry item (dag-fn-list dag-fn))
	  bdgs (remove-bdg var (dag-entry-pt entry))
	  temp (assoc bdgs ans :test #'equal-binding))
    (if temp
	(setf (cdr temp) 
	  (funcall fn (cdr temp) (dag-entry-val entry) bilattice))
      (push (cons bdgs (dag-entry-val entry)) ans)))
  (dag-accumulate-fn (dag-fn-dag dag-fn) bilattice ans))

;; here is where we actually make the modal operators.  Since the modal
;; operators are standard ones, they already exist and all we have to do
;; is to change the associated functions to the above.

(defun create-quantifier-modalities ()
  (setf (modal-op-fn (bdg-modal-op 'forall)) #'all-fn
	(modal-op-fn (bdg-modal-op 'exists)) #'exists-fn))

(defun bdg-modal-op (name)
  (find name (bilattice-modal-ops bdg-to-truth-val)
	:key #'modal-op-name))

|#					; *** Commented out by Geddis
