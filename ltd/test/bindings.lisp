;;; -*- Mode: LISP; Syntax: Common-lisp; Package: DTP; Base: 10 -*-

;;; This file was stolen from Matt Ginsberg's MVL 8/7/92

(in-package "DTP")

(eval-when (compile load eval)
  (export
   '(plug) ))

#-lucid (declaim (inline binding-variable binding-value))
#+lucid (proclaim '(inline binding-variable binding-value))

(defun binding-variable (binding)
  #+dtp-types (declare (type binding binding))
  (car binding) )

(defun binding-value (binding)
  #+dtp-types (declare (type binding binding))
  (cdr binding) )

;;;----------------------------------------------------------------------------

#-ANSI
(eval-when (compile load eval)
  (proclaim '(declaration dynamic-extent)) )

;; This file contains a variety of functions that manipulate binding
;; lists and "answers" - structures containing a binding list and a
;; truth value.

#|	Commented out by Don Geddis 5/12/93

(defvar true)

(defstruct (answer (:constructor make-answer (&optional binding value))
	    (:print-function print-answer))
  (binding nil)
  (value true))

(defun print-answer (answer stream print-depth)
  (declare (ignore print-depth))
  (format stream #+ANSI "~@<#S(~1IANSWER~:_ :BINDING~:_ ~s~:_ :VALUE~:_ ~s)~:>"
	         #-ANSI "#S(ANSWER :BINDING ~s :VALUE ~s)"
	  (answer-binding answer)
	  (mvl-print (answer-value answer))))

;; a few random utilities.
;;   invert-answer takes an answer and negates the truth value
;;   equal-answer checks two answers for equality

(defun invert-answer (answer)
  (make-answer (answer-binding answer) (mvl-not (answer-value answer))))

(defun equal-answer (a1 a2)
  (and (typep a1 'answer) (typep a2 'answer)
       (equal-binding (answer-binding a1) (answer-binding a2))
       (mvl-eq (answer-value a1) (answer-value a2))))

|#

;; popf				same keywords as delete
;; plug (x bdg-list)		plug binding list into x
;; delete-bdg (x bdg-list)	remove binding for x in bdg-list
;; get-bdg (x bdg-list)		find binding for x in bdg-list

(define-modify-macro popf (item &rest keywords) pop-fn)
(defun pop-fn (list item &rest key) (apply #'delete item list key))

;; plug.  If the binding list is empty, then you can just return x.  If
;; the binding list is not empty, then walk down the expression x; if x
;; is a variable on the binding list, do the substitution.  If x is an
;; atom not on the binding list, then just return it.  If x is a cons,
;; keep walking.  But as you walk, you have to be careful to handle
;; sequence variables correctly.  So if the car of x is a simple
;; variable, then you replace it with its value and continue.  If
;; the car of x is a sequence variable, do an append.

(defun plug (x bdg-list)
  (if bdg-list (plug1 x bdg-list) x))

(defun plug1 (x bdg-list &aux temp)
  (cond ((and (varp x) (setq temp (assoc x bdg-list))) (cdr temp))
	((atom x) x)
	((varp* (car x))
	 (if (setq temp (assoc (car x) bdg-list))
	     (append (cdr temp) (plug1 (cdr x) bdg-list))
	   (reuse-cons (car x) (plug1 (cdr x) bdg-list) x)))
	(t (reuse-cons (plug1 (car x) bdg-list) (plug1 (cdr x) bdg-list) x))))

(defun reuse-cons (car cdr orig)
  (if (and (eql car (car orig)) (eql cdr (cdr orig))) orig (cons car cdr)))

;; The following functions delete the binding for a given variable or
;; variables from a binding list.

(defun delete-bdg (x bdg-list)
  (delete x bdg-list :key #'car :count 1))

(defun remove-bdg (x bdg-list)
  (remove x bdg-list :key #'car :count 1))

(defun delete-bdgs (vars bdg-list)
  (delete-if #'(lambda (x) (member x vars)) bdg-list :key #'car))

(defun remove-bdgs (vars bdg-list)
  (remove-if #'(lambda (x) (member x vars)) bdg-list :key #'car))

;; Given a binding list and a list of variables, delete the bindings
;; for variables not on the list (which were spawned by intermediate
;; goals, presumably).

(defun meaningful-bdgs (bdgs vars)
  (remove-if-not #'(lambda (x) (member x vars)) bdgs :key #'car))

;; Get the binding for a given variable from a binding list.  Note that
;; this piece of code (and others!) makes implicit use of the fact that
;; binding lists are never "nested", so that the binding list binding x
;; to y and then y to z will never appear in the system -- x will always
;; be bound to z directly.

(defun get-bdg (x bdg-list) (cdr (assoc x bdg-list)))

