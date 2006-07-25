;;; -*- Mode: LISP; Syntax: Common-lisp; Package: DTP; Base: 10 -*-

;;;----------------------------------------------------------------------------
;;;
;;;	File		Epikit-DTP.Lisp
;;;	System		Don's Theorem Prover
;;;
;;;	Written by	Don Geddis (Geddis@CS.Stanford.Edu)
;;;	Purpose		To help with replacing Epikit by DTP

(in-package "DTP")

(eval-when (compile load eval)
  (export
   '(knownp proval remval prologp prologx prologs
     save drop empty facts contents brf )))

;;;----------------------------------------------------------------------------

(defun knownp (fact theory)		; matchp (DB can be more specific)
  "Actually only called with FACT of form (rel ?v1 ?v2 ...)"
  (not (null (active-theory-contents theory (car fact)))) )

(defun proval (fact theory)
  "Actually, no term inference, only equality lookup"
  (let ((*theory* theory))
    (prove (list '= fact '?x) :return-form '?x) ))

(defun remval (fact theory)
  (drop-sentence-from-theory
   (list '= fact (proval fact theory)) :theory-name theory ))

(defun prologp (fact theory)
  (let ((*theory* theory))
    (prove fact) ))

(defun prologx (expr fact theory)
  (let ((*theory* theory))
    (prove fact :return-form expr) ))

(defun prologs (expr fact theory)
  (let ((*theory* theory))
    (prove fact :all-answers t :return-form expr) ))

(defun save (fact theory)		; samep
  (save-sentence-in-theory fact :theory-name theory) )

(defun drop (fact theory)		; samep
  (drop-sentence-from-theory fact :theory-name theory) )

(defun empty (theory)
  (empty-theory theory) )

(defun facts (atom theory)
  (sentences-in theory :with-atom atom) )

(defun contents (theory)
  (sentences-in theory) )

(defun brf (fact)
  (let (cnf)
    (setq cnf
      (mapcar
       #'(lambda (dnf)
	   (cons
	    `<=
	    (cons
	     (first dnf)
	     (mapcar
	      #'(lambda (lit)
		  (if (eq 'not (first lit))
		      (second lit)
		    (list 'not lit) ))
	      (rest dnf) ))))
       (cnf fact) ))
    (if (cdr cnf)
	(cons 'and cnf)
      (first cnf) )
    ))

;;;----------------------------------------------------------------------------
