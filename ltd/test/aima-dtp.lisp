;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*- File: logic/dtp/aima-dtp.lisp

;;;----------------------------------------------------------------------------
;;;
;;;	System		Don's Theorem Prover
;;;
;;;	Written by	Don Geddis (Geddis@CS.Stanford.Edu)
;;;
;;;	Version history
;;;             2.10    Modified by Norvig to run in the AIMA system.
;;;		2.09	Re-introduced forking conjunctions
;;;		2.08	Merged answers and reduction answers
;;;		2.07	Iteration, on subgoal and function depth
;;;		2.06	Simplified conjunction solving (removed forward inf)
;;;		2.05	TPTP Library tools
;;;		2.04	General "view" tool on proofs and answers
;;;		2.03	Reduction check at conjunct instead of subgoal
;;;		2.02	Depth limits and iteration (Broken implementation)
;;;		2.01	Corrected reduction inference combined with caching
;;;		2.00	Subgoaling inference system
;;;		1.00	Resolution-based inference system
;;;
;;;	Documentation Notes
;;;
;;;	Exported functions are intended to be called by users.  All other
;;;	functions and symbols are intended to be internal.
;;;
;;;	Many files have a "public" and a "private" section.  Functions defined
;;;	in the "private" section are ONLY called by functions within the same
;;;	file.  All functions called by routines in other files are placed in
;;;	the "public" section.
;;;
;;;	Function names: Both Common Lisp and Scheme conventions are
;;;	occassionally followed.  Thus a suffix of "-P" or "?" indicates a
;;;	predicate (testing for a condition).  A prefix of "N" or a suffix
;;;	of "!" indicates a destructive function, which may alter its arguments.

(in-package "COMMON-LISP-USER")

(eval-when (compile eval load)
  (defpackage "DTP"
    #-lucid (:use "COMMON-LISP") ))

(in-package "DTP")

;;;----------------------------------------------------------------------------
;;;
;;;	Options to change for new system installation

(eval-when (compile load eval)
  
  ;; Trace feature
  ;; Controls: Whether to include code and data structures for watching the
  ;;    inference in the middle of problem solving (and for examining
  ;;    proof spaces afterwards).  Most applications will want this option
  ;;    present, but some (e.g. an autonomous process) might wish the extra
  ;;    speed and smaller space, sacrificing user-friendlyness.
  (pushnew :dtp-trace *features*)
  
  ;; Type feature
  ;; Controls: Whether to declare data types of variables.  Some lisps can't
  ;;    handle this.
  (pushnew :dtp-types *features*)
  )

;;;----------------------------------------------------------------------------

(eval-when (compile load eval)
  (defparameter *dtp-major-version* 2)
  (defparameter *dtp-minor-version* 10)	; aima change
  (defparameter *dtp-tracing-status*
      #+dtp-trace "tracing"
      #-dtp-trace "no tracing" )
  (defparameter *dtp-typing-status*
      #+dtp-types "types"
      #-dtp-types "no types" )
  )

(eval-when (compile load eval)
  (defparameter *dtp-version*
      (format nil "~D.~2,'0D [~A,~A]"
	      *dtp-major-version*
	      *dtp-minor-version*
	      *dtp-tracing-status*
	      *dtp-typing-status* ))
  (export '(*dtp-version*)) )


(eval-when (compile load eval)
  (format t "~&DTP version ~A~%" *dtp-version*) )

;;;----------------------------------------------------------------------------

(eval-when (compile load eval)
  (defparameter *dtp-directory*
    (cl-user::aima-file nil :path '("logic" "dtp"))) ; aima change
  (defparameter *dtp-logic-directory*
    (cl-user::aima-file nil :path '("logic" "dtp" "logic"))) ; aima change
  (export '(*dtp-directory* *dtp-logic-directory*)) )




