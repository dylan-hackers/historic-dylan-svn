;;; -*- Mode: LISP; Syntax: Common-lisp; Package: DTP; Base: 10 -*-

;;;----------------------------------------------------------------------------
;;;
;;;	File		File.Lisp
;;;	System		Don's Theorem Prover
;;;
;;;	Written by	Don Geddis (Geddis@CS.Stanford.Edu)

(in-package "DTP")

(eval-when (compile load eval)
  (export
   '(dtp-load) ))

;;;----------------------------------------------------------------------------
;;;
;;;	Files can contain
;;;	1. ":theory <theory-name>"
;;;	2. ":includes <theory-name>"
;;;	3. ":label <value> <label-structure-name>"
;;;	4. ":nolabel"
;;;	5. Logical sentences
;;;
;;;	Keywords apply to subsequent sentences

(defun dtp-load (filename)
  #+dtp-trace
  (when (find :file-load *trace*)
    (format t "~&DTP Loading ~A:~%" filename) )
  (when (stringp filename)
    (unless (find #\. filename)
      (setq filename (concatenate 'string filename ".dtp")) )
    (unless (or (find #\/ filename) (find #\: filename))
      (setq filename (concatenate 'string *dtp-logic-directory* filename)) )
    #+cltl2 (setq filename (translate-logical-pathname filename)) )

  (with-open-file (p filename :direction :input)
    (loop
	with *package* = *dtp-package*
	with theory
	with theories = nil
	with keyword = (read p nil nil)	; :theory
	until (null keyword)
	finally
	  #+dtp-trace
	  (when (find :file-load *trace*) (format t "Done~%"))
	  (return (reverse theories))
	do (setq theory (read p nil nil))
	   (when (null theory) (loop-finish))
	   (pushnew theory theories)
	   #+dtp-trace
	   (when (find :file-load *trace*) (format t "~A..." theory))
	   (loop
	       with label = nil
	       with sentences = nil
	       for sexp = (read p nil nil)
	       until (or (null sexp) (eq sexp :theory))
	       do
		 (case sexp
		   (:includes
		    (let ((included-theory (read p nil nil)))
		      (when included-theory
			(includes theory included-theory) )))
		   (:label
		    (let* ((value (read p nil nil))
			   (structure-name (read p nil nil))
			   structure )
		      (setq structure (get-label-structure structure-name))
		      (if structure
			  (setq label
			    (make-label :value value :structure structure) )
			(format t "~&Error: Label structure ~A not known~%"
				structure-name ))))
		   (:nolabel
		    (setq label nil) )
		   (otherwise
		    (push (cons sexp label) sentences) ))
	       finally
		 (make-theory-from-sentences theory (reverse sentences)) ))
    ))

;;;----------------------------------------------------------------------------
