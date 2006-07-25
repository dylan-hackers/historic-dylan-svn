;;; -*- Mode: LISP; Syntax: Common-lisp; Package: DTP; Base: 10 -*-

;;;----------------------------------------------------------------------------
;;;
;;;	File		TPTP.Lisp
;;;	System		Don's Theorem Prover
;;;
;;;	Written by	Don Geddis (Geddis@CS.Stanford.Edu)
;;;
;;;	PURPOSE
;;;	Interface to TPTP (Thousands of Problems for Theorem Provers).
;;;	TPTP is available on the World Wide Web at
;;;		http://wwwjessen.informatik.tu-muenchen.de/~suttner/tptp.html

(in-package "DTP")

(eval-when (compile load eval)
  (export
   '(convert-tptp tptp-load) ))

;;;----------------------------------------------------------------------------
;;;
;;;	Public

;;;----------------------------------------------------------------------------

(defun convert-tptp (&optional (category nil) (file nil) (overwrite nil))
  "Convert one (or multiple) files from TPTP format to KIF format"
  (cond

   ((and (stringp category) (stringp file))
    (tptp-to-kif category file overwrite t) )

   ((and (stringp category) (eq file t))
    (format t "Converting all files in ~A~%" category)
    (dolist (f (directory (tptp-pathname category)))
      (tptp-to-kif category (pathname-name f) overwrite t) ))

   ((stringp category)
    (loop
	initially (format t "~&Available files:~%  ")
	for fl in
	  (sort (mapcar #'pathname-name (directory (tptp-pathname category)))
		#'string< )
	for flnum = (subseq fl 3)
	for count from 0
	when (= count 6)
	do (setq count 0)
	   (format t "~%  ")
	do (format t "~10A" flnum) )
    (format t "~&File? ")
    (setq file (read-line))
    (setq file (string-upcase file))
    (setq file (concatenate 'string category file))
    (tptp-to-kif category file overwrite t) )
   
   ((eq category t)
    (format t "Converting all files in all categories~%")
    (dolist (d (directory (tptp-pathname)))
      (convert-tptp (pathname-name d) t overwrite) ))
   
   (t
    (loop
	initially (format t "~&Available directories:~%")
	for dirname in
	  (sort (mapcar #'pathname-name (directory (tptp-pathname))) #'string<)
	for count from 0
	when (= count 15)
	do (setq count 0)
	   (format t "~%")
	do (format t "  ~A" dirname) )
    (format t "~&Directory? ")
    (setq category (read-line))
    (setq category (string-upcase category))
    (convert-tptp category nil overwrite) )
   ))

;;;----------------------------------------------------------------------------

(defun tptp-load (pathname)
  #+dtp-types (declare (type (or pathname string) pathname))
  #+dtp-trace
  (when (find :file-load *trace*)
    (format t "~&DTP loading TPTP file ~A~%" (pathname-name pathname)) )
  (when (stringp pathname)
    (setq pathname (kif-pathname pathname)) )
  (with-open-file (p pathname :direction :input)
    (let ((*package* *dtp-package*))
      (loop
	  for sexp = (read p nil nil)
	  while sexp
	  collect (cons sexp nil) into sentences
	  finally (make-theory-from-sentences 'tptp sentences) )
      )))

;;;----------------------------------------------------------------------------
;;;
;;;	Private

;;;----------------------------------------------------------------------------

(defun tptp-to-kif (dom file overwrite comments)
  "Convert FILE from domain DOM, OVERWRITE old dest, keep COMMENTS"
  #+dtp-types (declare (type string dom file))
  #+dtp-types (declare (type boolean overwrite))
  (let ((source-path (tptp-pathname dom file))
	(dest-path (kif-pathname file)) )    
    (when (and (not overwrite) (probe-file dest-path))
      (unless (y-or-n-p "File ~A~%  already exists.  Overwrite (y/n)? "
			dest-path )
	(format t "~&TPTP conversion aborted~%")
	(return-from tptp-to-kif (values)) ))
    (format t "Converting ~A~%..to ~A~%" source-path dest-path)
    (with-open-file (in-f source-path :direction :input)
      (with-open-file (out-f dest-path
		       :direction :output :if-exists :supersede )
	(tptp-header out-f dom file)
	(loop
	    for line = (get-tptp-line in-f)
	    while line
	    do (process-tptp line out-f comments) )
	))
    (values) ))

;;;----------------------------------------------------------------------------

(defun tptp-pathname (&optional (category nil) (file nil))
  (let (path)
    (setq path (concatenate 'string *tptp-library* *tptp-problems*))
    #+cltl2 (setq path (translate-logical-pathname path))
    (when (stringp category)
      (setq path
            (merge-pathnames
             (concatenate 'string category *directory-separator*)
             path )))
    (when (stringp file)
      (unless (find #\. file)
	(setq file (concatenate 'string file ".p")) )
      (setq path (merge-pathnames file path)) )
    path ))

(defun kif-pathname (&optional (file nil))
  (let (path)
    (setq path (concatenate 'string *tptp-library* *tptp-kif*))
    #+cltl2 (setq path (translate-logical-pathname path))
    (when file
      (when (stringp file)
        (setq file (string-downcase file))
        (unless (pathname-type file)
	  (setq file (concatenate 'string file ".kif")) ))
      (setq path (merge-pathnames file path)) )
    path ))

;;;----------------------------------------------------------------------------

(defun get-tptp-line (s)
  "Read a line from stream S, concat multiple if in the middle of clause"
  (let ((line (read-line s nil nil)))
    (unless line (return-from get-tptp-line nil))
    (setq line (string-trim '(#\space) line))
    (if (and (not (string= line ""))
	     (eq (elt line 0) #\[)
	     (not (find #\] line)) )
	(loop
	    for nl = (read-line s nil nil)
	    until (or (null nl) (find #\] nl))
	    do
	      (setq nl (string-trim '(#\space) nl))
	      (setq line (concatenate 'string line nl))
	    finally
	      (if nl
		  (progn
		    (setq nl (string-trim '(#\space) nl))
		    (return (concatenate 'string line nl)) )
		(return nil) ))
      line )))

;;;----------------------------------------------------------------------------

(defun tptp-header (s cat file)
  (format s ";;; Automatic conversion of TPTP to KIF format~%")
  (format s ";;; By DTP version ~A~%" *dtp-version*)
  (format s ";;; Category ~A, File ~A~%" cat file)
  (format s "~%") )

;;;----------------------------------------------------------------------------

(defvar *tptp-clause-type* nil ":theorem, :axiom, :hypothesis")

(defun process-tptp (line out-f &optional (comments t))
  (setq line (string-trim '(#\space) line))
  (cond
   ((eq (length line) 0)	; Blank line
    (format out-f "~%") )
   ((eq (elt line 0) #\%)	; Comment
    (when comments
      (format out-f ";; ~A~%" line) ))
   ((input-clause-line line)	; input_clause
    (setq *tptp-clause-type* (tptp-clause-type line))
    (format out-f ";; ~A (~(~A~))~%"
	    (tptp-clause-name line) *tptp-clause-type* ) )
   ((include-file-line line)	; include
    (format out-f ";; ~A~%" line)
    (with-open-file (in2-f (include-file-name line) :direction :input)
      (loop
	  for line2 = (read-line in2-f nil nil)
	  while line2
	  do (process-tptp line2 out-f) )))
   (t				; Clause
    (tptp-parse-clause line out-f) )))

;;;----------------------------------------------------------------------------

(defun input-clause-line (line)
  (and (> (length line) 12)
       (string= (subseq line 0 12) "input_clause") ))

(defun tptp-clause-name (line)
  (subseq line (1+ (position #\( line)) (position #\, line)) )

(defun tptp-clause-type (line)
  (let ((end (1- (length line)))
	type )
    (when (eq (elt line end) #\,)
      (setq type
	(subseq line (1+ (position #\, line :from-end t :end (1- end))) end) )
      (cond
       ((string= type "theorem")
	:theorem )
       ((string= type "hypothesis")
	:hypothesis )
       ((string= type "axiom")
	:axiom )
       (t
	type ))
      )))

(defun include-file-line (line)
  (and (> (length line) 7)
       (string= (subseq line 0 7) "include") ))

(defun include-file-name (line)
  (let (path fn)
    (setq fn
      (subseq line (1+ (position #\' line)) (position #\' line :from-end t)) )
    (setq path *tptp-library*)
    #+cltl2 (setq path (translate-logical-pathname path))
    (setq path (merge-pathnames *tptp-axioms* path))
    (setq path (merge-pathnames fn path))
    path ))

;;;----------------------------------------------------------------------------

(defun tptp-parse-clause (line out-f)

  ;; Eliminate close of "input_clause" punctuation
  (when (string= (subseq line (- (length line) 2)) ").")
    (setq line (subseq line 0 (- (length line) 2))) )
  
  ;; Start of clause
  (when (eq (elt line 0) #\[)
    (format out-f "(or ")
    (when (eq *tptp-clause-type* :theorem)
      (format out-f "(goal) ") )
    (setq line (subseq line 1)) )
  
  ;; Find literals, one at a time
  (loop
      until (string= line "")
      with lit-str
      do
	(cond
	 ((eq (elt line 0) #\,)
	  (format out-f " ")
	  (setq line (subseq line 1)) )
	 ((eq (elt line 0) #\])
	  (format out-f ")~%")
	  (setq line (subseq line 1)) )
	 (t
	  (multiple-value-setq (lit-str line) (get-lit line))
	  (process-literal lit-str out-f) ))
	))

;;;----------------------------------------------------------------------------

(defun get-lit (line)
  "Return: (1) literal string, (2) remaining line"
  (loop
      with paren-depth = 0
      with lit-str = ""
      with lit-inc
      for comma-pos = (position #\, line)
      for open-paren-pos = (position #\( line)
      for close-paren-pos = (position #\) line)
      for close-bracket = (position #\] line)
      for linesize = (length line)
      until (or (= linesize 0)
		(and (= paren-depth 0)
		     (not (string= lit-str "")) ))
      finally (return (values lit-str line))
      do
	(cond
	 ((and open-paren-pos
	       (or (null comma-pos)
		   (> paren-depth 0)
		   (> comma-pos open-paren-pos) )
	       (> close-paren-pos open-paren-pos) )
	  (incf paren-depth)
	  (setq lit-inc (subseq line 0 (1+ open-paren-pos)))
	  (setq lit-str (concatenate 'string lit-str lit-inc))
	  (setq line (subseq line (1+ open-paren-pos))) )
	 ((and close-paren-pos (> paren-depth 0))
	  (decf paren-depth)
	  (setq lit-inc (subseq line 0 (1+ close-paren-pos)))
	  (setq lit-str (concatenate 'string lit-str lit-inc))
	  (setq line (subseq line (1+ close-paren-pos))) )
	 (comma-pos
	  (if (= paren-depth 0)
	      (progn
		(setq lit-inc (subseq line 0 comma-pos))
		(setq lit-str (concatenate 'string lit-str lit-inc))
		(setq line (subseq line comma-pos)) )
	    (progn
	      (setq lit-inc (subseq line 0 (1+ comma-pos)))
	      (setq lit-str (concatenate 'string lit-str lit-inc))
	      (setq line (subseq line (1+ comma-pos))) )))
	 (close-bracket
	  (setq lit-inc (subseq line 0 close-bracket))
	  (setq lit-str (concatenate 'string lit-str lit-inc))
	  (setq line (subseq line close-bracket)) )
	 (t
	  (format t "Error: Can't parse literal from '~A'~%" line)
	  (return (values "" "")) ))
	))

;;;----------------------------------------------------------------------------

(defun process-literal (lit-str out-f)
  (let ((not-flag nil))
    (when (eq (elt lit-str 0) #\-)
      (setq not-flag t)
      (format out-f "(not ") )
    (setq lit-str (subseq lit-str 2))
    (let ((pred-pos (position #\( lit-str)))
      (if pred-pos
	  (progn
	    (format out-f "(~A " (subseq lit-str 0 pred-pos))
	    (setq lit-str (subseq lit-str (1+ pred-pos))) )
	(progn
	  (format out-f "(~A" lit-str)
	  (setq lit-str "") )))
    (let ((term-end (position #\) lit-str :from-end t)))
      (when term-end
	(process-terms (subseq lit-str 0 term-end) out-f)
	(setq lit-str (subseq lit-str (1+ term-end))) ))
    (format out-f ")")
    (when not-flag
      (format out-f ")") )
    ))

;;;----------------------------------------------------------------------------

(defun process-terms (line out-f)
  (loop
      for end = (length line)
      until (= end 0)
      for comma-pos = (position #\, line)
      for open-pos = (position #\( line)
      for close-pos = (position #\) line)
      for next-pos = (min (or comma-pos end)
			  (or open-pos end)
			  (or close-pos end) )
      for next-punct = (unless (= next-pos end) (elt line next-pos))
      for symb = (subseq line 0 next-pos)
      do
	(if next-punct
	    (case next-punct
	      (#\, (unless (string= symb "")
		     (process-symb symb out-f) )
		(format out-f " ")
		(setq line (subseq line (1+ next-pos))) )
	      (#\( (format out-f "(")
		   (process-symb symb out-f)
		   (format out-f " ")
		   (setq line (subseq line (1+ next-pos))) )
	      (#\) (unless (string= symb "")
		     (process-symb symb out-f) )
		(format out-f ")")
		(setq line (subseq line (1+ next-pos))) ))
	  (progn
	    (process-symb symb out-f)
	    (setq line "") ))
	))

(defun process-symb (term out-f)
  (if (upper-case-p (elt term 0))
      (format out-f "?~(~A~)" term)
    (format out-f "~(~A~)" term) ))

;;;----------------------------------------------------------------------------
