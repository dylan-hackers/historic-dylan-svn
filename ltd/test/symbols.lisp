;;; -*- Mode: LISP; Syntax: Common-lisp; Package: DTP; Base: 10 -*-

;;; This file was stolen from Matt Ginsberg's MVL around 8/7/92

(in-package "DTP")

;;;----------------------------------------------------------------------------

;; Symbol-manipulation utilities.

;; MVL variables are of two types: normal variables, which begin with
;; the character "?" followed by any character other than a "*", and
;; sequence variables, which begin with "?*".

;; Variables are created using gentemp.  The functions that do that are
;; new-?var and new-*var.

;; Functions for creating and sensing variables.  The sensing functions
;; include checks to make sure that the argument is not simply the
;; one-character string "?" by making sure that it is at least two
;; characters long.

(defun new-?var () (gentemp "?"))
(defun new-*var () (gentemp "?*"))

;; To check to see if something is a variable, just check to see if the
;; first character is a "?".  To check the type, you have to be careful
;; to make sure you don't look at the second character of a symbol with
;; a 1-character name.

(defun varp (x) (and (symbolp x) (char= (char (symbol-name x) 0) '#\?)))

(defun varp* (x &aux name)
  (and (symbolp x)
       (char= (char (setq name (symbol-name x)) 0) '#\?)
       (var-is-* name)))

(defun var-is-* (name)
  (and (> (array-dimension name 0) 1)
       (char= '#\* (char name 1))))

(defun varp? (x &aux name)
  (and (symbolp x) 
       (char= (char (setq name (symbol-name x)) 0) '#\?)
       (not (var-is-* name))))

;; vartype takes a symbol and returns ? if it is a normal varible, ?* if
;; it is a sequence variable, and NIL if it isn't a variable.

(defun vartype (x &aux name)
  (cond ((not (symbolp x)) nil)
	((char/= (char (setq name (symbol-name x)) 0) '#\?) nil)
	((= (array-dimension name 0) 1) '?)
	((char= '#\* (char name 1)) '?*)
	(t '?)))

;; groundp takes a logical expression and sees if it is ground.  If it's
;; an atom, it's easy -- it's ground if it isn't a variable.  If it's an
;; s-exp, then you have to check to make sure every element is ground.

(defun groundp (x)
  (if (atom x) (not (varp x)) (every #'groundp x)))

;; Finds and returns the list of variables in the expression p

(defun vars-in (p &optional vars)
  (cond ((null p) vars)
  	((varp p) (pushnew p vars))
	((atom p) vars)
	(t (vars-in (cdr p) (vars-in (car p) vars)))))

;; Database variables are kept distinct from any variables that the user
;; types in.  They are not interned, and have *database-variable* in
;; their value slot.

(defparameter *database-variable* (list nil))

(defun database-variable (var)
  (and (boundp var) (eq (symbol-value var) *database-variable*)))

;; standardize-variables takes a proposition and returns a new version
;; in which the variables have been standardized apart from all other
;; variables in the MVL database.  The way in which this is done is that
;; new variables are created that have the value *database-variable* and
;; have print names identical to those of the original variables (so that
;; the user can figure out what's going on).  Of course, when these
;; variables are actually displayed, they will frequently appear as
;; #:?var since they are uninterned.

;; standardize-variables returns two values -- the standardized version
;; of the sentence and a binding list.  It does the work by constructing a
;; binding list of the original variables and the standardized versions to
;; which they are bound.

(defun standardize-variables (p &aux bdglist)
  (declare (special bdglist))
  (values (standardize-variables1 p) bdglist))

(defun napcar (fn list)
  (do ((items list (cdr items)))
      ((null items) list)
    (rplaca items (funcall fn (car items)))))

;; Do a little bit of standardization.  If p is a cons, then just
;; standardize each variable in p.  Otherwise, if p is not a variable,
;; do nothing and return it.  If p is a variable that already appears in
;; the bdg list, then return its standardized version.  Otherwise, trim
;; the leading ? off the name of p and use concsym to make a new
;; uninterned version of it.

(defun standardize-variables1 (p)
  (declare (special bdglist))
  (if (listp p) 
      (mapcar #'standardize-variables1 p)
    (let ((var (vartype p)))
      (cond ((null var) p)
	    ((cdr (assoc p bdglist)))
	    (t (setq var (make-symbol (symbol-name p)))
	       (setf (symbol-value var) *database-variable*)
	       (push (cons p var) bdglist)
	       var)))))

;; symbol-maker takes a prefix and returns a function that itself returns
;; prefix0 prefix1 prefix2 ...  It also sets the value of the symbol to n
;; when called (which is needed for sorting).

(defun symbol-maker (prefix &aux (counter 0))
  #'(lambda (&aux (sym (concsym prefix (incf counter))))
      (setf (symbol-value sym) counter)
      sym))

(defvar *proposition-prefix* "P")
(defparameter proposition-maker (symbol-maker *proposition-prefix*))

;; when putting something new into the MVL database, the new symbol
;; should begin with the proposition prefix and you should also take the
;; time now to label that symbol with a list of the variables in the
;; given expression. In some instances it is important *not* to rename
;; the variables in the given expression (probably because they've been
;; renamed already).  If uniquify is NIL, then no renaming is done.

(defun new-meta-symbol (x &key (uniquify t) &aux m)
  (do () (nil)
    (setq m (funcall proposition-maker))
    (unless (symbol-plist m) (return)))
  (if uniquify
      (multiple-value-bind (new bdgs) (standardize-variables x)
	(setf (get m 'denotes) new
	      (get m 'vars-in) (mapcar #'cdr bdgs)
	      (get m 'bdgs) bdgs))
    (setf (get m 'denotes) x
	  (get m 'vars-in) (vars-in x)
	  (get m 'bdgs) nil))
  m)

;; concsym is a utility that takes two strings (typically a prefix like
;; "p" and an identifier like the proposition number), concatenates
;; them, interns the result if intern? is T, and returns the new symbol.

(defun concsym (str1 str2)
  (intern (format nil "~a~a" str1 str2)))

;; The denotation of a sentence is on its denotes property.

(defun denotes (x) (get x 'denotes))

;; the truth value assigned to a particular datum is on its truth-value
;; property.

(defvar unknown)

(defun truth-value (x)
  (or (and (symbolp x) (get x 'truth-value)) unknown))
