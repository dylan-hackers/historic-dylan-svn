;;; -*- Mode: LISP; Syntax: Common-lisp; Package: DTP; Base: 10 -*-

;;; This file was stolen from Matt Ginsberg's MVL 5/31/92

(in-package "DTP")

(defun first-binding-list (list-of-bl)
  (cond
   ((first list-of-bl)
    (first list-of-bl) )
   ((consp list-of-bl)
    '((t . t)) )
   (t nil) ))

(defvar unify-dummies)			; I comment out the original below

(defun dtp-instp (sexp1 sexp2 &optional (old-bl nil) &aux unify-dummies)
  (first-binding-list
   (remove 'fail
	   (mapcar #'subst-bdgs
		   (mapcar #'copy-alist
			   (inst-1 sexp1 sexp2 (list old-bl) nil) )))))

(defun dtp-unifyp (sexp1 sexp2)
  (first-binding-list (unifyp sexp1 sexp2)) )

(defun dtp-samep-binding-list (sexp1 sexp2)
  (let ((bls (samep sexp1 sexp2 t)))
    (cond
     ((first bls)
      (first bls) )		; Only one binding list matters
     ((consp bls)
      nil )			; Success, with no binding required
     (t
      :not-a-binding-list )	; Failure
     )))

#+mcl (defvar finished-bdgs)

;;;----------------------------------------------------------------------------

;; This file contains matching functions that handle sequence (*)
;; variables.
;;
;; Pre-unify --  determines whether two expressions unify ignoring
;;               possible variable binding conflicts.  Returns t or nil
;; Samep     --  determines whether two expressions are the same
;;    	         modulo variable renaming.  Returns t or nil.  (Returns
;;    	         a binding list if one is needed.)
;; Instp     --  determines whether one expression is an instance
;;     		 of another.  Returns a list of binding lists.
;; Unifyp    --  determines whether two expressions unify.
;;		 Returns a list of binding lists.
;;
;; The truth table for these functions is:
;; 
;;           Nil  List  Const  ?   *
;;         ---------------------------
;;    Nil  |  T    F      F    F   LR
;;    List |  F   Mtch    F    T   LR
;;    Const|  F    F      =    T   LR		LR= Lop and recur
;;    ?    |  F    T      T    T   LR
;;    *    |  LR   LR     LR   LR  LR     
;;

;; pre-unify is the simplest of the matching functions.  Note that this
;; function expects two lists as arguments.  Walk down each expression, doing:
;;  1.  If they are eq (presumably both NIL), succeed.
;;  2.  If the car's are equal, keep walking.
;;  3.  If the car's are both lists, make sure they pre-unify and then
;;  keep walking.
;;  4.  If the car of p is a sequence variable, call pre-unify*var, i.e.:
;;   4a. If the cdr of p is NIL, then you're done since p matches all of q
;;   4b. If the cdr of p matches any tail of q, you're also done
;;  5.  If the car of p is a variable, then:
;;   5a. If the car of q is a sequence variable, call pre-unify*var
;;   5b. Otherwise, keep walking
;;  6.  If the car of p is not a variable, then check out q and behave
;;  similarly, except if the car of q isn't a variable either, return
;;  failure.

(defun pre-unify (p q)
  (loop 
    (cond ((eq p q) (return t))
	  ((eql (car p) (car q)))
	  ((and (listp (car p)) (listp (car q)))
	   (unless (pre-unify (car p) (car q)) (return nil)))
	  (t (case (vartype (car p))
	       (?* (return (pre-unify*var p q)))
	       (? (if (varp* (car q)) (return (pre-unify*var q p))))
	       (t (case (vartype (car q))
		    (?* (return (pre-unify*var q p)))
		    (?)
		    (t (return nil)))))))
    (setq p (cdr p) q (cdr q))))

(defun pre-unify*var (p q)
  (or (null (setq p (cdr p)))
      (loop
	(if (pre-unify p q) (return t))
	(unless q (return))
	(setq q (cdr q)))))

;; Simple matcher that determines whether or not two expressions are the
;; same up to variable renaming.  Keeps variable equivalences in the
;; special binding list blist.  This function accepts an optional
;; argument that indicates that T or NIL is not good enough as an answer
;; -- you want the binding list created.

(defun samep (p q &optional return-answer? &aux blist)
  (declare (special blist))
  (when (samep1 p q)
    (or (null return-answer?)  	       ;knowing they're the same is good enough
	(samep-answer blist))))

;; Take the binding list produced by samep1, and make it suitable to be
;; returned.  There are two things that need to be done:
;;  1.  Any variable bound to itself should be pruned.
;;  2.  Any sequence variable is in fact bound not to a list, but to the
;;  value it represents (it can't be bound to a list of values -- this
;;  is samep, not unifyp).  So we have to bind it to the *list*
;;  containing this value.

(defun samep-answer (blist)
  (list (delete-if #'(lambda (x)
		       (cond ((eq (car x) (cdr x)))	;x bound to x
			     ((varp* (car x)) (rplacd x (list (cdr x))) nil)))
						;if x is * var, adjust
						;its binding and then
						;don't remove it
		   blist)))

;; The algorithm should be familiar -- walk down the two lists,
;; accumulating a binding list.  If p is an atom (this includes NIL),
;; then check to see if q has the same variable type as p (in which case
;; bind them to each other if possible); if neither is a variable, make
;; sure they are equal.  If only one is an atom, fail.  If neither is an
;; atom, recur.

(defun samep1 (p q)
  (cond ((atom p)				; catches nil also
	 (case (vartype p) 
	   (? (and (varp? q) (samep-var p q))) 
	   (?* (and (varp* q) (samep-var p q)))
	   (t (eql p q))))
	((atom q) nil)
	((samep1 (car p) (car q))
	 (samep1 (cdr p) (cdr q)))))

;; samep-var checks to see if p can be bound to q by seeing if either p
;; or q has already been bound.  If so, then the binding had better be
;; the same; if not, stick a new cons cell describing the binding onto
;; the binding list being accumulated.  It's ok to use eq instead of eql
;; because p and q are both symbols.

(defun samep-var (p q &aux bdg)
  (declare (special blist))
  (cond ((setq bdg (assoc p blist))
	 (eq (cdr bdg) q))
	((setq bdg (rassoc q blist))
	 (eq p (car bdg)))
	(t (push (cons p q) blist))))

;; One sided unifier that determines whether q is an instance of p.
;; Like UNIFY, passes around a set of binding lists to allow for the
;; possibility that multiple answers can result from the presence of *
;; variables.  The parameter rflg is used to indicate whether or not the
;; lists are reversed.  reversal is done for efficiency whenever a
;; non-terminal * variable is encountered.  Ignores atomic cdrs; i.e.
;; treats them as if they were nil.  A list of binding lists is returned.

(defun instp (p q)
  (inst-1 p q (list nil) nil))

;; the work is actually done here.  p and q can be assumed to be lists.
;; If q is NIL, then in order for it to be an instance of p, (car p)
;; must be a sequence variable, which is then bound to NIL and NIL must
;; be an instance of (cdr p) as well.

;; In general, if (car p) is a cons, we basically need to recur,
;; checking first that (car q) is also a cons and then doing the
;; recursion.  If (car p) is an atom, then:
;;  1.  If it is a sequence variable, call inst*var
;;  2.  If it is a normal variable, then provided that (car q) is not a
;;  sequence variable (in which case we fail), bind (car p) to (car q)
;;  and continue.
;;  3.  If it is not a variable, but is eql to (car q), simply recur.

(defun inst-1 (p q blists rflg)
  (cond ((null p) (unless q blists))	;to catch p and q both NIL
	((null q)
	 (and (varp* (car p))
	      (setq blists (inst?var (car p) nil blists))
	      (inst-1 (cdr p) nil blists rflg)))
	((consp (car p))
	 (and (listp (car q))		;to catch (car q) nil
	      (setq blists (inst-1 (car p) (car q) blists nil))
	      (inst-1 (cdr p) (cdr q) blists rflg)))
	(t (case (vartype (car p))
	     (?* (inst*var p q blists rflg))
	     (? (when (and (not (varp* (car q)))
			   (setq blists (inst?var (car p) (car q) blists)))
		  (inst-1 (cdr p) (cdr q) blists rflg)))
	     (t (and (eql (car p) (car q))
		     (inst-1 (cdr p) (cdr q) blists rflg)))))))

;; Handles the unification for star variables.  The first element of p
;; is guaranteed to be a * variable.  There are the following three
;; cases:
;;  1.  If (cdr p) is NIL, then the given * variable is all there is, so
;;  just call inst?var to do the actual unification.
;;  2.  If rflg is NIL, then this is the first * variable, and you can
;;  get away with simply reversing the two lists and trying again.
;;  3.  If rflg is T, then you have to walk down q, trying inst at each
;;  point.  This is done in a do loop, where qhead is the stuff being
;;  matched to (car p) and q contains what's left in q.  For each point
;;  in q, we call inst-1 to try to match the rest of p with the rest of
;;  q and if this succeeds, try to add the variable binding for (car p)
;;  to the result.

;; Note that qhead in the following routine is apparently reversed from its
;; true value.  In fact, this is as it should be, since the lists themselves
;; were presumably reversed when the first * variable was encountered.

(defmacro pushconc (c l) `(setf ,l (nconc ,c ,l)))

(defun inst*var (p q blists rflg)
  (cond ((null (cdr p)) 
	 (inst?var (car p) (if rflg (reverse q) q) blists))
	(rflg
	 (do (newblists qhead) (nil)
	   (pushconc (inst?var (car p) qhead (inst-1 (cdr p) q blists t))
		     newblists)
	   (unless q (return newblists))
	   (push (pop q) qhead)))	   
	(t (inst-1 (reverse p) (reverse q) blists t))))

;; inst?var actually adds a new variable binding to the binding lists
;; being accumulated.  The variable is in var, and the binding is q.
;; The binding lists that have been collected so far are in blists.
;; If the variable has already been bound, then the binding had better
;; be the same.  If it hasn't been bound, just push the binding onto the
;; accumulated list.

(defun inst?var (var q blists &aux newblists bdg)
  (dolist (blist blists newblists)
    (setq bdg (assoc var blist))
    (cond ((null bdg) (push (acons var q blist) newblists))
	  ((equal (cdr bdg) q) (push blist newblists)))))

;; General unification function and matcher.  Both call unify to do the
;; unification, then call subst-bdgs to fully instantiate the binding
;; lists and to check for any loops.  MATCHP is slightly different from
;; UNIFYP in that it must make sure new variables are created for any
;; database variables that end up free in the binding list.  It does
;; this by adding extra binding pairs for database variables to the
;; binding list before substitution.  After substitution MATCHP returns
;; a list of conses, the cars of which are the bindings for the
;; variables that appear in the query, and the cdrs of which are the
;; bindings generated for database variables.

;; In order that these functions be as efficient as possible, they
;; attempt to reuse as many conses as possible.  To make the code clear
;; while this is going on, these macros have been defined for pushing
;; and popping cells off of lists in ways that allow the conses to be
;; reused:

(defmacro popcell (l) `(rplacd (prog1 ,l (setq ,l (cdr ,l))) nil))

(defmacro pushcell (c l) `(setq ,l (rplacd ,c ,l)))

;; The unifier works by first calling unify, which does the basic
;; unification but may return binding lists with loops or incomplete
;; bindings (e.g., x is bound to y and y is then bound to z).  In order
;; to deal with these possibilities, we work through the list of binding
;; lists returned, calling subst-bdgs on each one to simplify it.  Since
;; subst-bdgs is destructive, we have to copy the binding list unless
;; it's the last one we need to do (in which case we don't mind
;; destroying it).

;; subst-bdgs returns FAIL if there is an internal conflict in the
;; binding list.  Thus we first check to see if the binding is empty (in
;; which case we just push it on to the list unchanged); if it isn't we
;; try subst-bdgs.  Then we just remove any FAILs from the result.

(defvar unify*flg)			;binding lists must be copied
;; (defvar unify-dummies)			;list of dummy variables
;; ^^^^^^^^^^^^^^^^^^^^^^ commented out 5/18/93 by Don Geddis (moved to top)
(defvar nasty-unify)			;variables may be in wrong order?

(defun unifyp (p q &aux unify-dummies (blists (unify p q)))
  (cond ((car blists)
	 (napcar #'copy-alist (cdr blists))
	 (napcar #'subst-bdgs blists)
	 (delete 'fail blists))
	(t blists)))

;; the matcher is much the same, but matches an expression to a database
;; sentence (which is referenced by its proposition symbol).  There are
;; a couple of additional subtleties, though:

;; 1.  We want to bind the database variables and not the variables in
;; the query wherever possible.  check-order is responsible for fiddling
;; with the binding lists to make this happen; nasty-unify is a flag
;; indicating that we couldn't get by without it being a possibility in
;; the unification phase.

;; 2.  We don't want to have things bound to the uninterned symbols in
;; the database sentence, so we next bind each database variable to a new
;; variable.  We simplify as for unify, and then split the bindings into 
;; those appearing in the query and those appearing in the database.

(defun matchp (exp datum &aux unify-dummies nasty-unify
			      (blists (unify (denotes datum) exp)))
  (cond ((car blists)
	 (when nasty-unify
	   (mapc #'(lambda (blist)
		     (napcar #'(lambda (b) (check-order b blist)) blist))
		 blists))
	 (let ((db (get datum 'vars-in)))
	   (napcar #'copy-alist (cdr blists))
	   (napcar #'(lambda (b) (match-db-vars b db)) blists)
	   (napcar #'divide-bdgs (delete 'fail blists))))
	(t blists)))

;; Here we check the order of the variables appearing in a particular
;; binding list.  If either the car is a database variable or the cdr
;; is bound, we don't have to worry.  (If the cdr is bound, the binding
;; will be simplified away.)  Now there are two cases:
;; 1.  If the car is a *var, and is bound to a list consisting of a
;;     *var alone, and the second *var is a database variable, we have
;;     to swap them.
;; 2.  If the car is a normal var, it's much the same.

(defun check-order (bdg blist)
  (cond ((or (database-variable (car bdg)) (assoc (cdr bdg) blist))
	 bdg)
	((varp? (car bdg))
	 (if (and (symbolp (cdr bdg)) (database-variable (cdr bdg)))
	     (cons (cdr bdg) (car bdg))
	   bdg))
	((and (symbolp (second bdg)) (database-variable (second bdg)))
	 (list (second bdg) (car bdg)))
	(t bdg)))

;; Here we replace the database variables with new variables.  Any unbound
;; database variable (there is a list in vars) gets bound to a new variable
;; before we call subst-bdgs to sort it all out.

(defun match-db-vars (bdgs vars)
  (dolist (var vars (subst-bdgs bdgs))
    (unless (assoc var bdgs)
      (push (if (varp* var) (list var (new-*var)) (cons var (new-?var)))
	    bdgs))))

;; And here we split the binding list into a database set and a query set.
;; We do it without consing up a whole bunch of new cells.

(defun divide-bdgs (bdgs)
  (do (bdg query database) 
      ((null bdgs) (cons query database))
    (setq bdg (popcell bdgs))
    (if (database-variable (caar bdg))
	(pushcell bdg database)
	(pushcell bdg query))))

;; Unifier that handles star variables.  Passes around a set of binding
;; lists to allow for the possibility that multiple answers can result
;; from the presence of * variables.  The parameter rflg is used to
;; indicate whether or not the lists are reversed.  reversal is done
;; for efficiency whenever a non-terminal * variable is encountered.
;; Note:  requires post pass to eliminate all variable loops and to
;; simplify variable bindings.  There is one other catch -- when possible,
;; we would like to bind the variables in p and not those in q.  This
;; functionality is needed by the matcher.  The special variable
;; nasty-unify is used to record the fact that we may have failed to
;; do this.

(defun unify (p q &aux unify*flg)
  (if (or (atom p) (atom q)) (setq p (list p) q (list q)))
  (unify-1 p q (list nil) nil))

;; Here we go.
;;  1.  If p and q are eq, you're done.
;;  2.  If p is NIL, then q needs to be a list starting with a *
;;  variable, in which case we try to unify (car q) with NIL and (cdr q)
;;  with NIL if that succeeds.
;;  3.  If q is NIL, it's much the same.
;;  4.  If the car's are eql, we unify the cdrs.
;;  5.  If the car's are both lists, we unify them and then the cdrs.
;;  6.  If the car of p is a variable, then:
;;   6a.  If the car of q is a sequence variable, we call unify*var
;;   6b.  Otherwise, we just try to set (car p) to (car q) and continue
;;  7.  If the car of p is a sequence variable, we call unify*var
;;  8.  If the car of p is not a variable, then if the car of q is a
;;   variable of any type, we treat it as in 6,7

(defun unify-1 (p q blists rflg)
  (cond ((eq p q) blists)
	((null p)
	 (and (varp* (car q))
	      (setq blists (unify?var (car q) nil blists))
	      (unify-1 (cdr q) nil blists rflg)))
	((null q)
	 (and (varp* (car p))
	      (setq blists (unify?var (car p) nil blists))
	      (unify-1 (cdr p) nil blists rflg)))
	((eql (car p) (car q)) 
	 (unify-1 (cdr p) (cdr q) blists rflg))
	((and (listp (car p)) (listp (car q)))
	 (and (setq blists (unify-1 (car p) (car q) blists nil))
	      (unify-1 (cdr p) (cdr q) blists rflg)))
	(t (case (vartype (car p))
	     (? (cond ((varp* (car q))
		       (setq nasty-unify t)
		       (unify*var q p blists rflg))
		      ((setq blists (unify?var (car p) (car q) blists))
		       (unify-1 (cdr p) (cdr q) blists rflg))))
	     (?* (unify*var p q blists rflg))
	     (t (case (vartype (car q))
		  (? (when (setq blists (unify?var (car q) (car p) blists))
		       (setq nasty-unify t)
		       (unify-1 (cdr p) (cdr q) blists rflg)))
		  (?* (setq nasty-unify t)
		      (unify*var q p blists rflg))))))))

;; Handles the unification for star variables.  The first element of p
;; is guaranteed to be a * variable.  The situation is complicated by
;; the fact that if the variable is already bound, we want to plug for
;; it here and not risk the looping that happens when two * variables
;; meet.  So we have to do the analysis for each binding list in blists.
;; There are the following four cases:
;;  1.  If (cdr p) is NIL, then the given * variable is all there is, so
;;  just call unify?var to do the actual unification.
;;  2.  If (car q) is a * var and (cdr q) is NIL, we treat it like (1).
;;  3.  If rflg is NIL, then this is the first * variable, and we can
;;  get away with simply reversing the two lists and trying again.
;;  4.  Otherwise we look through the blists to bind p if possible:
;;    4a.  If p is bound, then we stick its binding onto the cdr
;;  	   of p and repeat the call.
;;    4b.  If q does not begin with a * variable, things are somewhat 
;;         simpler; we have to walk down q, trying unify at each point.
;;         This is done by unify*loop.
;;    4c.  If q is bound, we plug in the binding as in case 1.
;;    4d.  The last case is nasty and unify-** is responsible for sorting
;;         it out.

(defun unify*var (p q blists rflg &aux qstar)
  (cond ((null (cdr p)) (unify?var (car p) (if rflg (reverse q) q) blists))
	((and (setq qstar (varp* (car q))) (null (cdr q)))
	 (setq nasty-unify t)
	 (unify?var (car q) (if rflg (reverse p) p) blists))
	(rflg
	 (setq unify*flg t)
	 (do (newblists)
	     ((null blists) newblists)
	   (let* ((blist (popcell blists))
		  (bdg (find*var (car p) blist)))
	     (pushconc
	      (cond (bdg (unify-1 (revappend (cdr bdg) (cdr p)) q blist t))
		    (qstar
		     (let ((qbdg (find*var (car q) blist)))
		       (if qbdg
			   (unify-1 p (revappend (cdr qbdg) (cdr q))
				    blist rflg)
			 (unify-** p q blist))))
		    (t (unify*loop p nil q blist)))
	      newblists))))
	(t (unify-1 (reverse p) (reverse q) blists t))))

;; Here we handle the loop where p begins with a * variable and q
;; doesn't.  qhead is the reverse of the stuff being matched to (car p)
;; and qtail is what's left in q.

;; It's pretty simple, except if q contains a * variable itself.  This
;; case is handled after the * variable is pushed onto qhead, so if it's
;; on qtail we don't have to do anything.  In the "simple" case where qtail
;; begins with something other than a * variable, we just call unify-1
;; to unify qtail with the cdr of p and bind the * variable beginning p
;; to all of qhead.  (If there is a * variable at the beginning of the
;; reversed qhead, this operation is a special case of the first clause
;; of the main cond below.)

;; To handle the case where there is a * variable in q, we have to 
;; allow for the possibility that (car p) only unifies with a *part* 
;; of this * variable (toegether with all of qhead, of course).  This 
;; is done by splitting the * variable into two new variables, and 
;; assuming that (car p) only includes the first.

;; There are two additional subtleties.  First, the new * variables created
;; are just dummies, and we want to be able to get rid of them at the end of
;; the unification process if possible.  So instead of invoking
;; (new-*var) to create them, we invoke (new-dummy-var), which records
;; their dummy status.

;; Second, we never "resplit" a * variable.  This is not complete, but
;; otherwise the unifier may loop in a variety of situations (which was
;; felt to be worse).

(defun unify*loop (p qhead qtail blists &aux newblists)
  (loop
    (cond ((member (car qhead) unify-dummies) nil)	;don't resplit *var
	  ((varp* (car qhead))
	   (pushconc
	    (let ((v1 (new-dummy-var)) (v2 (new-dummy-var)))
	      (unify-1 (cdr p) (cons v1 qtail)
		       (unify?var (car p) (cons v2 (cdr qhead))
				  (unify?var (car qhead) (list v1 v2)
					     (copy-list blists)))
		       t))
	    newblists))
	  ((not (varp* (car qtail)))
	   (pushconc (unify-1 (cdr p) qtail 
			      (unify?var (car p) qhead (copy-list blists)) t)
		     newblists)))
    (unless qtail (return newblists))
    (push (pop qtail) qhead)))

;; p and q both begin with * variables.  Now there are two
;; possibilities:
;;  1.  (car p) unifies with (car q) and perhaps a little more
;;  2.  (car q) unifies with (car p) and a little more
;; Of course, by "a little more", we mean some portion of the tail of
;; the list being considered -- which may mean a *part* of some
;; subsequent * variable.  Thus if ?*x is unifying with the car of
;; (?*y ?*z) and a little more, it might unify with ?*y and some *part*
;; of ?*z and not the whole thing.

;; To handle this (admittedly bizarre) case, when we decide to unify
;; with a "little more" and this little more ends in a * variable, we
;; split that * variable into two parts, including the first in the
;; little more and leaving the rest to unify with the remainder of the
;; expression.  But now note that since the first of these parts can
;; unify with nil (in which case it is just the same as if the *
;; variable wasn't included in the "little more" after all), there is no
;; point in ending the "little more" just before a sequence variable.

(defun unify-** (p q blists)
  (nconc
   (unify-1 (cdr p) (cdr q)
	    (unify?var (car p) (list (car q)) (copy-list blists)) t)
   (unify-**-1 p q (copy-list blists))
   (unify-**-1 q p (copy-list blists))))

;; here is the basic function; (car p) is assumed to unify with (car q)
;; and a little more.  But this is just what unify*loop does; we have to
;; be sure to move the first two elements of q to qhead and leave the
;; rest as qtail.

(defun unify-**-1 (p q blists)
  (when (cdr q)
    (unify*loop p (list (second q) (first q)) (cddr q) blists)))

(defun new-dummy-var (&aux (var (new-*var)))
  (push var unify-dummies)
  var)

;; modify a collection of binding lists to indicate that the variable
;; var is bound to the value q.  If unify*flg is T, then blists cannot
;; be destroyed in the process.

;; We proceed through blists one at a time.  For each blist, we check to
;; see if var is already bound; find?var returns q if it isn't and the
;; existing binding if it is.  There are therefore the following cases:
;;  1.  If the value returned is q (for whatever reason), then
;;  everything is fine and we just pass this blist off as done.
;;  2.  If the value returned is "badloopcheck", then we have
;;  essentially hit an occurcheck problem, in that the variable is
;;  already being investigated by a higher-level call to unify?var.  In
;;  this case, we give up.
;;  3.  If both the current and desired values are lists, then we have
;;  to try to unify them.  We mark the variable under consideration as
;;  "badloopcheck" (see 2 above), and invoke the unifier recursively.
;;  Then we reset the old value to which the variable is bound, and add
;;  the result of the unification call to the growing list of new
;;  answers.  (We do these two steps in the reverse order.)
;;  4.  If the desired value is a variable, then we basically do things
;;  in the reverse order, finding the value to which *q* is bound.  This
;;  leads to:
;;   4a.  If q is bound (for whatever reason) to the desired value,
;;   proceed with success.
;;   4b.  If q is bound to badloopcheck by a higher-level unification
;;   call, give up.  (This happens by default.)
;;   4c.  If both var's value and q's value are lists, try to unify
;;   them as in (3).

(defun unify?var (var q blists)
  (do (blist bdg val valq newblists) 
      ((null blists) newblists)
    (setq blist (popcell blists))		;strip off first blist
    (setq bdg (if unify*flg (find?var var q blist) (nfind?var var q blist))
	  val (cdr bdg))
    (cond ((eql val q) 
	   (pushcell blist newblists))
	  ((eq val '|BADLOOPCHECK|))
	  ((and (consp val) (consp q))
	   (rplacd bdg '|BADLOOPCHECK|)
	   (pushconc (unify-1 val q blist nil) newblists)
	   (rplacd bdg val))
	  ((varp q) 
	   (setq bdg (if unify*flg (find?var q val blist) 
		       (nfind?var q val blist))
		 valq (cdr bdg))
	   (cond ((eql val valq) (pushcell blist newblists))
		 ((and (consp val) (consp valq))
		  (rplacd bdg '|BADLOOPCHECK|)
		  (pushconc (unify-1 val valq blist nil) newblists)
		  (rplacd bdg valq)))))))

;; Find the value to which a variable is bound.  The binding list
;; examined is (car blist).

;; We first look for var's binding.  If it has none, we add a dotted
;; pair (var . q) giving the desired binding to the front of the
;; binding list, and return that.

;; If var is bound to a variable, then we basically just recur, trying
;; to find the binding for *that* variable.  The only catch is that if
;; we encounter the given variable again, we should just note that
;; we've hit a loop and bind either one of the two variables to the
;; desired value.  In practice, the variable var is bound to q.

;; If var is bound to something other than a variable, we just return
;; that.

;; There are two routines here, one for * variables (that returns nil if
;; the * variable is unbound, and the binding otherwise), and one for
;; normal variables, that successfully binds the variable to the given
;; value if there is no other binding.

(defun find*var (var blist &aux (bdg (assoc var (car blist))) (val (cdr bdg)))
  (cond ((null bdg) nil)		;failure -- no existing binding
	((and (consp val)
	      (null (cdr val)) 
	      (varp* (car val)))	;another * variable?
	 (prog2
	     (rplacd bdg '|VARLOOPCHECK|)
	     (or (find*var (car val) blist) bdg)
	   (rplacd bdg val)))
	((eq val '|VARLOOPCHECK|)	;variable loop -- hit loopcheck
	 (setf (car blist) (remove bdg (car blist)))
	 nil)
	(t bdg)))

(defun find?var (var q blist &aux (bdg (assoc var (car blist)))
				  (val (cdr bdg)))
  (cond ((null bdg)			;success -- no existing binding
	 (car (push (cons var q) (car blist))))
	((consp val) bdg)		;existing binding to a list
	((eql q val) bdg)		;existing binding to same value
	((eq val '|VARLOOPCHECK|)	;variable loop -- remove current
					;binding for var and set up a new one
	 (setf (car blist) (remove bdg (car blist)))
	 (car (push (cons (car bdg) q) (car blist))))
	((varp val)			;bound to a variable but no loop yet
	 (prog2
	     (rplacd bdg '|VARLOOPCHECK|)
	     (find?var val q blist)
	   (rplacd bdg val)))
	(t bdg)))			;bound to a constant -- return it

;; Destructive version of the above routine.  As it unwinds, nfind?var
;; destructively substitutes the final value into any intermediate
;; variables.

(defun nfind?var (var q blist &aux (bdg (assoc var (car blist)))
				   (val (cdr bdg)))
  (cond ((null bdg) 
	 (car (push (cons var q) (car blist))))
	((consp val) bdg)
	((eql q val) bdg)
	((eq val '|VARLOOPCHECK|)	;bind it destructively
	 (rplacd bdg q))
	((varp val)
	 (rplacd bdg '|VARLOOPCHECK|)
	 (setq val (nfind?var val q blist))
	 (cond ((eq bdg val) val)
	       ((atom (cdr val))
		(rplacd bdg (cdr val)))	;bind intermediates
	       (t (rplacd bdg (car val))
		  val)))
	(t bdg)))

;; Functions that check for loops in the binding list and do complete
;; binding list substitution.  Returns FAIL if the check fails.
;; Otherwise, returns the substituted binding list.

;; This routine works by working down the given binding list, one
;; binding at a time.  A list of equivalent variables is maintained in
;; equivalent-bdgs; a list of the variables that have been accumulated
;; so far is kept in finished-bdgs (so finished-bdgs is returned at the
;; end, after removing any bindings to dummy variables).

(defun subst-bdgs (working-bdgs &aux equivalent-bdgs finished-bdgs pending*)
  (declare (special working-bdgs equivalent-bdgs finished-bdgs pending*))
  (catch 'no-unify
    (do (temp) 
	((null working-bdgs) (remove-dummy-bindings finished-bdgs))
      (setq temp (popcell working-bdgs))	;chop a cell from working-bdgs
      (subst-bdg temp))))

;; Process a single binding.  There are five possibilities:

;;  1. If the variable is being bound to another variable (this will
;;  only happen with regular variables; * variables will always be bound
;;  to lists), add the variable to equivalent-bdgs and find the value
;;  for the variable using subst-var.

;;  2. If the variable is bound to an atom (including nil) move the
;;  binding to finished-bdgs.

;;  3. If the variable is a star variable and is bound to a list
;;  consisting of another star variable then treat it the same way as
;;  in case 1.

;;  4. If the variable is not a * variable and is being bound to a list,
;;  then process the given list to see (destructively) what *its*
;;  variables are bound to.  This is done by subst-term; when we
;;  call subst-term, we mark the given variable to make sure no binding
;;  loops occur.

;;  5. If the variable is a * variable and is bound to a list, it's a
;;  little harder.  We still want to do basically the same thing, but
;;  it is possible that the target list includes the given * variable
;;  without there being a conflict if the only other things on the target
;;  list are * variables that can be bound to NIL; consider unifying
;;  (?*1) with (?*1 ?*2).  To handle this, we invoke subst-term *without*
;;  pushing the given cell onto the list of finished bindings *or*
;;  marking the variable loop.  In the process, the original * variable
;;  will have been "forgotten" and therefore not rebound.  There are
;;  now three possibilities:

;;  5a. If the original * variable does not appear in the tree, it's ok
;;  to return it.
;;  5b. If it does appear in the tree, then it has to appear at the
;;  top level only, and everything else has to be a *var that can be
;;  bound to NIL.
;;  5c. Otherwise, fail.

;;  To make the check in (5a) faster, we do the whole process, keeping
;;  a list of *vars that are pending.  Whenever we try to find a binding
;;  for a *var and there isn't one (i.e., if it's pending, we just hit a
;;  loop), we delete the *var from the list.  Then we can just check to
;;  see if it's on the list to check (5a), avoiding the need to walk
;;  through the tree.

(defun subst-bdg (cell &aux (bdg (car cell)) (val (cdr bdg)))
  (declare (special equivalent-bdgs finished-bdgs pending*))
  (cond ((varp val)
	 (rplacd bdg '|BADLOOPCHECK|)
	 (rplacd bdg (subst-var val))
	 (unless (eq (car bdg) (cdr bdg)) (pushcell cell equivalent-bdgs))
	 (when equivalent-bdgs
	   (pushconc equivalent-bdgs finished-bdgs)
	   (setq equivalent-bdgs nil)))
	((atom val)
	 (pushcell cell finished-bdgs))
	((and (null (cdr val)) (varp* (car bdg)) (varp* (car val)))
	 (rplacd bdg '|BADLOOPCHECK|)
	 (let ((newval (subst-var (car val))))
	   (rplacd bdg (if (listp newval) newval val)))
	 (unless (eq (car bdg) (second bdg)) (pushcell cell equivalent-bdgs))
	 (when equivalent-bdgs
	   (pushconc equivalent-bdgs finished-bdgs)
	   (setq equivalent-bdgs nil)))	  
	((consp val)
	 (cond ((varp* (car bdg))
		(push (car bdg) pending*)
		(let ((term (subst-term val)))
		  (cond ((eq (car bdg) (car pending*))
			 (pop pending*)
			 (pushcell cell finished-bdgs)
			 (rplacd bdg term))
			(t (subst-bdg-special-case bdg term)))))
	       (t (pushcell cell finished-bdgs)
		  (rplacd bdg '|BADLOOPCHECK|)
		  (rplacd bdg (subst-term val))))))
  bdg)

;; Here is where we handle the cases (5b) and (5c) above.  Since the
;; term to which the binding has been reduced has all the variables
;; substituted in, any remaining variable is free for substitution.
;; So we have to check that every element of the term is a *var,
;; then it's easy.  We bind them all to nil and also bind the *given*
;; *var to nil if it appears multiple times, as in
;; (unifyp '(?*1 ?*2 ?*2) '(?*2)).

(defun subst-bdg-special-case (bdg term)
  (declare (special finished-bdgs))
  (cond ((notevery #'varp* term)
	 (throw 'no-unify 'fail))
	(t (let ((nil* bdg) (var (car bdg)) found1 found2)
	     (setf (cdr bdg) nil)
	     (dolist (v term (unless found2 (setf (cdr bdg) (list (car bdg)))))
	       (cond ((member v nil*)
		      (when (eql v var)
			(cond (found2)
			      (found1 (setq found2 t)
				      (push bdg finished-bdgs))
			      (t (setq found1 t)))))
		     (t (push v nil*)
			(push (list v) finished-bdgs))))))))

;; This routine finds the value for the variable var.  If a finished
;; binding is known for var, then either there is a variable loop (in
;; which case we give up), or we return that binding.  Otherwise, we:
;;  1. Find the value on the working binding-list
;;  2. Splice the binding out of the working binding list
;;  3. Call subst-bdg on the bdg to find out the value
;;  4. Return the fully substituted value
;; If there is no binding for the variable anywhere and it's a *var,
;; we remove it from the list of pending *vars as described in subst-bdg.

(defun subst-var (var &aux (bdg (assoc var finished-bdgs)) cell)
  (declare (special working-bdgs finished-bdgs pending*))
  (cond (bdg (if (eq (cdr bdg) '|BADLOOPCHECK|)
		 (throw 'no-unify 'fail)
	       (cdr bdg)))
	((setq cell (member var working-bdgs :key #'car))
	 (cond ((cdr cell)			;complex version of
		(setq bdg (car cell))		;(setq working
		(rplaca cell (cadr cell)) 	;  (delete bdg working))
		(setq bdg (rplaca (cdr cell) bdg))
		(rplacd cell (cddr cell))
		(setq cell bdg))
	       (t (setq working-bdgs (nbutlast working-bdgs))))
	 (cdr (subst-bdg cell)))
	(t (when (varp* var) (setq pending* (delete var pending* :count 1)))
	   var)))

(defun subst-term (term)
  (when term
    (case (vartype (car term))
      (? (reuse-cons (subst-var (car term)) (subst-term (cdr term)) term))
      (?* (let ((val (subst-var (car term))))
	    (cond ((null val) (subst-term (cdr term)))
		  ((atom val)		;no bdg for the var
		   (reuse-cons (car term) (subst-term (cdr term)) term))
		  (t (append val (subst-term (cdr term)))))))
      (t (reuse-cons (if (listp (car term)) (subst-term (car term)) (car term))
		     (subst-term (cdr term))
		     term)))))

(defun remove-dummy-bindings (bdgs)
  (if unify-dummies
      (delete-if #'(lambda (x) (member x unify-dummies)) bdgs :key #'car)
      bdgs))
