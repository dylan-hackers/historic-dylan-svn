;;;; -*- mode:Lisp; syntax:Common-Lisp; package:user -*- ;;;;
;;;; Copyright 1992 Patrick H. Winston.  All rights reserved.
;;;; Version 1.1.1, copied from master file on 23 Apr 93       
;;;; 
;;;; This software is licensed by Patrick H. Winston (licensor) for
;;;; instructional use with the textbooks ``Artificial Intelligence,'' by
;;;; Patrick H. Winston, and ``Lisp,'' by Patrick H. Winston and Berthold
;;;; K. P. Horn.  Your are free to make copies of this software and
;;;; modify it for such instructional use as long as:
;;;; 1. You keep this notice intact.
;;;; 2. You cause any modified files to carry a prominent notice stating
;;;;    that you modified the files and the date of your modifications.
;;;; This software is licensed ``AS IS'' without warranty and the licensor
;;;; shall have no liability for any alleged defect or damages.

#|

The procedures in this fill offer a small subset of the functionality
of the Common Lisp Object System (CLOS).

Note that the emphasis is on simplicity, not speed.  Accordingly,
values are kept on property lists, and methods are kept on
association lists.

Only simple versions of DEFCLASS, MAKE-INSTANCE, and DEFMETHOD
are provided; :around :before, :primary, and :after methods are
provided.

Methods can be specialized to instances using the following in
the position normally occupied in a DEFMETHOD form by a class name:
(EQL <form evaluating to instance>).

SETF methods are allowed using the following in the position
normally occupied in a DEFMETHOD form by a method name:
(setf <method name>)

The procedures are meant to constitute an abstraction layer.
Hence no effort hs been made to comment them properly.

|#

;;;;  DYNAMIC VARIABLES

(defvar *around-methods*)
(defvar *before-methods*)
(defvar *primary-methods*)
(defvar *after-methods*)
(defvar *args*)

;;;; METHOD DEFINITION AND INTERPRETATION

(defmacro defmethod (&rest x)
  (delete-if #'stringp x)
  (let* ((name (pop x))
	 (type (if (symbolp (first x)) (pop x) :primary))
	 (args (pop x))
	 (body x)
	 (arglist (mapcar #'(lambda (arg) (if (listp arg) (first arg) arg))
			  args)))
    (when (listp name)
      (if (eq 'setf (first name))
	  (setf name (make-setf-symbol (second name)))
	(error "~a is not a recognized DEFMETHOD name." name)))
    `(progn
       ;;If generic function has been defined, just add the method:
       (unless (or (get ',name :before)
		   (get ',name :primary)
		   (get ',name :after)
		   (get ',name :around))
	 ;;Following modified to accomodate SETF better, 21 Nov 92
	 (defun ,name (,@arglist) (process-methods ',name (list ,@arglist))))
       (add-method
	 ',name
	 ',(mapcar
	      #'(lambda (arg)
		  (if (listp arg)
		      (let ((class (second arg)))
			;;Changed 25 Sep 90 to allow instance specialization:
			(if (listp class)
			    (if (and (eq 'eql (first class)) (rest class))
				(eval (second class))
			      (error "Wrong specialization syntax in ~
				     definition of ~a" name))
			  class))
		    t))
	      args)
	 (function (lambda ,arglist ,@body))
	 ',type)
       ',name)))

(defun process-methods (name args &aux result (*args* args))
  (let* ((*around-methods* (fetch-methods name args :around))
	 (*before-methods* (fetch-methods name args :before))
	 (*primary-methods* (fetch-methods name args :primary))
	 (*after-methods* (fetch-methods name args :after)))
    (call-next-method)))

(defun call-next-method ()
  (if *around-methods*
      (apply (pop *around-methods*) *args*)
    (progn
      (do () ((endp *before-methods*))
	(apply (pop *before-methods*) *args*))
      (multiple-value-prog1
	  (if *primary-methods*
	      (apply (pop *primary-methods*) *args*)
	    (error "Oops, no applicable primary method!"))
	(do () ((endp *after-methods*))
	  (apply (pop *after-methods*) *args*))))))

(defun add-method (name key function type)
  (let* ((methods (get name type))
	 (current (assoc key methods :test #'equal)))
    (when methods
      (unless (= (length key) (length (first (first methods))))
	(error "Ooops new ~a method has inconsistent number of arguments."
	       name)))
    (if current
	(progn
	  #+comment
	  (format t "~%Redefining ~a method." name)
	  (setf (second current) function)
	  name)
      (push (list key function) (get name type)))))

;;;; METHOD REMOVAL

(defun remove-methods (name)
  (setf (get name :primary) nil)
  (setf (get name :before) nil)
  (setf (get name :after) nil)
  (setf (get name :around) nil))

;;;; METHOD FILTERING AND ARRANGING

(defun fetch-methods (name args type)
  (let ((precedence-lists
	  (mapcar #'(lambda (arg class)
		      ;;Allow specialization to instances:
		      (cons arg
			    (or (get class 'precedence-list)
				(make-precedence-list class))))
		  args
		  (mapcar #'get-argument-class args)))
	(methods (get name type)))
    (setf methods
	  (remove-if-not
	    #'(lambda (pair) (applicable-p (first pair) precedence-lists))
	    methods))
    (setf methods
	  (sort methods
		#'(lambda (x y)
		    (higher-p (first x) (first y) precedence-lists))))
    (mapcar #'second methods)))

(defun higher-p (m1 m2 precedence-lists)
  (if (equal m1 m2)
      nil
    (let ((n1 (position (first m1) (first precedence-lists)))
	  (n2 (position (first m2) (first precedence-lists))))
      (if (= n1 n2)
	  (higher-p (rest m1) (rest m2) (rest precedence-lists))
	(< n1 n2)))))

(defun applicable-p (specializers lists)
  (do ((specializers specializers (rest specializers))
       (lists lists (rest lists)))
      ((endp specializers) t)
    ;;Allow specialization to instances:
    (unless
	(if (listp (first specializers))
	    (eql (second (first specializers)) (first (first lists)))
	  (member (first specializers) (first lists)))
      (return nil))))

(defun get-argument-class (arg)
  (if (and (symbolp arg) (get arg 'is-a))
      (get arg 'is-a)
    (let ((result (type-of arg)))
      (if (listp result) (first result) result))))

;;;; PRECEDENCE LIST CONSTRUCTION

(defun make-precedence-list (object)
  (let ((superclasses (make-relatives-list object 'superclasses)))
    (establish-order
      superclasses
      (mapcar #'(lambda (class)
		  (cons class
			(or (get class 'superclasses) '(t))))
	      superclasses))))

(defun make-relatives-list (class property &aux (classes (list class)))
  (do ((result classes))
      ((null classes) result)
    (let ((probes (get (pop classes) property)))
      (dolist (probe probes)
	(if (eql probe class)
	    (error "Beware! ~a is circular!" class)
	  (progn (pushnew probe result)
		 (pushnew probe classes)))))))

(defun establish-order (classes direct-supers)
  (let ((precedence-list nil)
        (pairs (construct-pairs direct-supers)))
    (loop
      (when (endp classes) (return (reverse (cons t precedence-list))))
      (let* ((candidates (filter-classes classes pairs))
             (winner (filter-candidates candidates
                                        precedence-list
                                        direct-supers)))
        ;;Shrink the list of precedence pairs:
        (setf pairs (filter-pairs pairs winner))
        ;;Move the winning class to the precedence list:
        (setf classes (remove winner classes))
        (push winner precedence-list)))))

(defun construct-pairs (direct-supers)
  (apply #'append (mapcar #'construct-pairs-aux direct-supers)))

(defun construct-pairs-aux (l)
  (if (endp (rest (rest l)))
      (list l)
      (cons (list (first l) (second l))
            (construct-pairs-aux (rest l)))))

(defun filter-classes (classes precedence-pairs &aux result)
  (dolist (class classes)
    (unless (member class precedence-pairs
                    :test
                    #'(lambda (x y) (eq x (second y))))
      (push class result)))
  (case (length result)
    (0 (error "Precedence list for ~%~a~%cannot be computed."
	      precedence-pairs))
    (otherwise result)))

(defun filter-candidates (candidates precedence-list direct-supers)
  (case (length candidates)
    (1 (first candidates))
    (otherwise
     (catch 'found-it
       (dolist (possible-subclass precedence-list)
	 (dolist (candidate candidates)
	   (when (member candidate
			 (assoc possible-subclass
				direct-supers))
	     (throw 'found-it candidate))))))))

(defun filter-pairs (precedence-pairs winner)
  (remove-if #'(lambda (pair) (eq winner (first pair)))
             precedence-pairs))

;;;; CLASS DEFINITION

(defmacro defclass (&rest x)
  (delete-if #'stringp x)
  (let ((class (pop x))
	(superclasses (pop x))
	(slots (pop x)))
    `(progn
       (dolist (s (get ',class 'superclasses))
	 (setf (get s 'downlink) (delete ',class (get s 'downlink))))
       (process-precedence ',class ',superclasses)
       (dolist (slot ',slots) (process-accessor ',class slot))
       (defmethod initialize-initargs :before ((object ,class) (initargs t))
		  (process-initargs object ',slots initargs))
       (defmethod initialize-initforms :before ((object ,class))
		  (process-initforms object ',slots))
       ',class)))

(defun process-initargs (object slots initargs)
  (dolist (slot slots)
    (unless (member (first slot) (symbol-plist object))
      (let ((apointer (member :initarg slot)))
	(when (and apointer (member (second apointer) initargs))
	  (setf (get object (first slot))
		(second (member (second apointer) initargs))))))))

(defun process-initforms (object slots)
  (dolist (slot slots)
    (unless (member (first slot) (symbol-plist object))
      (let ((fpointer (member :initform slot)))
	(when fpointer
	  (setf (get object (first slot))
		(eval (second fpointer))))))))

(defun process-precedence (class superclasses)
  (setf (get class 'superclasses) superclasses)
  (dolist (s superclasses) (pushnew class (get s 'downlink)))
  (dolist (s (make-relatives-list class 'downlink))
    (setf (get s 'precedence-list) (make-precedence-list s))))

(defun process-accessor (class slot-description)
  (let* ((slot (first slot-description))
	 (reader (second (member :accessor slot-description)))
	 (writer (make-setf-symbol reader)))
    (when reader
       (eval `(defmethod ,reader ((object ,class)) (get object ',slot)))
       (eval `(defmethod ,writer ((object ,class) value)
		(setf (get object ',slot) value)))
       (eval `(defsetf ,reader ,writer)))))

(defun make-setf-symbol (symbol)
  (intern (concatenate 'string "SETF-" (string symbol))))

;;;; INSTANCE DEFINITION

(defun make-instance (class &rest args)
  (let ((instance (gentemp (format nil "~a-" class))))
    (setf (get instance 'is-a) class)
    (initialize-initargs instance args)
    (initialize-initforms instance)))

(defmethod initialize-initargs ((object t) (initargs t)) object)

(defmethod initialize-initforms ((object t)) object)

;;;; MISCELLANEOUS ACCESSORS

(defun class-of (x) (get x 'is-a))

(defun precedence-list-of (x) (get x 'precedence-list))

;;;; INSTANCE AND CLASS DISPLAY

(defun show (object)
 (labels
  ((find-width (symbol &aux (result 0))
     (do ((l (symbol-plist symbol) (rest (rest l))))
	 ((endp l) result)
       (let ((w (length (format nil "~a" (first l)))))
	 (when (> w result) (setf result w)))))
   (show-properties (symbol)
    (let ((width (find-width symbol)))
      (do ((l (symbol-plist symbol) (rest (rest l))))
	  ((endp l))
	(unless (eq 'is-a (first l))
	  (format t "~%  ~a:~a  ~a"
		  (first l)
		  (make-string
		    (- width
		       (length (format nil "~a" (first l))))
		    :initial-element #\ )
		  (second l))))))
   (instance-p (symbol) (get symbol 'is-a))
   (class-p (symbol) (get symbol 'superclasses)))
  (cond ((instance-p object)
	 (format t "~%Instance ~a:" object)
	 (show-properties object)
	 (show (get object 'is-a)))
	((class-p object)
	 (format t "~%Class ~a:~%" object)
	 (dolist (l (rest (precedence-list-of object)))
	   (format t "  ~a" l)))
	(t (format t "~%Not an instance or class.")))
  (values)))

