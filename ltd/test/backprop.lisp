
#|----------------------------------------------------------------------------
Artificial Intelligence, Second Edition
Elaine Rich and Kevin Knight
McGraw Hill, 1991

This code may be freely copied and used for educational or research purposes.
All software written by Kevin Knight.
Comments, bugs, improvements to knight@cs.cmu.edu
----------------------------------------------------------------------------|#

#|----------------------------------------------------------------------------
			BACKPROPAGATION ALGORITHM
			 (SINGLE, BINARY OUTPUT)
			     "backprop.lisp"
----------------------------------------------------------------------------|#

#|-----------------------------------------------------------------------------

   Backpropagation algorithm (single, binary output) 
 
   To use this program, you must set up three input files: a ".txt",
   a ".train", and a ".test".  Here is an example:

   File xor.txt:

	Network structure:			(2 2 1)
	Epochs:					801
	Test after every N epochs:		100
	Learning rate (eta):			0.35
	Momentum (alpha):			0.90
	Noise?:					0
	Training data:				xor.train
	Testing data:			        xor.test

   File xor.train:

	0 0 0
	0 1 1
	1 0 1
	1 1 0

   File xor.test:

	0 0 0
	1 0 1

  The main file contains settings for various parameters.  There are 
  separate files for training and testing data.  

  To run, call (backprop "xor.txt") (or whatever the filename is).
  The program will periodically append its results to the end of the 
  "xor.txt" file.  This version of backpropagation is geared toward 
  networks with a single, binary output.  It provides periodic analyses
  of how well the network is predicting the output bit as learning 
  progresses.

----------------------------------------------------------------------------|# 

;;;
;;; Variables.
;;;

(defvar NETWORK nil)
(defvar OUTPUT-LAYER nil)
(defvar STRUCTURE nil)

(defvar TOTAL-EPOCHS 0)
(defvar TEST-INTERVAL 0)

(defvar TRAINING-INPUTS nil)
(defvar TRAINING-OUTPUTS nil)
(defvar TESTING-INPUTS nil)
(defvar TESTING-OUTPUTS nil)

(defvar TOTAL-TRAINING nil)
(defvar TOTAL-TESTING nil)

(defvar TOTAL-INPUTS nil)
(defvar TOTAL-OUTPUTS nil)

(defvar ETA 0)
(defvar ALPHA 0)
(defvar NOISE 0)

(defvar TRAIN-FILE nil)
(defvar TEST-FILE nil)

(defvar TOTAL-GUESSED (make-array 8 :element-type 'float))
(defvar TOTAL-RIGHT (make-array 8 :element-type 'float))

;;;
;;; Structures.
;;;

(defstruct (unit) 
  (weighted-sum 'float) 
  (activation 'float) (delta 'float))
(defstruct (net) 
  units connections size next-layer prev-layer)
(defstruct (connection) 
  (weight 'float) (delta-weight 'float))

(defun output-layer? (layer) 
  (null (net-next-layer layer)))
(defun input-layer? (layer) 
  (null (net-prev-layer layer)))
(defun hidden-layer? (layer) 
  (and (net-next-layer layer) (net-prev-layer layer)))

;;;
;;; Building the network.
;;;

(defun random-real (lo hi) (+ lo (random (- hi lo))))
(defun random-weight () (random-real -0.8 0.8))
(defun random-noise () (random-real 0.0 0.15))

(defun construct-units ()
  (do ((n STRUCTURE (cdr n)) (last-layer nil) (temp-net nil))
      ((null n) (setf OUTPUT-LAYER last-layer))
    (setf temp-net
	  (make-net :units (make-array (1+ (car n)) :element-type 'unit)
		    :connections 
		      (if (cdr n) 
		          (make-array (list (1+ (car n)) (1+ (cadr n)))
				      :element-type 'connection)
			  nil)
		    :prev-layer last-layer))
    (do ((u 0 (1+ u))) ((> u (car n)) nil)
      (setf (aref (net-units temp-net) u) (make-unit)))
    (when (cdr n)
      (do ((u1 0 (1+ u1))) ((> u1 (car n)) nil)
	(do ((u2 1 (1+ u2))) ((> u2 (cadr n)) nil)
	  (setf (aref (net-connections temp-net) u1 u2) (make-connection)))))
    (cond ((= (length n) (length STRUCTURE)) 
	   (setf NETWORK temp-net)
	   (setf (net-size temp-net) (car n))
	   (setf last-layer temp-net))
	  (t 
	   (setf (net-next-layer last-layer) temp-net)
	   (setf (net-size temp-net) (car n))
	   (setf last-layer temp-net)))))

(defun set-initial-weights ()
  (do ((layer NETWORK (net-next-layer layer))) ((null layer) nil)
    (let ((size (net-size layer))
	  (units (net-units layer))
	  (connections (net-connections layer)))
      (do ((u 0 (1+ u))) ((> u size) nil)
	(setf (unit-activation (aref units u)) 0)
	(setf (unit-weighted-sum (aref units u)) 0)
	(setf (unit-delta (aref units u)) 0))
      (when (not (output-layer? layer))
	(let ((next-layer-size (net-size (net-next-layer layer))))
	  (do ((u1 0 (1+ u1))) ((> u1 size) nil)
	    (do ((u2 1 (1+ u2))) ((> u2 next-layer-size) nil)
	      (setf (connection-weight (aref connections u1 u2)) 
                    (random-weight))
	      (setf (connection-delta-weight (aref connections u1 u2)) 
		    0.0)))))))
  (do ((layer NETWORK (net-next-layer layer))) ((null layer) nil)
    (setf (unit-activation (aref (net-units layer) 0)) 1.0)))

(defun build-network ()
  (setf TOTAL-INPUTS (car STRUCTURE))
  (setf TOTAL-OUTPUTS (car (last STRUCTURE)))
  (construct-units)
  (set-initial-weights))

;;;
;;; Building the test and train sets.
;;;

(defun build-test-and-train-sets ()
  (setf TOTAL-TRAINING 0)
  (with-open-file (ifile TRAIN-FILE :direction :input)
    (do ((x (read-line ifile nil 'error) (read-line ifile nil 'error)) (tot 0))
	((or (string-equal "" x) (eq x 'error)) (setf TOTAL-TRAINING tot))
      (when (not (string-equal "" x)) (setf tot (1+ tot)))))
  (setf TOTAL-TESTING 0)
  (with-open-file (ifile TEST-FILE :direction :input)
    (do ((x (read-line ifile nil 'error) (read-line ifile nil 'error)) (tot 0))
	((or (string-equal "" x) (eq x 'error)) (setf TOTAL-TESTING tot))
      (when (not (string-equal "" x)) (setf tot (1+ tot)))))
  (setf TRAINING-INPUTS 
	(make-array (list (1+ TOTAL-TRAINING) (1+ TOTAL-INPUTS)) 
          :initial-element 0.0))
  (setf TRAINING-OUTPUTS 
	(make-array (list (1+ TOTAL-TRAINING) (1+ TOTAL-OUTPUTS)) 
	  :initial-element 0.0))
  (setf TESTING-INPUTS 
	(make-array (list (1+ TOTAL-TESTING) (1+ TOTAL-INPUTS)) 
	  :initial-element 0.0))
  (setf TESTING-OUTPUTS 
	(make-array (list (1+ TOTAL-TESTING) (1+ TOTAL-OUTPUTS)) 
	  :initial-element 0.0))
  (with-open-file (ifile TRAIN-FILE :direction :input)
    (do ((x 1 (1+ x))) ((> x TOTAL-TRAINING) nil)
      (do ((y 1 (1+ y))) ((> y TOTAL-INPUTS) nil)
	(setf (aref TRAINING-INPUTS  x y) (read ifile)))
      (do ((y 1 (1+ y))) ((> y TOTAL-OUTPUTS) nil)
	(setf (aref TRAINING-OUTPUTS x y) (bound-outputs (read ifile))))))
  (with-open-file (ifile TEST-FILE :direction :input)
    (do ((x 1 (1+ x))) ((> x TOTAL-TESTING) nil)
      (do ((y 1 (1+ y))) ((> y TOTAL-INPUTS) nil)
	(setf (aref TESTING-INPUTS  x y) (read ifile)))
      (do ((y 1 (1+ y))) ((> y TOTAL-OUTPUTS) nil)
	(setf (aref TESTING-OUTPUTS x y) (bound-outputs (read ifile)))))))

;; Function BOUND-OUTPUTS takes a target output from a testing or training 
;; file, and forces it to be at least 0.1 and at most 0.9. 

(defun bound-outputs (x)
  (cond ((> x 0.9) 0.9)
	((< x 0.1) 0.1)
	(t x)))
 
(defun init-network (verbose)
  (when verbose (format t "Building network ...~%"))
  (build-network)
  (when verbose (format t "Building test and train sets ...~%"))
  (build-test-and-train-sets))

;;;
;;; Reading the input file.
;;;

(defun read-data (infile verbose)
  (when verbose (format t "Reading input data ...~%"))
  (with-open-file (ifile infile :direction :input)
    (do ((x (read-char ifile) (read-char ifile))) ((equal x #\:) nil) nil)
    (setf STRUCTURE (read ifile))
    (do ((x (read-char ifile) (read-char ifile))) ((equal x #\:) nil) nil)
    (setf TOTAL-EPOCHS (read ifile)) 
    (do ((x (read-char ifile) (read-char ifile))) ((equal x #\:) nil) nil)
    (setf TEST-INTERVAL (read ifile))
    (do ((x (read-char ifile) (read-char ifile))) ((equal x #\:) nil) nil)
    (setf ETA (read ifile))
    (do ((x (read-char ifile) (read-char ifile))) ((equal x #\:) nil) nil)
    (setf ALPHA (read ifile))
    (do ((x (read-char ifile) (read-char ifile))) ((equal x #\:) nil) nil)
    (setf NOISE (read ifile))
    (do ((x (read-char ifile) (read-char ifile))) ((equal x #\:) nil) nil)
    (setf TRAIN-FILE (delete #\Tab (delete #\Space (read-line ifile))))
    (do ((x (read-char ifile) (read-char ifile))) ((equal x #\:) nil) nil)
    (setf TEST-FILE (delete #\Tab (delete #\Space (read-line ifile))))))

;;;
;;; Activation function.
;;;

(defvar zero-float (coerce 0.0 'float))

(defun activation-function (sum) 
  (/ 1.0 (+ 1.0 (exp (- zero-float sum)))))

;;;
;;; Multiplication.
;;;

(defun mult (&rest args) 
;  (without-floating-underflow-traps (apply #'* args)))
  (apply #'* args))

;;;
;;; Feed Forward phase.
;;;

(defun feed-forward (index set verbose)
  (when verbose (format t "Feeding vector ~d from ~d into network ...~%" 
                           index (if (eq set TRAINING-INPUTS) 0 1)))
  (do ((u 1 (1+ u))) ((> u (net-size NETWORK)) nil)
    (let* ((noise-added (if (= NOISE 1) (random-noise) 0.0))
	   (this-input (aref set index u))
	   (input-plus-noise (if (> this-input 0.5) 
				 (- this-input noise-added)
				 (+ this-input noise-added))))
      (setf (unit-activation (aref (net-units NETWORK) u)) input-plus-noise)))
  (do ((layer NETWORK (net-next-layer layer)))
      ((output-layer? layer) nil)
    (do ((u1 1 (1+ u1))) ((> u1 (net-size (net-next-layer layer))) nil)
      (let ((this-unit (aref (net-units (net-next-layer layer)) u1)))
	(setf (unit-weighted-sum this-unit) 0.0)
	(do ((u2 0 (1+ u2))) ((> u2 (net-size layer)) nil)
	  (setf (unit-weighted-sum this-unit)
		(+ (unit-weighted-sum this-unit)
		   (mult (unit-activation (aref (net-units layer) u2))
			 (connection-weight 
			   (aref (net-connections layer) u2 u1))))))
	(setf (unit-activation this-unit)
	      (activation-function (unit-weighted-sum this-unit))))))
  (when verbose (format t "Result = ~14,7f ...~%" 
		  (unit-activation (aref (net-units OUTPUT-LAYER) 1)))))

;;;
;;; Back Propagate phase.
;;;

(defun back-propagate (index temp-alpha verbose)
  (when verbose (format t "Backpropagating errors ...~%"))
  (do ((u 1 (1+ u))) ((> u (net-size OUTPUT-LAYER)) nil)
    (let ((this-unit (aref (net-units OUTPUT-LAYER) u)))
      (setf (unit-delta this-unit)
	    (mult (- (aref TRAINING-OUTPUTS index u) 
		     (unit-activation this-unit))
		  (unit-activation this-unit)
		  (- 1.0 (unit-activation this-unit))))))
  (do ((layer (net-prev-layer OUTPUT-LAYER) (net-prev-layer layer)))
      ((input-layer? layer) nil)
    (do ((u 0 (1+ u))) ((> u (net-size layer)))
      (let ((this-unit (aref (net-units layer) u)) (sum 0.0))
	(do ((u2 1 (1+ u2))) ((> u2 (net-size (net-next-layer layer))))
	  (setf sum 
		(+ sum (mult (unit-delta 
                               (aref (net-units (net-next-layer layer)) u2))
			     (connection-weight 
                               (aref (net-connections layer) u u2))))))
	(setf (unit-delta this-unit)
	      (mult (- 1.0 (unit-activation this-unit)) 
                    (unit-activation this-unit) sum)))))
  (do ((layer (net-prev-layer OUTPUT-LAYER) (net-prev-layer layer))
       (l 1 (1+ l)))
      ((null layer) nil)
    (do ((u 0 (1+ u))) ((> u (net-size layer)) nil)
      (let ((low-unit (aref (net-units layer) u)))
	(do ((u2 1 (1+ u2))) ((> u2 (net-size (net-next-layer layer))) nil)
	  (let* ((hi-unit (aref (net-units (net-next-layer layer)) u2))
		 (the-connection (aref (net-connections layer) u u2))
		 (newchange
		   (+ (mult ETA (unit-delta hi-unit) 
                                (unit-activation low-unit))
		      (mult temp-alpha 
                            (connection-delta-weight the-connection)))))
            (when verbose 
		  (format t "Changing weight (~d ~d ~d) from ~14,7f to ~14,7f~%"
                       l u u2 (connection-weight the-connection) 
		       (+ (connection-weight the-connection) newchange)))
	    (setf (connection-weight the-connection) 
		  (+ (connection-weight the-connection) newchange))
	    (setf (connection-delta-weight the-connection) newchange)))))))

;;;
;;; Learn.
;;;

(defun learn (infile verbose)
  (dotimes (epoch TOTAL-EPOCHS)
    (when verbose (format t "Starting epoch ~d ...~%" epoch))
    (let ((temp-alpha (if (< epoch 10) 0.0 ALPHA)))
      (when (= 0 (mod epoch TEST-INTERVAL))
	(evaluate-progress epoch infile) )
      (do ((x 1 (1+ x))) ((> x TOTAL-TRAINING))
	(feed-forward x TRAINING-INPUTS verbose)
	(back-propagate x temp-alpha verbose)))))

;;;
;;; Evaluate Progress.
;;;

(defun evaluate-progress (epoch infile)
  (evaluate-training-data epoch infile)
  (evaluate-testing-data epoch infile))

(defun evaluate-training-data (epoch infile)
  (do ((i 0 (1+ i))) ((= i 8)) (setf (aref TOTAL-GUESSED i) 0.0))
  (do ((i 0 (1+ i))) ((= i 8)) (setf (aref TOTAL-RIGHT i) 0.0))
  (do ((i 1 (1+ i))) ((> i TOTAL-TRAINING))
    (let ((right (aref TRAINING-OUTPUTS i 1)))
      (feed-forward i TRAINING-INPUTS nil)
      (let ((output-activation 
              (unit-activation (aref (net-units OUTPUT-LAYER) 1))))
	(do ((j 0 (1+ j))) ((= j 8))
	  (when (or (> output-activation (+ 0.5 (* 0.05 j)))
		    (< output-activation (- 0.5 (* 0.05 j))))
	    (setf (aref TOTAL-GUESSED j) (+ 1.0 (aref TOTAL-GUESSED j)))
	    (when (or (and (> right 0.5)
			   (> output-activation (+ 0.5 (* 0.05 j))))
		      (and (< right 0.5)
			   (< output-activation (- 0.5 (* 0.05 j)))))
	      (setf (aref TOTAL-RIGHT j) (+ 1.0 (aref TOTAL-RIGHT j)))))))))
  (with-open-file (ifile infile :direction :output :if-exists :append)
    (format ifile "EPOCH ~d.  Performance on training data:~%~%" epoch)
    (format ifile "Confidence: ")
    (do ((i 0 (1+ i))) ((= i 8)) (format ifile "~6,2f " (* i 0.05)))
    (format ifile "~%")
    (format ifile "Guessed:    ")
    (do ((i 0 (1+ i))) ((= i 8)) (format ifile "~6d " 
      (round (aref TOTAL-GUESSED i))))
    (format ifile "~%")
    (format ifile "Correct:    ")
    (do ((i 0 (1+ i))) ((= i 8)) (format ifile "~6d " 
      (round (aref TOTAL-RIGHT i))))
    (format ifile "~%")
    (format ifile "Percent:    ")
    (do ((i 0 (1+ i))) ((= i 8))
      (format ifile "~6,2f " 
	      (if (> (aref TOTAL-GUESSED i) 0.5) 
		  (/ (* 100.0 (aref TOTAL-RIGHT i)) (aref TOTAL-GUESSED i))
		  0.0)))
    (format ifile "~%~%")))

(defun evaluate-testing-data (epoch infile)
  (do ((i 0 (1+ i))) ((= i 8)) (setf (aref TOTAL-GUESSED i) 0.0))
  (do ((i 0 (1+ i))) ((= i 8)) (setf (aref TOTAL-RIGHT i) 0.0))
  (do ((i 1 (1+ i))) ((> i TOTAL-TESTING))
    (let ((right (aref TESTING-OUTPUTS i 1)))
      (feed-forward i TESTING-INPUTS nil)
      (let ((output-activation 
              (unit-activation (aref (net-units OUTPUT-LAYER) 1))))
	(do ((j 0 (1+ j))) ((= j 8))
	  (when (or (> output-activation (+ 0.5 (* 0.05 j)))
		    (< output-activation (- 0.5 (* 0.05 j))))
	    (setf (aref TOTAL-GUESSED j) (+ 1.0 (aref TOTAL-GUESSED j)))
	    (when (or (and (> right 0.5)
			   (> output-activation (+ 0.5 (* 0.05 j))))
		      (and (< right 0.5)
			   (< output-activation (- 0.5 (* 0.05 j)))))
	      (setf (aref TOTAL-RIGHT j) (+ 1.0 (aref TOTAL-RIGHT j)))))))))
  (with-open-file (ifile infile :direction :output :if-exists :append)
    (format ifile "EPOCH ~d.  Performance on testing data:~%~%" epoch)
    (format ifile "Confidence: ")
    (do ((i 0 (1+ i))) ((= i 8)) (format ifile "~6,2f " (* i 0.05)))
    (format ifile "~%")
    (format ifile "Guessed:    ")
    (do ((i 0 (1+ i))) ((= i 8)) (format ifile "~6d " 
      (round (aref TOTAL-GUESSED i))))
    (format ifile "~%")
    (format ifile "Correct:    ")
    (do ((i 0 (1+ i))) ((= i 8)) (format ifile "~6d " 
      (round (aref TOTAL-RIGHT i))))
    (format ifile "~%")
    (format ifile "Percent:    ")
    (do ((i 0 (1+ i))) ((= i 8)) 
      (format ifile "~6,2f " 
	      (if (> (aref TOTAL-GUESSED i) 0.5) 
		  (/ (* 100.0 (aref TOTAL-RIGHT i)) (aref TOTAL-GUESSED i))
		  0.0)))
    (format ifile "~%~%")))

;;;
;;; Print Weights.
;;;

(defun print-weights (infile)
  (with-open-file (ifile infile :direction :output :if-exists :append)
    (format ifile "~%~%Weights:~%~%")
    (do ((layer NETWORK (net-next-layer layer)) (k 1 (1+ k)))
	((output-layer? layer) nil)
      (do ((u 0 (1+ u))) ((> u (net-size layer)) nil)
	  (do ((u2 1 (1+ u2))) ((> u2 (net-size (net-next-layer layer))) nil)
	    (let ((the-connection (aref (net-connections layer) u u2)))
	      (format ifile "(~d ~d ~d ~14.7f)~%" 
		      k u u2 (connection-weight the-connection))))))))

(defun print-weights-to-screen ()
    (format t "~%~%Weights:~%~%")
    (do ((layer NETWORK (net-next-layer layer)) (k 1 (1+ k)))
	((output-layer? layer) nil)
      (do ((u 0 (1+ u))) ((> u (net-size layer)) nil)
	  (do ((u2 1 (1+ u2))) ((> u2 (net-size (net-next-layer layer))) nil)
	    (let ((the-connection (aref (net-connections layer) u u2)))
	      (format t "~d~%" 
		      (list k u u2 (connection-weight the-connection))))))))

;;;
;;; Backprop.
;;;

(defun backprop (infile &optional verbose)
  (read-data infile verbose)
  (init-network verbose)
  (learn infile verbose)
  (print-weights infile))


