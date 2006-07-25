;; File DeltaRule.Lisp
;; Copyright 1990 by Mark Wilson

;;;;;;;;;; Externally (callable) functions in this file: ;;;;;;;;;;;;;;;;;;;;;;

; (NewDeltaNetwork sizeList)
;     Args:  sizeList = list of sizes of slabs. This also defines
;            the number of slabs in the network.
;            (e.g., '(10 5 4) ==> a 3-slab network with 10
;            input neurons, 5 hidden neurons, and 4 output
;            neurons).
;
;  Returned value = a list describing the network:
;   (nLayers sizeList
;     (activation-array[l] .. activation-array[nLayers])
;     (weight-array[2] .. weight-array[nLayers])
;     (sum-of-products[2] .. sum-of-products[nLayers[nLayers])
;     (back-prop-error[2] .. back-prop-error[nLayers]))
;     (old-delta-weights[2] .. for momentum term

;(DeltaLearn networkList trainingList)
;  Args:  networkList = list returned from function NewDeltaNetwork
;        trainingList = a list of lists of trainlng exemplars.
;             For example, a list might be:
;             (((0 1) (1 0))  ; first exemplar
;              ((1 0) (0 1))) ; second exemplar
;             Note: the inner sub-lists can also be arrays.
;     nlterations = number of complete training iterations
;
;  Returned value = average error at output neurons for last
;        training cycle.

; (DeltaRecall networkList inputList)
;  Args:  networkList = list returned from function 'NewDeltaNetwork'
;     inputList = list OR array of input activation values
;
;  Returned value = list of output neuron values

; (DeltaPlot networkList) ==> plots a network. Must call '(init-plot) first.

; (WriteDeltaNetwork fileName networkList) ==> saves a network to disk


; (ReadDeltaNetwork fileName) ==> returns a network list

;;;;;;;;;;;;;; End of list of externally callable functions ;;;;;;;;;;;;;;;;

(proclaim '(special eidaList defaultEidaList *delta-default-input-noise-value*
                    *delta-rule-debug-flag* ))

;; Define default learning rates for each layer of neurons:

(setq defaultEidaList '(0.5 0.4 0.3 0.2 0.08 0.07))

;; Define the default noise to add to each input neuron:

(setq *delta-default-input-noise-value* 0.08)
(setq *delta-rule-debug-flag* nil)

;;
 ; Create a new delta network:                
 ;;

;; alpha = coefficient for new weight change
;; beta = coefficient for adding in last weight change

(defun NewDeltaNetwork (sizeList &optional (alpha 0.2) (beta 0.8))
 (let ((numLayers (length sizeList))
       (w-list nil)      ; weights
       (dw-list nil)     ; delta weights
       (old-dw-list nil) ; old delta weights for momentum terms
       (a-list nil)      ; activation values
       (s-list nil)      ; sum of products
       (d-list nil))     ; back propagated deltas

   (setq eidaList defaultEidaList)
;;        
 ; Initialize storage for activation energy for all slabs:
 ;;
   (setq a-list
         (mapcar '(lambda (size) (make-array (list size) :element-type :float))
                 sizeList))
;;
 ; Initialize storage for sum of products arrays:
 ;;
   (setq  s-list
         (mapcar '(lambda (size) (make-array (list size) :element-type :float))
                 (cdr sizeList)))
;;
 ; Initialize storage for delta arrays:
 ;;
   (setq d-list
         (mapcar '(lambda (size) (make-array (list size) :element-type :float))
                 (cdr sizeList)))
;;
 ; Initialize storage for the weights:
 ;;
   (dotimes (i (- numLayers 1))
     (setq w-list
           (cons (list (nth i sizeList) (nth (+ i 1) sizeList)) w-list)))
   (setq w-list
        (mapcar '(lambda (size) (make-array size :element-type :float))
                (reverse w-list)))
;;
 ; Initialize the storage for delta weights;
 ;;
   (dotimes (i (- numLayers 1))
     (setq dw-list
           (cons (list (nth i sizeList) (nth (+ i 1) sizeList)) dw-list)))
   (setq dw-list
         (mapcar '(lambda (size) (make-array size :element-type :float))
                 (reverse dw-list)))
;;
 ; Initialize the storage for old delta weights:
 ;;
   (dotimes (i  (- numLayers 1))
     (setq old-dw-list
           (cons (list (nth i sizeList) (nth (+ i 1) sizeList)) old-dw-list)))
   (setq old-dw-list
         (mapcar '(lambda (size) (make-array size :element-type :float
                                                  :initial-element 0.0))
                 (reverse old-dw-list)))
;;
 ; Initialize values for all activations:
 ;;
   (mapc '(lambda (x)
            (let ((num (array-dimension x 0)))
              (dotimes (n num)
                (setf (aref x n) (frandom 0.01 0.1)))))
          a-list)
;;
 ; Initialize values for all weights:
 ;;
   (mapc '(lambda (x)
            (let ((numI (array-dimension x 0))
                  (numJ (array-dimension x 1)))
              (dotimes (j numJ)
                (dotimes (i numI)
                  (setf (aref x i j) frandom -0.5 0.5))))))
         w-list)
   (list numLayers sizeList a-list s-list w-list dw-list
         d-list old-dw-list alpha beta))

;;
 ; Utility function for training a delta rule neural network.
 ; The first argument is a network definition (as returned from
 ; NewDeltaNetwork), the second argument is a list of training
 ; data cases (see the example test functions at the end of this
 ; file for examples), the third (optional) argument is a flag
 ; for automatic plotting of the state of the network (activated
 ; by holding down the mouse button while this function is
 ; executing, and the fourth (optional) argument is formatting
 ; information passed to function DeltaPlot to specify plotting
 ; the input neuron activation values as a two dimensional array
 ; instead of the (default) format of a one dimensional array.
;;

(defun DeltaLearn (netList trainList
                   &optional (autoPlot 'no) (input-format nil))
  (let ((nLayers (car netList))
        (sizeList (cadr netList))
        (activationList (caddr netList))
        (sumOfProductsList (car (cdddr netList)))
        (weightList (cadr (cdddr netList)))
        (deltaWeightList (caddr (cdddr netList)))
        (deltaList (cadddr (cdddr netList)))
        (oldDeltaWeightList (cadddr (cdddr (cdr netList))))
        (alpha (cadddr (cdddr (cddr netList))))
        (beta (cadddr (cdddr (cdddr netList))))
        (inputs nil)
        (targetOutputs nil)
        (iDimension nil)
        (jDimension nil)
        (iActivationVector nil)
        (jActivationVector nil)
        (n nil)
        (weightArray nil)
        (sumOfProductsArray nil)
        (iDeltaVector nil)
        (jDeltaVector nil)
        (deltaWeightArray nil)
        (oldDeltaWeightArray nil)
        (sum nil)
        (iSumOfProductsArray nil)
        (error nil)
        (outputError 0)
        (delta nil)
        (eida nil)
        (inputNoise 0))
;;
 ; Zero out deltas:
 ;;
   (dotimes (n (- nLayers 1))
     (let* ((dw   (nth n deltaList))
            (len1 (array-dimension dw 0)))
       (dotimes (i len1)
         (setf (aref dw i) 0))))
;;
 ; Zero out delta weights:
 ;;
   (dotimes (n (- nLayers 1))
     (let* ((dw   (nth n deltaWeightList))
            (len1 (array-dimension dw 0))
            (len2 (array-dimension dw 1)))
       (dotimes (i len1)
         (dotimes (j len2)
           (setf (aref dw i j) 0)))))

    (setq inputNoise *delta-default-input-noise-value*)

;;
 ; Main loop on training examples:
 ;;
    (dolist (tl trainList)
      (setq inputs (car tl))
      (setq targetOutputs (cadr tl))
      (if *delta-rule-debug-flag*
        (print (list "Current targets:" targetOutputs)))

    (setq iDimension (car sizeList)) ; get the size of the input slab
    (setq iActivationVector (car activationList)) ; get array of input activations
    (dotimes (i iDimension)                       ; copy training inputs to input slab
      (setf (aref iActivationVector i)
            (+ (nth i inputs) (frandom (- inputNoise) inputNoise))))
;;
 ; Propagate activation through all of the slabs:
 ;;
    (dotimes (n-1 (- nLayers 1)) ; update layer i to layer flowing to layer j
      (setq n (+ n-1 1))
      (setq jDimension (nth n sizeList)) ; get the size of the j'th layer
      (setq jActivationVector (nth n activationList)) ; activation array for slab j
      (setq weightArray (nth n-1 weightList))
      (setq sumOfProductsArray (nth n-1 sumOfProductsList))
      (dotimes (j jDimension) ; process each neuron in slab j
        (setq sum 0.0)        ; init sum of products to zero
        (dotimes (i iDimension) ; to get activation from each neuron in previous slab
          (setq sum
                (+ sum (* (aref weightArray i j) (aref iActivationVector i)))))
        (setf (aref sumOfProductsArray j) sum) ; save sum of products
        (setf (aref jActivationVector j) (Sigmoid sum)))
      (setq iDimension jDimension)  ; reset index for next slab pair
      (setq iActivationVector jActivationVector))
;;
 ; Activation is spread through the network and sum of products calculated.
 ; Now modify the weights in the network using back error propagation. Start
 ; by calculating the error signal for each neuron in the output layer:
;;
    (setq jDimension (nth (- nLayers 1) sizeList)) ; size of last layer
    (setq jActivationVector (nth (- nLayers 1) activationList))
    (setq jDeltaVector (nth (- nLayers 2) deltaList))
    (setq sumOfProductsArray (nth (- nLayers 2) sumOfProductsList))
    (setq outputError 0)
    (dotimes (j jDimension)
      (setq delta (- (nth j targetOutputs) (aref jActivationVector j)))
      (setq outputError (+ outputError (abs delta)))
      (setf (aref jDeltaVector j)
            (+ (aref jDeltaVector j) (* delta (dSigmoid (aref sumOfProductsArray j))))))
;;
 ; Now calculate the backpropagated error signal for all hidden slabs:
 ;;
    (dotimes (nn (- nLayers 2))
      (setq n (- nLayers 3 nn))
      (setq iDimension (nth (+ n 1) sizeList))
      (setq iSumOfProductsArray (nth n sumOfProductsList))
      (setq iDeltaVector (nth n deltaList))
      (dotimes (i iDimension)
        (setf (aref iDeltaVector i) 0.0))
      (setq weightArray (nth + n 1) weightList))
      (dotimes (i iDimension)
        (setq error 0.0)
        (dotimes (i iDimension)
          (setq error (+ error (* (aref jDeltaVector j) (aref weightArray i j)))))
        (setf (aref iDeltaVector i)
              (+ (aref iDeltaVector i) (* error (dSigmoid (aref iSumOfProductsArray i))))))
    (setq jDimension iDimension)
    (setq jDeltaVector iDeltaVector))

;;
 ; Update all delta weights in the network:
 ;;
    (setq iDimension (car sizeList))
    (dotimes (n (- nLayers 1))
      (setq iActivationVector (nth n activationList))
      (setq jDimension (nth (+ n 1) sizeList))
      (Setq jDeltaVector (nth n deltaList))
      (setq deltaWeightArray (nth n deltaWeightList))
      (setq weightArray (nth n weightlist))
      (setq eida (nth n eidaList))

      (dotimes (j jDimension)
        (dotimes (i iDimension)
          (setq delta (* eida (aref jDeltaVector j) (aref iActivationVector i)))
          (setf (aref DeltaWeightArray i j)
                (+ (aref DeltaWeightArray i j) delta)))) ; remember delta weight change
     (setq iDimension jDimension))

     (if (not (equal autoPlot 'no))
         (DeltaPlot netList input-format)))

;;
 ; Update all weights in the network:
 ;;

    (setq iDimension (car sizeList))
    (dotimes (n (- nLayers 1))
      (setq iActivationVector (nth n activationList))
      (setq jDimension (nth (+ n 1) sizeList))
      (setq jDeltaVector (nth n deltaList))
      (setq deltaWeightArray (nth n deltaWeightList))
      (setq oldDeltaWeightArray (nth n oldDeltaWeightList))
      (setq weightArray (nth n weightList))
      (dotimes (j jDimension)
        (dotimes (i iDimension)
          (setf (aref weiqhtArray i j)
                (+ (aref weightArray i j)
                   (* alpha (aref deltaWeightArray i j))
                   (* beta (aref oldDeltaWeightArray i j))))
          (setf (aref oldDeltaWeightArray i j) ; save current delta weights
                (aref deltaWeightArray i j)))) ; ...for next momentum term.
      (setq iDimension jDimension))
    (/ outputError jDimension))

;;
 ; Utility for using a trained neural network in the recall mode.
 ; The first argument to this function is a network definition (as
 ; returned from NewDeltaNetwork) and the second argument is a list
 ; of input neuron activation values to drive through the network.
;;
(defun DeltaRecall (netList inputs)
  (let ((nLayers (car netList))
        (sizeList (cadr netList))
        (activationList (caddr netList))
        (weightList (cadr (cdddr netList)))
        (iDimension nil)
        (jDimension nil)
        (iActivationVector nil)
        (jActivationVector nil)
        (n nil)
        (weightArray nil)
        (returnList nil)
        (sum nil))           
    (setq iDimension (car sizeList))  ;get the size of ths input slab 
    (setq iActivationVector (car activationList)) ;get array of input activations
    (dotimes (i iDimension) ; copy training inputs to input slab
      (setf (aref iActivationVector i) (nth i inputs)))
    (dotimes (n-1 (- nLayers 1)) 
      (setq n (+ n-1 1))
      (setq jDimension (nth n sizeList)) ; get the size of the j'th layer
      (setq jActivationVector (nth n activationList)) ; activation array for slab j
      (setq weightArray (nth n-1 weightList))
      (dotimes (j jDimension) ; process sach neuron in slab j
        (setq sum 0.0) ; init sum of products to zero
        (dotimes (i iDimension) ; to get activation from each neuron in previous slab
          (setq sum (+ sum  (* (aref weightArray i j) (aref iActivationVector i)))))
        (if *delta-rule-debug-flag*
          (print (list "sum =" sum)))
        (setf (aref jActivationVector j) (Sigmoid sum)))
      (setq iDimension jDimension) ; get ready for next slab pair
      (setq iActivationVector jActivationVector))
    (dotimes (i jDimension)
      (setq returnList (append returnList (list (aref jActivationVector j)))))
    returnList))

;;
 ; Utilities to plot a network
 ;;

(defun plotActivations (title x y data dmin dmax &optional (input-format nil))
  (let ((size (array-dimension data 0)) (ypos 0) (xpos x))
    (plot-string x (- y 10) title)
    (if input-format
        (let ((x-size (car input-format)) (y-size (cadr input-format)))
          (dotimes (yy y-size)
            (dotimes (xx x-size)
              (plot-size-rect
                     (truncate (+ xpos (* 9 xx)))
                     (truncate (+ ypos (* 9 yy) 20))
                     8 8
                     (truncate (* (/ (- (aref data (+ (* yy x-size) xx)) dmin)
                                     (- dmax dmin))
                                  8))))))
      (dotimes (i size)
        (if (< size 20)
            (setq ypos y xpos (+ x (* i 9)))
          (if (< i (/ size 2))
              (setq ypos (- y 7) xpos (+ x (* i 9)))
            (setq ypos (+ y 2) xpos (+ x (* (- i (/ size 2)) 9)))))
        (plot-size-rect
                        (truncate xpos) (truncate ypos) 8 8
                        (truncate (* (/ (- (aref data i) dmin) (- dmax dmin)) 8)))
        (plot-frame-rect (truncate xpos) (truncate ypos) 8 8)))))

(defun plotWeights (title x y data dmin dmax deltaWeights)
  (let ((Xsize (array-dimension data 0))
        (Ysize (array-dimension data 1)))
    (if (< (* Xsize Ysize) 200)  ;; don't try to plot very large weight sets
        (progn
               (plot-string (+ x 20) (- y 10) title)
               (dotimes (i Xsize)
                 (dotimes (j Ysize)
                   (plot-size-rect (+ x (* i 9)) (+ y (* j 9)) 8 8
                                   (truncate (* (/ (- (aref data i j) dmin) (- dmax dmin)) 8)))
                   (plot-frame-rect (+ x (* i 9)) (+ y (* j 9)) 8 8)
                   (plot-size-rect (+ (* Xsize 8) 30 x (* i 9))
                                   (+ Y (* j 9)) 8 8
                           (truncate (* (/ (- (aref deltaWeights i j) -.05) (- .05 -.05)) 8)))
                   (plot-frame-rect (+ (* Xsize 8) 30 x (* i 9)) (+ Y (* j 9)) 8 8)))))))


(defun DeltaPlot (netList input-format &optional (plotOnlyIfMouseClick 'yes))
  (if (or (not (equal plotOnlyIfMouseClick 'yes))
          (plot-mouse-down))  ;; only plot if mouse is clicked!
      (let ((nLayers (car netList))
            (sizeList (cadr netList))
            (activationList (caddr netList))
            (sumOfProductsList (car (cdddr netList)))
            (weightList (cadr (cdddr netList)))
            (deltaWeightList (caddr (cdddr netList)))
            (minScale -0.3)
            (maxScale 0.3)
            (y-start 0))
        (show-plot)
        (plot-string-bold 20 15 "Delta Network")
        (plot-string 20 30 "Activation slabs:")
        (plotActivations "slab1" 10 60 (nth 0 ActivationList) -0.5 0.5 input-format)
        (if input-format
            (setq y-start (+ 30 (* (cadr input-format) 11)))
          (setq Y-start 60))
        (dotimes (n-1 (- nLayers 1))
          (setq n (+ n-1 1))
          (if (equal n (- nLayers 1))
              (setq minScale -0.1 maxScale 0.1)) ; scale up output display
          (plotActivations
                   (nth n '("slab1" "slab2" "slab3" "slab4" "slab5")) ; title
                   10 ; x location for subplot
                   (+ y-start (* n 35)) ; Y location for subplot
                   (nth n ActivationList) ; data to plot as gray scale
                   minScale maxScale nil))
        (if (< nLayers 4)
            (progn (setq y-start (+ y-start 100))
                   (plot-string 20 y-start "Weights and Delta Weights:")
                   (setq y-start (+ y-start 20))
                   (dotimes (n (- nLayers 1))
                     (plotWeights    
                             (nth n '("slabl -> slab2" "slab2 -> slab3" "slab3 -> slab4"))
                             10 (+ y-start (* n 70)) ; x,y position of subplot
                             (nth n WeightList)
                             -1.0 1.0
                            (nth n deltaWeightList))))))))

;;
 ; Calculate Sigmoid and derivative of Sigmoid functions:
 ;;

(defun Sigmoid (x)
  (/ 1.0 (+ 1.0 (exp (- x)))))

(defun dSigmoid (x)
  (let ((temp (Sigmoid x)))
    (* temp (- 1.0 temp))))

;;
 ; Generate floating point random numbers:
 ;;

(defun frandom (low high)
  (let ((range (- high low)))
    (+ (* (/ (random 1000) 1000.0) range) low)))

;;
 ; Save a Delta Network to a disk file:
 ;;

(defun WriteDeltaNetwork (fileName netList)
  (let ((fileStream (open fileName :direction :output))
        (nLayers (car netList))
        (sizeList (cadr netList))
        (activationList (caddr netList))
        (weightList (cadr (cdddr netList)))
        (deltaWeightList (caddr (cdddr netList)))
        (oldDeltaWeightList (car (cddddr (cdddr netLlst))))
        (alpha (cadr (cddddr (cdddr netList))))
        (beta (caddr (cddddr (cdddr netLjst)))))
;;
 ; Write out header
 ;;   

    (print nLayers fileStream)
    (print sizeList fileStream)
;;
 ; Write out activations:
 ;;
    (dotimes (n nLayers) 
      (dotimes (i (nth n sizeList))
          (print (aref (nth n activationList) i) FileStream)))
;;
 ;Write out weights:
 ;;
    (dotimes (n (- nLayers 1))
      (let ((w (nth n weightList)))
        (dotimes (i (array-dimension w 0))
          (dotimes (j (array-dimension w 1))
            (print (aref w i j) fileStream)))))
;;
 ; Write out delta weights:
 ;;
    (dotimes (n (- nLayers 1))
      (let ((w (nth n deltaWeightList)))
        (dotimes (i (array-dimension w 0))
          (dotimes (j (array-dimension w 1))
            (print (aref w i j) fileStream)))))
;;
 ; Write out old delta weights:
 ;;
    (dotimes (n (- nLayers 1))
      (let ((w (nth n oldDeltaWeightList)))
        (dotimes (i (array-dimension w 0))
          (dotimes (j (array-dimension w 1))
            (print (aref w i j) fileStream)))))
;;
 ; Write alpha, beta terms (used for momentum):
 ;;
    (print alpha fileStream)
    (print beta fileStream)

    (close fileStream)))

;;
 ; Read a delta detwork from a disk file:
 ;;

(defun ReadDeltaNetwork (fileName)
  (let ((fileStream (open fileName :direction :input))
        (numLayers nil)
        (sizeList nil)
        (a-list nil)
        (s-list nil)
        (w-list nil)
        (dw-list nil)
        (old-dw-list)
        (d-list nil)
        alpha beta)
;;
 ; Read in header:
 ;;
    (setq numLayers (read fileStream))
    (setq sizeList (read fileStream))
;;
 ; Allocate array storage:
 ;;
    (setq a-list 
          (mapcar '(lambda (size) (make-array (list size))) sizeList))
    (setq s-list
          (mapcar '(lambda (size) (make-array (list size))) (cdr sizeList)))
    (dotimes (i (- numLayers 1))
      (setq w-list (cons (list (nth i sizeList) (nth (+ i 1) sizeList)) w-list)))
    (setq w-list
         (mapcar '(lambda (size) (make-array size)) (reverse w-list)))
    (dotimes (i (- numLayers 1))
      (setq dw-list (cons (list (nth i sizeList) (nth (+ i 1) sizeList)) dw-list)))
    (dotimes (i (- numLayers 1))
      (setq old-dw-list (cons (list (nth i sizeList) (nth (+ i 1) sizeList)) old-dw-list)))
    (setq dw-list
            (mapcar '(lambda (size) (make-array size)) (reverse dw-list)))
    (setq old-dw-list
            (mapcar '(lambda (size) (make-array size)) (reverse old-dw-list)))
;;
 ; Read in activations:
 ;;
    (dotimes (n numLayers)
      (dotimes (i (nth n sizeList))
        (setf (aref (nth n a-list) i) (read fileStream))))
;;
 ; Read in weights:
 ;;
    (dotimes (n (- numLayers 1))
      (let ((w (nth n w-list)))
        (dotimes (i (array-dimension w 0))
          (dotimes (j (array-dimension w 1))
            (setf (aref w i j) (read fileStream))))))
;;
 ; Read in delta weights:
 ;;
    (dotimes (n (- numLayers 1))
      (let ((w (nth n dw-list)))
        (dotimes (i (array-dimension w 0))
          (dotimes (j (array-dimension w1))
            (setf (aref w i j) (read fileStream))))))
;;
 ; Read in old delta weights:
 ;;
    (dotimes (n (- numLayers 1))
      (let ((w (nth n old-dw-list)))
        (dotimes (i (array-dimension w 0))
          (dotimes (j (array-dimension w 1))
            (setf (aref w i j) (read fileStream))))))

    (setq alpha (read fileStream)
          beta (read fileStream))

    (close fileStream)
    (list numLayers sizeList a-list s-list w-list dw-list d-list
          old-dw-list alpha beta)))

;;
 ; Throw away test functions for two, three and four layer networks:
 ;;

(proclaim '(special temp)) ; temp will be used to hold the network data

(defun test2 (&optional (restart 'yes) &aux RMSerror)
  (if (equal restart 'yes)
      (setq temp (newdeltanetwork '(2 2)))) ; specify a two layer network
  (dotimes (ii 10000)
    (setq RMSerror
         (deltalearn temp '(((1 0) (0 1))
                            ((0 1) (1 0)))
                     'yes)) ; autoplot mode
    (if (equal (mod ii 50) 0) ;; print out every 50 cycles
        (progn
              (princ "....training cycle \#")
              (princ ii)
              (princ " RMS error = ")
              (princ RMSerror)
              (terpri)) ) ))

(defun test3 (&optional (restart 'yes) &aux RMSerror) ; three layer network
  (if (equal restart 'yes)
      (setq temp (newdeltanetwork '(5 4 5))))
  (dotimes (ii 10000)
    (setq RMSerror
          (deltalearn temp '(((1 0 0 0 0) (0 1 0 0 0))
                             ((0 1 0 0 0) (0 0 1 0 0))
                             ((0 0 1 0 0) (0 0 0 1 0))
                             ((0 0 0 1 0) (0 0 0 0 1))
                             ((0 0 0 0 1) (1 0 0 0 0)))
                       'yes)) ; autoplot mode
  (if (equal (mod ii 50) 0) ;; print out every 50 cycles
      (progn
             (princ "....training cycle \#")
             (princ ii)
             (princ " RMS error = ")
             (princ RMSerror)
             (terpri)))))

(defun test4 (&optional (restart 'yes) &aux RMSerror) ; four layer network
  (if (equal restart 'yes)
      (setq temp (newdeltanetwork '(4 5 5 4))))
  (dotimes (ii 10000)
    (setq RMSerror
          (deltalearn temp '(((1 0 0 0) (0 1 0 0))
                             ((0 1 0 0) (0 0 1 0))
                             ((0 0 1 0) (0 0 0 1))
                             ((0 0 0 1) (1 0 0 0)))
                      'yes)) ; autoplot mode
    (if (equal (mod ii 50) 0) ;; print out every 50 cycles
        (progn
               (princ "....training cycle \#")
               (princ ii)
               (princ " RMS error = ")
               (princ RMSerror)
               (terpri)) ) ))
