
#|-----------------------------------------------------------------------------
Artificial Intelligence, Second Edition
Elaine Rich and Kevin Knight
McGraw Hill, 1991

This code may be freely copied and used for educational or research purposes.
All software written by Kevin Knight.
Comments, bugs, improvements to knight@cs.cmu.edu
----------------------------------------------------------------------------|#

#|----------------------------------------------------------------------------
			  N-PUZZLE DOMAIN
			  "n-puzzle.lisp"
----------------------------------------------------------------------------|#

#|-----------------------------------------------------------------------------

This file contains code for the n-puzzle search problem.
The important functions are (s, s1 and s2 are states):

 (goal-state? s)		t if s is a goal state
 (eq-states s1 s2)		are s1 and s2 equal?
 (expand s)			successor states of s
 (hash-state s)			some has value for s
 (print-state s)		print function
 (destroy-state s)		dispose of state structure
 (heuristic s)   		heuristic distance between s and the goal
 (generate-problem)		randomly generated start-state
 (convert-list-to-state s)	function for creating states
 (cost-of-move s1 s2)		always returns 1

The important variables are:

 *sample-initial-state*
 *goal-state*

These functions and variables can all be called from an outside search program.
In fact, these are the functions called by our implementations of depth-first,
breadth-first, hill-climbing, A*, DFID, IDA*, and RTA* search.

----------------------------------------------------------------------------|#

;; Variable *INFINITY* will be used as the largest possible number. 
;; MOST-POSITIVE-FIXNUM is a Lisp symbol that provides it.

(defvar *infinity* most-positive-fixnum)

;; -------------------------------------------------------------------------
;; Variable *GOAL-STATE* is a standard goal configuration: tiles ordered,
;; blank in the upper-left hand corner.

(defvar *goal-state* nil)

;; Variable *PUZZLE-SIZE* gives the size of the puzzle along one dimension.
;; The 8-puzzle has size 3, the 15-puzzle has size 4, and the 24-puzzle 
;; has size 5.

(defvar *puzzle-size* nil)
(setq *puzzle-size* 3)

;; Variable *PUZZLE-TILES* is *PUZZLE-SIZE* squared.

(defvar *puzzle-tiles* nil)
(setq *puzzle-tiles* (* *puzzle-size* *puzzle-size*))

;; Variable *BLANK-TILE* represents the empty position in the puzzle.

(defvar *blank-tile* nil)
(setq *blank-tile* 0)

;; Variable *FREE-STATES* is used for memory management.  It is a list of
;; no longer needed states.

(defvar *free-states* nil)

;; -------------------------------------------------------------------------
;; Structure PUZZLE-STATE stores a particular state of the puzzle.  It 
;; stores two distinct but equivalent representations.  The first (board)
;; is a two-dimensional array, each element of which is a tile represented by 
;; its number (0 being blank).  The second representation consists of two 
;; arrays (xcoord/ycoord).  Each array is indexed by the tile number and 
;; gives the coordinate of that tile.  Thus, it is easy to find out what tile
;; is located at position (x,y) and it is also easy to find out the position
;; of a particular tile.  Position (0,0) is the left lower side of the board.

(defstruct (puzzle-state (:print-function print-state))
      board
      xcoords
      ycoords)


;; -------------------------------------------------------------------------
;; Function MAKE-STATE creates an empty state.

(defun make-state ()
  (make-puzzle-state
	:board (make-array (list *puzzle-size* *puzzle-size*)
			   :element-type 'integer)
	:xcoords (make-array *puzzle-tiles*
			     :element-type 'integer)
	:ycoords (make-array *puzzle-tiles*
			     :element-type 'integer)))


;; Function CREATE-STATE returns a new state, either by retrieving one from
;; *FREE-STATES*, or by calling MAKE-STATE.
;;
;; Function DESTROY-STATE adds a state to *FREE-STATES*, so that it is 
;; available whenever we need a state structure.

(defun create-state ()
  (cond ((null *free-states*) (make-state))
        (t (let ((s (car *free-states*)))
	      (setq *free-states* (cdr *free-states*))
	      s))))

(defun destroy-state (s)
   (setq *free-states* (cons s *free-states*)))

;; -------------------------------------------------------------------------
;; Function TILE-NUMBER takes a state and a pair of (x,y) coordinates, and 
;; returns the tile number of the tile located there (0 if blank).
;;
;; Functions XCOORD, YCOORD take a state and a tile number, and return
;; coordinate locations of the tile.
;;
;; Functions SET-TILE-NUMBER, SET-XCOORD, and SET-YCOORD modify a state.

(defun tile-in-position (s x y)
  (aref (puzzle-state-board s) x y))

(defun xcoord (s tile)
  (aref (puzzle-state-xcoords s) tile))

(defun ycoord (s tile)
  (aref (puzzle-state-ycoords s) tile))

(defun set-tile-in-position (s x y tile)
  (setf (aref (puzzle-state-board s) x y) tile))

(defun set-xcoord (s tile x)
  (setf (aref (puzzle-state-xcoords s) tile) x))

(defun set-ycoord (s tile x)
  (setf (aref (puzzle-state-ycoords s) tile) x))


;; -------------------------------------------------------------------------
;; Function EQ-STATES returns t if the two states s1 and s2 have the same 
;; tiles in the same positions. It returns nil otherwise.

(defun eq-states (s1 s2)
  (do ((n 1 (1+ n)) 
       (fail nil))
      ((or fail (= n *puzzle-tiles*))
       (not fail))
    (when (or (not (= (xcoord s1 n) (xcoord s2 n)))
	      (not (= (ycoord s1 n) (ycoord s2 n))))
       (setq fail t))))

;; -------------------------------------------------------------------------
;; Function HEURISTIC returns the Manhattan distance between states s and the
;; goal.  The Manhattan distance is calculated by adding up, for each non-blank
;; tile, the number of horizontal and vertical moves required to move it from
;; its position in s1 to its position in s2.

(defun heuristic (s)
  (manhattan s *goal-state*))

(defun manhattan (s1 s2)
  (do ((n 1 (1+ n))
       (total 0))
      ((= n *puzzle-tiles*) total)
    (setq total (+ total (abs (- (xcoord s1 n) (xcoord s2 n)))
			 (abs (- (ycoord s1 n) (ycoord s2 n)))))))

; -------------------------------------------------------------------------
;; Function EXPAND returns a list of all legal sucessor states of s.  It
;; looks for the position of the blank tile and moves adjacent tiles.  
;; An optional parameter t may be supplied  -- in that case, the successors 
;; are returned in a scrambled order.

(defun expand (s &optional randomize)
  (let ((blankx (xcoord s *blank-tile*))
 	(blanky (ycoord s *blank-tile*))
	(boards nil))
     (when (> blanky 0)
	(setq boards (cons (move-tile s blankx blanky blankx (1- blanky))
			   boards)))
     (when (> blankx 0)
	(setq boards (cons (move-tile s blankx blanky (1- blankx) blanky)
			   boards)))
     (when (< blanky (1- *puzzle-size*))
	(setq boards (cons (move-tile s blankx blanky blankx (1+ blanky))
			   boards)))
     (when (< blankx (1- *puzzle-size*))
	(setq boards (cons (move-tile s blankx blanky (1+ blankx) blanky)
			   boards)))
     (if randomize (random-permute boards)
	 boards)))


;; Function RANDOM-PERMUTE mixes up the elements of a list.

(defun random-permute (x)
  (do ((y x)
       (res nil))
      ((null y) res)
    (let ((next (nth (random (length y)) y)))
       (setq res (cons next res))
       (setq y (delete next y)))))


;; -------------------------------------------------------------------------
;; Function COST-OF-MOVE takes a state and its successor and returns the
;; cost of making that transition.  In the case of the n-puzzle, the 
;; cost of sliding a tile is always 1.

(defun cost-of-move (state successor)
  (declare (ignore state successor))
  1)


;; -------------------------------------------------------------------------
;; Function MOVE-TILE takes a state (s), a set of coordinates indicating 
;; the location of the blank (bx/by), and a set of coordinates indicating 
;; the location of the tile to be moved (tx/ty). It copies the state and
;; moves the tile, returning a new state.

(defun move-tile (s bx by tx ty)
  (let ((c (copy-state s)))
     (swap-tiles c bx by tx ty)))

(defun swap-tiles (s bx by tx ty)
  (let ((tile (tile-in-position s tx ty)))
     (set-tile-in-position s tx ty *blank-tile*)
     (set-tile-in-position s bx by tile)
     (set-xcoord s *blank-tile* tx)
     (set-ycoord s *blank-tile* ty)
     (set-xcoord s tile bx)
     (set-ycoord s tile by)
     s))

; -------------------------------------------------------------------------
;; Function COPY-STATE creates and returns a new state structure which is 
;; a copy of its input.

(defun copy-state (s)
  (let ((s-new (create-state)))
    (dotimes (x *puzzle-size*)
	(dotimes (y *puzzle-size*)
	    (set-tile-in-position s-new x y (tile-in-position s x y))))
    (dotimes (x *puzzle-tiles*)
        (set-xcoord s-new x (xcoord s x))
	(set-ycoord s-new x (ycoord s x)))
    s-new))

; -------------------------------------------------------------------------
;; Function PRINT-STATE prints a state in a linear sequence of characters,
;; e.g., #< 0 1 2 3 4 5 6 7 8 >.
;;
;; Function PRINT-STATE-ALTERNATE prints a state in two-dimensional format.
;; If you like the second format, rename this function PRINT-STATE.

(defun print-state (s &rest ignore)
   (declare (ignore ignore))
   (format *standard-output* "#<")
   (dotimes (y *puzzle-size*)
       (dotimes (x *puzzle-size*)
	   (format *standard-output* "~3d" (tile-in-position s x y))))
   (format *standard-output*  ">"))

(defun print-state-alternate (s &rest ignore)
   (declare (ignore ignore))
   (dotimes (y *puzzle-size*)
       (format *standard-output* "~%")
       (dotimes (x *puzzle-size*)
	   (format *standard-output* "~2d" (tile-in-position s x y))))
   (format *standard-output* "~%"))

; -------------------------------------------------------------------------
;; Function CONVERT-LIST-TO-STATE takes a list like '(0 1 2 3 4 5 6 7 8)
;; and creates a state structure out of it.

(defun convert-list-to-state (s)
  (let ((s-new (create-state)))
    (dotimes (x *puzzle-size*)
        (dotimes (y *puzzle-size*)
           (let ((n (+ (* y *puzzle-size*) x)))
	      (set-tile-in-position s-new x y (nth n s))
              (set-xcoord s-new (nth n s) x)
              (set-ycoord s-new (nth n s) y))))
  s-new))


; -------------------------------------------------------------------------
;; Function HASH-STATE takes a state and returns some integer. The same
;; state always yields the same integer.

(defvar *primes*
  '(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 
    79 83 87 89 91 93 97))

(defun hash-state (s)
  (let ((hash-value 0))
    (dotimes (x *puzzle-size*)
      (dotimes (y *puzzle-size*)
        (setq hash-value (+ hash-value (* (tile-in-position s x y) 
			    		  (nth (+ (* x *puzzle-size*) y)
                                 	       *primes*))))))
    hash-value))


; -------------------------------------------------------------------------
;; Function GENERATE-PROBLEM returns a list of two elements: first is a 
;; randomly generated start state, second is the goal state. The start
;; state is generated by simulating 1000 moves from the goal state.

(defun generate-problem ()
  (generate-random-state 1000))

(defun generate-random-state (n)
   (let ((s (copy-state *goal-state*)))
      (dotimes (i n)
	 (let* ((succs (expand s))
	        (next (nth (random (length succs)) succs)))
	    (destroy-state s)
	    (setq s next)
	    (dolist (z succs)
		(when (not (eq s z))
		   (destroy-state z)))))
      s))


;; -------------------------------------------------------------------------
;; Variable *GOAL-STATE* is a standard goal configuration: tiles ordered,
;; blank in the upper-left hand corner.

(defun numbers-up-to (n)
    (cond ((= n 0) (list 0))
          (t (cons n (numbers-up-to (1- n))))))

(defvar *goal-state* nil)

(setq *goal-state* 
	(convert-list-to-state (reverse (numbers-up-to (1- *puzzle-tiles*)))))

;; Variable *SAMPLE-INITIAL-STATE* is a state very close to the goal state.

(defvar *sample-initial-state*)

(setq *sample-initial-state*
	(generate-random-state 12))

;; -------------------------------------------------------------------------
;; Function GOAL-STATE? returns t if its argument meets the specification for 
;; the goal state.

(defun goal-state? (s)
  (eq-states s *goal-state*))

