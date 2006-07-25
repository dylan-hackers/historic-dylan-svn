// ----------------------------------------------------------------------------
// 			  N-PUZZLE DOMAIN
// 			  "n-puzzle.lisp"
// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
// 			  N-PUZZLE DOMAIN
// 			  "n-puzzle.lisp"
// ----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// 
// This file contains code for the n-puzzle search problem.
// The important functions are (s, s1 and s2 are states):
// 
//  (goal-state? s)		t if s is a goal state
//  (eq-states s1 s2)		are s1 and s2 equal?
//  (expand s)			successor states of s
//  (hash-state s)			some has value for s
//  (print-state s)		print function
//  (destroy-state s)		dispose of state structure
//  (heuristic s)   		heuristic distance between s and the goal
//  (generate-problem)		randomly generated start-state
//  (convert-list-to-state s)	function for creating states
//  (cost-of-move s1 s2)		always returns 1
// 
// The important variables are:
// 
//  *sample-initial-state*
//  *goal-state*
// 
// These functions and variables can all be called from an outside search program.
// In fact, these are the functions called by our implementations of depth-first,
// breadth-first, hill-climbing, A*, DFID, IDA*, and RTA* search.
// 
// ----------------------------------------------------------------------------
//  Variable *INFINITY* will be used as the largest possible number. 
//  MOST-POSITIVE-FIXNUM is a Lisp symbol that provides it.
define variable *infinity* = $most-positive-fixnum;

//  -------------------------------------------------------------------------
//  Variable *GOAL-STATE* is a standard goal configuration: tiles ordered,
//  blank in the upper-left hand corner.
define variable *goal-state* = #f;

//  Variable *PUZZLE-SIZE* gives the size of the puzzle along one dimension.
//  The 8-puzzle has size 3, the 15-puzzle has size 4, and the 24-puzzle 
//  has size 5.
define variable *puzzle-size* = #f;

*puzzle-size* := 3;

//  Variable *PUZZLE-TILES* is *PUZZLE-SIZE* squared.
define variable *puzzle-tiles* = #f;

*puzzle-tiles* := *puzzle-size* * *puzzle-size*;

//  Variable *BLANK-TILE* represents the empty position in the puzzle.
define variable *blank-tile* = #f;

*blank-tile* := 0;

//  Variable *FREE-STATES* is used for memory management.  It is a list of
//  no longer needed states.
define variable *free-states* = #f;

//  -------------------------------------------------------------------------
//  Structure PUZZLE-STATE stores a particular state of the puzzle.  It 
//  stores two distinct but equivalent representations.  The first (board)
//  is a two-dimensional array, each element of which is a tile represented by 
//  its number (0 being blank).  The second representation consists of two 
//  arrays (xcoord/ycoord).  Each array is indexed by the tile number and 
//  gives the coordinate of that tile.  Thus, it is easy to find out what tile
//  is located at position (x,y) and it is also easy to find out the position
//  of a particular tile.  Position (0,0) is the left lower side of the board.
define class <puzzle-state> (<object>)
  slot puzzle-state-board, init-keyword: #"puzzle-state-board";
  slot puzzle-state-xcoords, init-keyword: #"puzzle-state-xcoords";
  slot puzzle-state-ycoords, init-keyword: #"puzzle-state-ycoords";
end class <puzzle-state>;

//  -------------------------------------------------------------------------
//  Function MAKE-STATE creates an empty state.
define method make-state ()
  make-puzzle-state(board: make(<array>,
                                dimensions: list(*puzzle-size*,
                                                 *puzzle-size*)),
                    xcoords: make(<array>, dimensions: *puzzle-tiles*),
                    ycoords: make(<array>, dimensions: *puzzle-tiles*));
end method make-state;

//  Function CREATE-STATE returns a new state, either by retrieving one from
//  *FREE-STATES*, or by calling MAKE-STATE.
// 
//  Function DESTROY-STATE adds a state to *FREE-STATES*, so that it is 
//  available whenever we need a state structure.
define method create-state ()
  if (empty?(*free-states*))
    make-state();
  else
    begin
      let s = head(*free-states*);
      *free-states* := tail(*free-states*);
      s;
    end;
  end if;
end method create-state;

define method destroy-state (s)
  *free-states* := pair(s, *free-states*);
end method destroy-state;

//  -------------------------------------------------------------------------
//  Function TILE-NUMBER takes a state and a pair of (x,y) coordinates, and 
//  returns the tile number of the tile located there (0 if blank).
// 
//  Functions XCOORD, YCOORD take a state and a tile number, and return
//  coordinate locations of the tile.
// 
//  Functions SET-TILE-NUMBER, SET-XCOORD, and SET-YCOORD modify a state.
define method tile-in-position (s, x, y)
  s.puzzle-state-board[x,
                       y];
end method tile-in-position;

define method xcoord (s, tile) s.puzzle-state-xcoords[tile]; end method xcoord;

define method ycoord (s, tile) s.puzzle-state-ycoords[tile]; end method ycoord;

define method set-tile-in-position (s, x, y, tile)
  s.puzzle-state-board[x, y] := tile;
end method set-tile-in-position;

define method set-xcoord (s, tile, x)
  s.puzzle-state-xcoords[tile] := x;
end method set-xcoord;

define method set-ycoord (s, tile, x)
  s.puzzle-state-ycoords[tile] := x;
end method set-ycoord;

//  -------------------------------------------------------------------------
//  Function EQ-STATES returns t if the two states s1 and s2 have the same 
//  tiles in the same positions. It returns nil otherwise.
define method eq-states (s1, s2)
  for (n = 1 then 1+(n), fail = nil then nil, until fail | n = *puzzle-tiles*)
    if (~ (xcoord(s1, n) = xcoord(s2, n)) | ~ (ycoord(s1, n) = ycoord(s2, n)))
      fail := #t;
    end if;
  finally
    ~ fail;
  end for;
end method eq-states;

//  -------------------------------------------------------------------------
//  Function HEURISTIC returns the Manhattan distance between states s and the
//  goal.  The Manhattan distance is calculated by adding up, for each non-blank
//  tile, the number of horizontal and vertical moves required to move it from
//  its position in s1 to its position in s2.
define method heuristic (s) manhattan(s, *goal-state*); end method heuristic;

define method manhattan (s1, s2)
  for (n = 1 then 1+(n), total = 0 then 0, until n = *puzzle-tiles*)
    total
     := total + abs((xcoord(s1, n) - xcoord(s2, n)))
         + abs((ycoord(s1, n) - ycoord(s2, n)));
  finally
    total;
  end for;
end method manhattan;

//  -------------------------------------------------------------------------
//  Function EXPAND returns a list of all legal sucessor states of s.  It
//  looks for the position of the blank tile and moves adjacent tiles.  
//  An optional parameter t may be supplied  -- in that case, the successors 
//  are returned in a scrambled order.
define method expand (s, #key randomize)
  let blankx = xcoord(s, *blank-tile*);
  let blanky = ycoord(s, *blank-tile*);
  let boards = #f;
  if (blanky > 0)
    boards := pair(move-tile(s, blankx, blanky, blankx, blanky - 1), boards);
  end if;
  if (blankx > 0)
    boards := pair(move-tile(s, blankx, blanky, blankx - 1, blanky), boards);
  end if;
  if (blanky < *puzzle-size* - 1)
    boards := pair(move-tile(s, blankx, blanky, blankx, blanky + 1), boards);
  end if;
  if (blankx < *puzzle-size* - 1)
    boards := pair(move-tile(s, blankx, blanky, blankx + 1, blanky), boards);
  end if;
  if (randomize) random-permute(boards); else boards; end if;
end method expand;

//  Function RANDOM-PERMUTE mixes up the elements of a list.
define method random-permute (x)
  for (y = x then x, res = nil then nil, until empty?(y))
    let next = y[random-uniform(to: size(y))];
    res := pair(next, res);
    y := remove!(y, next);
  finally
    res;
  end for;
end method random-permute;

//  -------------------------------------------------------------------------
//  Function COST-OF-MOVE takes a state and its successor and returns the
//  cost of making that transition.  In the case of the n-puzzle, the 
//  cost of sliding a tile is always 1.
define method cost-of-move (state, successor) 1; end method cost-of-move;

//  -------------------------------------------------------------------------
//  Function MOVE-TILE takes a state (s), a set of coordinates indicating 
//  the location of the blank (bx/by), and a set of coordinates indicating 
//  the location of the tile to be moved (tx/ty). It copies the state and
//  moves the tile, returning a new state.
define method move-tile (s, bx, by, tx, ty)
  let c = copy-state(s);
  swap-tiles(c, bx, by, tx, ty);
end method move-tile;

define method swap-tiles (s, bx, by, tx, ty)
  let tile = tile-in-position(s, tx, ty);
  set-tile-in-position(s, tx, ty, *blank-tile*);
  set-tile-in-position(s, bx, by, tile);
  set-xcoord(s, *blank-tile*, tx);
  set-ycoord(s, *blank-tile*, ty);
  set-xcoord(s, tile, bx);
  set-ycoord(s, tile, by);
  s;
end method swap-tiles;

//  -------------------------------------------------------------------------
//  Function COPY-STATE creates and returns a new state structure which is 
//  a copy of its input.
define method copy-state (s)
  let s-new = create-state();
  for (x from 0 below *puzzle-size*)
    for (y from 0 below *puzzle-size*)
      set-tile-in-position(s-new, x, y, tile-in-position(s, x, y));
    end for;
  end for;
  for (x from 0 below *puzzle-tiles*)
    set-xcoord(s-new, x, xcoord(s, x));
    set-ycoord(s-new, x, ycoord(s, x));
  end for;
  s-new;
end method copy-state;

//  -------------------------------------------------------------------------
//  Function PRINT-STATE prints a state in a linear sequence of characters,
//  e.g., #< 0 1 2 3 4 5 6 7 8 >.
// 
//  Function PRINT-STATE-ALTERNATE prints a state in two-dimensional format.
//  If you like the second format, rename this function PRINT-STATE.
define method print-state (s, #rest ignore)
  format(*standard-output*, "#<");
  for (y from 0 below *puzzle-size*)
    for (x from 0 below *puzzle-size*)
      (formatter-1("~3d"))(*standard-output*, tile-in-position(s, x, y));
    end for;
  end for;
  format(*standard-output*, ">");
end method print-state;

define method print-state-alternate (s, #rest ignore)
  for (y from 0 below *puzzle-size*)
    format(*standard-output*, "\n");
    for (x from 0 below *puzzle-size*)
      (formatter-1("~2d"))(*standard-output*, tile-in-position(s, x, y));
    end for;
  end for;
  format(*standard-output*, "\n");
end method print-state-alternate;

//  -------------------------------------------------------------------------
//  Function CONVERT-LIST-TO-STATE takes a list like '(0 1 2 3 4 5 6 7 8)
//  and creates a state structure out of it.
define method convert-list-to-state (s)
  let s-new = create-state();
  for (x from 0 below *puzzle-size*)
    for (y from 0 below *puzzle-size*)
      let n = y * *puzzle-size* + x;
      set-tile-in-position(s-new, x, y, s[n]);
      set-xcoord(s-new, s[n], x);
      set-ycoord(s-new, s[n], y);
    end for;
  end for;
  s-new;
end method convert-list-to-state;

//  -------------------------------------------------------------------------
//  Function HASH-STATE takes a state and returns some integer. The same
//  state always yields the same integer.
define variable *primes* =
  #(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67,
    71, 73, 79, 83, 87, 89, 91, 93, 97);

define method hash-state (s)
  let hash-value = 0;
  for (x from 0 below *puzzle-size*)
    for (y from 0 below *puzzle-size*)
      hash-value
       := hash-value
           + tile-in-position(s, x, y) * *primes*[(x * *puzzle-size* + y)];
    end for;
  end for;
  hash-value;
end method hash-state;

//  -------------------------------------------------------------------------
//  Function GENERATE-PROBLEM returns a list of two elements: first is a 
//  randomly generated start state, second is the goal state. The start
//  state is generated by simulating 1000 moves from the goal state.
define method generate-problem ()
  generate-random-state(1000);
end method generate-problem;

define method generate-random-state (n)
  let s = copy-state(*goal-state*);
  for (i from 0 below n)
    let succs = expand(s);
    let next = succs[random-uniform(to: size(succs))];
    destroy-state(s);
    s := next;
    for (z in succs) if (~ (s == z)) destroy-state(z); end if; end for;
  end for;
  s;
end method generate-random-state;

//  -------------------------------------------------------------------------
//  Variable *GOAL-STATE* is a standard goal configuration: tiles ordered,
//  blank in the upper-left hand corner.
define method numbers-up-to (n)
  if (n = 0) list(0); else pair(n, numbers-up-to(n - 1)); end if;
end method numbers-up-to;

define variable *goal-state* = #f;

*goal-state*
 := convert-list-to-state(reverse(numbers-up-to(*puzzle-tiles* - 1)));

//  Variable *SAMPLE-INITIAL-STATE* is a state very close to the goal state.
define variable *sample-initial-state* = #f;

*sample-initial-state* := generate-random-state(12);

//  -------------------------------------------------------------------------
//  Function GOAL-STATE? returns t if its argument meets the specification for 
//  the goal state.
define method goal-state? (s)
  eq-states(s, *goal-state*);
end method goal-state?;

