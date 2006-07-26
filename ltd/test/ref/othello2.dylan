//  -*- Mode: Lisp; Syntax: Common-Lisp -*-
//  Code from Paradigms of AI Programming
//  Copyright (c) 1991 Peter Norvig
//  File othello2.lisp:  More strategies for othello.lisp,
//  from section 18.9 onward (alpha-beta2, alpha-beta3, iago).
//  If a compiled version of edge-table.lisp exists, then merely
//  load it after you load this file.  Otherwise, load this file,
//  evaluate (init-edge-table) (this will take a really long time),
//  then compile edge-table.lisp.  This will save the edge-table for
//  future use.
requires("othello");

define constant all-squares =
  sort!(begin
          let _acc = make(<deque>);
          for (i from 11 to 88)
            if (begin
                  let g112164 = modulo(i, 10);
                  1 <= g112164 & g112164 <= 8;
                end)
              push-last(_acc, i);
            end if;
          finally
            _acc;
          end for;
        end,
        test: method (x, y)
                (method (sq) *weights*[sq]; end method)(x)
                 > (method (sq) *weights*[sq]; end method)(y);
              end method);

define class <node> (<object>)
  slot node-square, init-keyword: #"node-square";
  slot node-board, init-keyword: #"node-board";
  slot node-value, init-keyword: #"node-value";
end class <node>;

define method alpha-beta-searcher2 (depth, eval-fn)
  // Return a strategy that does A-B search with sorted moves.
  method (player, board)
    let (value, node)
        = alpha-beta2(player,
                      make-node(board: board, value: eval-fn(player, board)),
                      losing-value, winning-value, depth, eval-fn);
    node.node-square;
  end method;
end method alpha-beta-searcher2;

define method alpha-beta2 (player, node, achievable, cutoff, ply, eval-fn)
  // A-B search, sorting moves by eval-fn
  //  Returns two values: achievable-value and move-to-make
  if (ply = 0)
    values(node.node-value, node);
  else
    let board = node.node-board;
    let nodes = legal-nodes(player, board, eval-fn);
    if (empty?(nodes))
      if (any-legal-move?(opponent(player), board))
        values(- alpha-beta2(opponent(player), negate-value(node), (- cutoff),
                             (- achievable), (ply - 1), eval-fn),
               #f);
      else
        values(final-value(player, board), #f);
      end if;
    else
      let best-node = first(nodes);
      for (move in nodes,
           val = - alpha-beta2(opponent(player), negate-value(move),
                               (- cutoff), (- achievable), (ply - 1),
                               eval-fn) then - alpha-beta2(opponent(player),
                                                           negate-value(move),
                                                           (- cutoff),
                                                           (- achievable),
                                                           (ply - 1),
                                                           eval-fn),
           until achievable >= cutoff)
        if (val > achievable) achievable := val; best-node := move; end if;
      end for;
      values(achievable, best-node);
    end if;
  end if;
end method alpha-beta2;

define method negate-value (node)
  // Set the value of a node to its negative.
  node.node-value := - node.node-value;
  node;
end method negate-value;

define method legal-nodes (player, board, eval-fn)
  // Return a list of legal moves, each one packed into a node.
  let moves = legal-moves(player, board);
  sort!(map-into(moves,
                 method (move)
                   let new-board = make-move(move, player, copy-board(board));
                   make-node(square: move, board: new-board,
                             value: eval-fn(player, new-board));
                 end method,
                 moves),
        test: method (x, y) x.node-value > y.node-value; end method);
end method legal-nodes;

define variable *ply-boards* =
  apply(vector,
        begin
          let _acc = make(<deque>);
          for (% from 1 to 40)
            push-last(_acc, initial-board());
          finally
            _acc;
          end for;
        end);

define method alpha-beta3 (player, board, achievable, cutoff, ply, eval-fn,
                           killer)
  // A-B search, putting killer move first.
  if (ply = 0)
    eval-fn(player, board);
  else
    let moves = put-first(killer, legal-moves(player, board));
    if (empty?(moves))
      if (any-legal-move?(opponent(player), board))
        - alpha-beta3(opponent(player), board, (- cutoff), (- achievable),
                      (ply - 1), eval-fn, nil);
      else
        final-value(player, board);
      end if;
    else
      let best-move = first(moves);
      let new-board = *ply-boards*[ply];
      let killer2 = #f;
      let killer2-val = winning-value;
      for (move in moves, until achievable >= cutoff)
        let (val, reply)
            = alpha-beta3(opponent(player),
                          make-move(move, player,
                                    replace-subsequence!(new-board, board)),
                          - cutoff, - achievable, ply - 1, eval-fn, killer2);
        val := - val;
        if (val > achievable) achievable := val; best-move := move; end if;
        if (reply & val < killer2-val)
          killer2 := reply;
          killer2-val := val;
        end if;
      end for;
      values(achievable, best-move);
    end if;
  end if;
end method alpha-beta3;

define method alpha-beta-searcher3 (depth, eval-fn)
  // Return a strategy that does A-B search with killer moves.
  method (player, board)
    let (value, move)
        = alpha-beta3(player, board, losing-value, winning-value, depth,
                      eval-fn, #f);
    move;
  end method;
end method alpha-beta-searcher3;

define method put-first (killer, moves)
  // Move the killer move to the front of moves,
  //   if the killer move is in fact a legal move.
  if (member?(killer, moves))
    pair(killer, remove!(moves, killer));
  else
    moves;
  end if;
end method put-first;

define method mobility (player, board)
  // Current Mobility is the number of legal moves.
  //   Potential mobility is the number of blank squares
  //   adjacent to an opponent that are not legal moves.
  //   Returns current and potential mobility for player.
  let opp = opponent(player);
  let current = 0;
  let potential = 0;
  //  player's potential mobility
  for (square in all-squares)
    if (bref(board, square) == empty)
      if (legal-p(square, player, board))
        inc!(current);
      elseif (any?(method (sq) bref(board, sq) == opp; end method,
                   neighbors(square)))
        inc!(potential);
      end if;
    end if;
  end for;
  values(current, current + potential);
end method mobility;

// Array of values to player-to-move for edge positions.
define variable *edge-table* = make(<array>, dimensions: 3 ^ 10);

// The four edges (with their X-squares).
define constant edge-and-x-lists =
  #(#(22, 11, 12, 13, 14, 15, 16, 17, 18, 27),
    #(72, 81, 82, 83, 84, 85, 86, 87, 88, 77),
    #(22, 11, 21, 31, 41, 51, 61, 71, 81, 72),
    #(27, 18, 28, 38, 48, 58, 68, 78, 88, 77));

define method edge-index (player, board, squares)
  // The index counts 1 for player; 2 for opponent,
  //   on each square---summed as a base 3 number.
  let index = 0;
  for (sq in squares)
    index
     := index * 3
         + if ((bref(board, sq) == empty))
             0;
           elseif ((bref(board, sq) == player))
             1;
           else
             2;
           end if;
  end for;
  index;
end method edge-index;

define method edge-stability (player, board)
  // Total edge evaluation for player to move on board.
  let _acc = 0;
  for (edge-list in edge-and-x-lists)
    inc!(_acc, *edge-table*[edge-index(player, board, edge-list)]);
  finally
    _acc;
  end for;
end method edge-stability;

define constant top-edge = first(edge-and-x-lists);

define method init-edge-table ()
  // Initialize *edge-table*, starting from the empty board.
  //  Initialize the static values
  for (n-pieces from 0 to 10)
    map-edge-n-pieces(method (board, index)
                        *edge-table*[index]
                                      := static-edge-stability(black, board);
                      end method,
                      black, initial-board(), n-pieces, top-edge, 0);
  end for;
  //  Now iterate five times trying to improve:
  for (i from 0 below 5)
    //  Do the indexes with most pieces first
    for (n-pieces from 9 to 1)
      map-edge-n-pieces(method (board, index)
                          *edge-table*[index]
                                        := possible-edge-moves-value(black,
                                                                     board,
                                                                     index);
                        end method,
                        black, initial-board(), n-pieces, top-edge, 0);
    end for;
  end for;
end method init-edge-table;

define method map-edge-n-pieces (fn, player, board, n, squares, index)
  // Call fn on all edges with n pieces.
  //  Index counts 1 for player; 2 for opponent
  if (size(squares) < n)
    #f;
  elseif (empty?(squares))
    fn(board, index);
  else
    begin
      let index3 = 3 * index;
      let sq = first(squares);
      map-edge-n-pieces(fn, player, board, n, tail(squares), index3);
      if (n > 0 & bref(board, sq) == empty)
        bref(board, sq) := player;
        map-edge-n-pieces(fn, player, board, n - 1, tail(squares),
                          1 + index3);
        bref(board, sq) := opponent(player);
        map-edge-n-pieces(fn, player, board, n - 1, tail(squares),
                          2 + index3);
        bref(board, sq) := empty;
      end if;
    end;
  end if;
end method map-edge-n-pieces;

define method possible-edge-moves-value (player, board, index)
  // Consider all possible edge moves. 
  //   Combine their values into a single number.
  combine-edge-moves(pair(list(1.0, *edge-table*[index]), //  no move
                          begin
                            let _acc = make(<deque>);
                            for (sq in top-edge)
                              if (bref(board, sq) == empty)
                                push-last(_acc,
                                          possible-edge-move(player,
                                                             board,
                                                             sq));
                              end if;
                            finally
                              _acc;
                            end for;
                          end),
                     player);
end method possible-edge-moves-value;

define method possible-edge-move (player, board, sq)
  // Return a (prob val) pair for a possible edge move.
  let new-board = replace-subsequence!(*ply-boards*[player], board);
  make-move(sq, player, new-board);
  list(edge-move-probability(player, board, sq),
       - *edge-table*[edge-index(opponent(player), new-board, top-edge)]);
end method possible-edge-move;

define method combine-edge-moves (possibilities, player)
  // Combine the best moves.
  let prob = 1.0;
  let val = 0.0;
  let fn = if (player == black) \>; else \<; end if;
  for (pair in sort!(possibilities,
                     test: method (x, y)
                             fn(second(x), second(y));
                           end method),
       while prob >= 0.0)
    inc!(val, prob * first(pair) * second(pair));
    dec!(prob, prob * first(pair));
  end for;
  round(val);
end method combine-edge-moves;

begin
  let corner/xsqs = #(#(11 . 22), #(18 . 27), #(81, 72), #(88 . 77));
  define method corner-p (sq) cl-assoc(sq, corner/xsqs); end method corner-p;
  define method x-square-p (sq)
    // LTD: Function RASSOC not yet implemented.
    rassoc(sq, corner/xsqs);
  end method x-square-p;
  define method x-square-for (corner)
    tail(cl-assoc(corner, corner/xsqs));
  end method x-square-for;
  define method corner-for (xsq)
    head(// LTD: Function RASSOC not yet implemented.
         rassoc(xsq, corner/xsqs));
  end method corner-for;
end;

define method edge-move-probability (player, board, square)
  // What's the probability that player can move to this square?
  if (x-square-p(square))
    0.5;
    //  X-squares
    elseif (legal-p(square, player, board))
    1.0;
    //  immediate capture
    elseif (corner-p(square))
    //  move to corner depends on X-square
    begin
      let x-sq = x-square-for(square);
      if (bref(board, x-sq) == empty)
        0.1;
      elseif (bref(board, x-sq) == player)
        0.001;
      else
        0.9;
      end if;
    end;
  else
    #2A((0.1 0.4 0.7) (0.05 0.3 *) (0.01 * *))[count-edge-neighbors(player,
                                                                    board,
                                                                    square),
                                               count-edge-neighbors(opponent(player),
                                                                    board,
                                                                    square)]
                                                / if (legal-p(square,
                                                              opponent(player),
                                                              board))
                                                  2;
                                                  else
                                                  1;
                                                  end if;
  end if;
end method edge-move-probability;

define method count-edge-neighbors (player, board, square)
  // Count the neighbors of this square occupied by player.
  cl-count-if(method (inc) bref(board, square + inc) == player; end method,
              #(1, -1));
end method count-edge-neighbors;

define variable *static-edge-table* =
  #2A((* 0 -2000) (700 * *) (1200 200 -25) (1000 200 75) (1000 200 50) (1000 200 50) (1000 200 75) (1200 200 -25) (700 * *) (* 0 -2000));

define method static-edge-stability (player, board)
  // Compute this edge's static stability
  let _acc = 0;
  for (sq in top-edge, i from 0)
    inc!(_acc,
         if (bref(board, sq) == empty)
           0;
         elseif (bref(board, sq) == player)
           *static-edge-table*[i,
                               piece-stability(board, sq)];
         else
           - *static-edge-table*[i,
                                 piece-stability(board, sq)];
         end if);
  finally
    _acc;
  end for;
end method static-edge-stability;

begin
  let stable = 0;
  let semi-stable = 1;
  let unstable = 2;
  define method piece-stability (board, sq)
    if (corner-p(sq))
      stable;
    elseif (x-square-p(sq))
      if (bref(board, corner-for(sq)) == empty)
        unstable;
      else
        semi-stable;
      end if;
    else
      begin
        let player = bref(board, sq);
        let opp = opponent(player);
        let p1 = cl-find(player, board, test-not: \==, start: sq, end: 19);
        let p2
            = cl-find(player, board, test-not: \==, start: 11, end: sq,
                      from-end: #t);
        if (p1 == empty & p2 == opp | (p2 == empty & p1 == opp))
          unstable;
          //  Semi-stable pieces might be captured
          elseif (p1 == opp & p2 == opp
                   & cl-find(empty, board, start: 11, end: 19))
          semi-stable;
        elseif (p1 == empty & p2 == empty)
          semi-stable;
          //  Stable pieces can never be captured
          else
          stable;
        end if;
      end;
    end if;
  end method piece-stability;
end;

define method iago-eval (player, board)
  // Combine edge-stability, current mobility and
  //   potential mobility to arrive at an evaluation.
  let c-edg = 312000 + 6240 * *move-number*;
  let c-cur
      = if (*move-number* < 25)
          50000 + 2000 * *move-number*;
        else
          75000 + 1000 * *move-number*;
        end if;
  let c-pot = 20000;
  let (p-cur, p-pot) = mobility(player, board);
  let (o-cur, o-pot) = mobility(opponent(player), board);
  //  Combine the three factors into one sum:
  round/(c-edg * edge-stability(player, board), 32000)
   + round/(c-cur * (p-cur - o-cur), (p-cur + o-cur + 2))
   + round/(c-pot * (p-pot - o-pot), (p-pot + o-pot + 2));
end method iago-eval;

define method iago (depth)
  // Use an approximation of Iago's evaluation function.
  alpha-beta-searcher3(depth, iago-eval);
end method iago;

