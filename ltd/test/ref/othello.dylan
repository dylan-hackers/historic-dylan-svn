//  -*- Mode: Lisp; Syntax: Common-Lisp -*-
//  Code from Paradigms of AI Programming
//  Copyright (c) 1991 Peter Norvig
//  File othello.lisp: An othello monitor, with all strategies
//  up to and including section 18.8
//  One bug fix by Alberto Segre, segre@cs.cornell.edu, March 1993.
define method cross-product (fn, xlist, ylist)
  // Return a list of all (fn x y) values.
  mappend(method (y) map(method (x) fn(x, y); end method, xlist); end method,
          ylist);
end method cross-product;

define constant all-directions = #(-11, -10, -9, -1, 1, 9, 10, 11);

// An empty square
define constant empty = 0;

// A black piece
define constant black = 1;

// A white piece
define constant white = 2;

// Marks squares outside the 8x8 board
define constant outer = 3;

// LTD: Can't handle complex deftypes.
#f;

define method name-of (piece) ".@O?"[piece]; end method name-of;

define method opponent (player)
  if (player == black) white; else black; end if;
end method opponent;

// LTD: Can't handle complex deftypes.
#f;

define method bref (board, square) board[square]; end method bref;

// LTD: No setf macros.
#"bref";

define method copy-board (board) copy-sequence(board); end method copy-board;

define constant all-squares =
  begin
    let _acc = make(<deque>);
    for (i from 11 to 88)
      if (begin let g110706 = modulo(i, 10); 1 <= g110706 & g110706 <= 8; end)
        push-last(_acc, i);
      end if;
    finally
      _acc;
    end for;
  end;

define method initial-board ()
  // Return a board, empty except for four pieces in the middle.
  let board = make(<vector>, size: 100);
  for (square in all-squares) bref(board, square) := empty; end for;
  begin
    bref(board, 44) := white;
    bref(board, 45) := black;
    bref(board, 54) := black;
    bref(board, 55) := white;
  end;
  board;
end method initial-board;

define method count-difference (player, board)
  // Count player's pieces minus opponent's pieces.
  cl-count(player, board) - cl-count(opponent(player), board);
end method count-difference;

define method valid-p (move)
  // Valid moves are numbers in the range 11-88 that end in 1-8.
  instance?(move, <integer>) & (11 <= move & move <= 88)
   & begin let g110803 = modulo(move, 10); (1 <= g110803 & g110803 <= 8); end;
end method valid-p;

define method legal-p (move, player, board)
  // A Legal move must be into an empty square, and it must
  //   flip at least one opponent piece.
  bref(board, move) == empty
   & any?(method (dir) would-flip?(move, player, board, dir); end method,
          all-directions);
end method legal-p;

define method make-move (move, player, board)
  // Update board to reflect move by player
  //  First make the move, then make any flips
  bref(board, move) := player;
  for (dir in all-directions) make-flips(move, player, board, dir); end for;
  board;
end method make-move;

define method make-flips (move, player, board, dir)
  // Make any flips in the given direction.
  let bracketer = would-flip?(move, player, board, dir);
  if (bracketer)
    for (c from move + dir by dir, until c == bracketer)
      bref(board, c) := player;
    end for;
  end if;
end method make-flips;

define method would-flip? (move, player, board, dir)
  // Would this move result in any flips in this direction?
  //   If so, return the square number of the bracketing piece.
  let c = move + dir;
  bref(board, c) == opponent(player)
   & find-bracketing-piece(c + dir, player, board, dir);
end method would-flip?;

define method find-bracketing-piece (square, player, board, dir)
  // Return the square number of the bracketing piece.
  if (bref(board, square) == player)
    square;
  elseif (bref(board, square) == opponent(player))
    find-bracketing-piece(square + dir, player, board, dir);
  else
    #f;
  end if;
end method find-bracketing-piece;

define method next-to-play (board, previous-player, print)
  // Compute the player to move next, or NIL if nobody can move.
  let opp = opponent(previous-player);
  if (any-legal-move?(opp, board))
    opp;
  elseif (any-legal-move?(previous-player, board))
    if (print)
      format-out("\n%c has no moves and must pass.", name-of(opp));
    end if;
    previous-player;
  else
    #f;
  end if;
end method next-to-play;

define method any-legal-move? (player, board)
  // Does player have any legal moves in this position?
  any?(method (move) legal-p(move, player, board); end method, all-squares);
end method any-legal-move?;

define method random-strategy (player, board)
  // Make any legal move.
  random-elt(legal-moves(player, board));
end method random-strategy;

define method legal-moves (player, board)
  // Returns a list of legal moves for player
  let _acc = make(<deque>);
  for (move in all-squares)
    if (legal-p(move, player, board)) push-last(_acc, move); end if;
  finally
    _acc;
  end for;
end method legal-moves;

define method maximize-difference (player, board)
  // A strategy that maximizes the difference in pieces.
  (maximizer(count-difference))(player, board);
end method maximize-difference;

define method maximizer (eval-fn)
  // Return a strategy that will consider every legal move,
  //   apply EVAL-FN to each resulting board, and choose 
  //   the move for which EVAL-FN returns the best score.
  //   FN takes two arguments: the player-to-move and board
  method (player, board)
    let moves = legal-moves(player, board);
    let scores
        = map(method (move)
                eval-fn(player, make-move(move, player, copy-board(board)));
              end method,
              moves);
    let best = apply(max, scores);
    moves[find-key(scores, curry(\==, best))];
  end method;
end method maximizer;

define variable *weights* =
  #[0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 120, -20, 20, 5, 5, 20, -20, 120, 0, 0,
    -20, -40, -5, -5, -5, -5, -40, -20, 0, 0, 20, -5, 15, 3, 3, 15, -5, 20, 0,
    0, 5, -5, 3, 3, 3, 3, -5, 5, 0, 0, 5, -5, 3, 3, 3, 3, -5, 5, 0, 0, 20, -5,
    15, 3, 3, 15, -5, 20, 0, 0, -20, -40, -5, -5, -5, -5, -40, -20, 0, 0, 120,
    -20, 20, 5, 5, 20, -20, 120, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0];

define method weighted-squares (player, board)
  // Sum of the weights of player's squares minus opponent's.
  let opp = opponent(player);
  let _acc = 0;
  for (i in all-squares)
    if (bref(board, i) == player) inc!(_acc, *weights*[i]); end if;
    if (bref(board, i) == opp) inc!(_acc, - *weights*[i]); end if;
  finally
    _acc;
  end for;
end method weighted-squares;

define constant winning-value = $most-positive-fixnum;

define constant losing-value = $most-negative-fixnum;

define method final-value (player, board)
  // Is this a win, loss, or draw for player?
  select (signum(count-difference(player, board)))
    -1
       => losing-value;
    0
       => 0;
    1
       => winning-value;
    otherwise
       => #f;
  end select;
end method final-value;

define method minimax (player, board, ply, eval-fn)
  // Find the best move, for PLAYER, according to EVAL-FN,
  //   searching PLY levels deep and backing up values.
  if (ply = 0)
    eval-fn(player, board);
  else
    let moves = legal-moves(player, board);
    if (empty?(moves))
      if (any-legal-move?(opponent(player), board))
        - minimax(opponent(player), board, (ply - 1), eval-fn);
      else
        final-value(player, board);
      end if;
    else
      let best-move = #f;
      let best-val = #f;
      for (move in moves)
        let board2 = make-move(move, player, copy-board(board));
        let val = - minimax(opponent(player), board2, (ply - 1), eval-fn);
        if (empty?(best-val) | val > best-val)
          best-val := val;
          best-move := move;
        end if;
      end for;
      values(best-val, best-move);
    end if;
  end if;
end method minimax;

define method minimax-searcher (ply, eval-fn)
  // A strategy that searches PLY levels and then uses EVAL-FN.
  method (player, board)
    let (value, move) = minimax(player, board, ply, eval-fn);
    move;
  end method;
end method minimax-searcher;

define method alpha-beta (player, board, achievable, cutoff, ply, eval-fn)
  // Find the best move, for PLAYER, according to EVAL-FN,
  //   searching PLY levels deep and backing up values,
  //   using cutoffs whenever possible.
  if (ply = 0)
    eval-fn(player, board);
  else
    let moves = legal-moves(player, board);
    if (empty?(moves))
      if (any-legal-move?(opponent(player), board))
        - alpha-beta(opponent(player), board, (- cutoff), (- achievable),
                     (ply - 1), eval-fn);
      else
        final-value(player, board);
      end if;
    else
      let best-move = first(moves);
      for (move in moves, until achievable >= cutoff)
        begin
          let board2 = make-move(move, player, copy-board(board));
          let val
              = - alpha-beta(opponent(player), board2, (- cutoff),
                             (- achievable), (ply - 1), eval-fn);
          if (val > achievable) achievable := val; best-move := move; end if;
        end;
      end for;
      values(achievable, best-move);
    end if;
  end if;
end method alpha-beta;

define method alpha-beta-searcher (depth, eval-fn)
  // A strategy that searches to DEPTH and then uses EVAL-FN.
  method (player, board)
    let (value, move)
        = alpha-beta(player, board, losing-value, winning-value, depth,
                     eval-fn);
    move;
  end method;
end method alpha-beta-searcher;

define method modified-weighted-squares (player, board)
  // Like WEIGHTED-SQUARES, but don't take off for moving
  //   near an occupied corner.
  let w = weighted-squares(player, board);
  for (corner in #(11, 18, 81, 88))
    if (~ (bref(board, corner) == empty))
      for (c in neighbors(corner))
        if (~ (bref(board, c) == empty))
          inc!(w,
               (5 - *weights*[c])
                * if ((bref(board, c) == player)) 1; else -1; end if);
        end if;
      end for;
    end if;
  end for;
  w;
end method modified-weighted-squares;

begin
  let neighbor-table = make(<vector>, size: 100);
  //  Initialize the neighbor table
  for (square in all-squares)
    for (dir in all-directions)
      if (valid-p(square + dir))
        push!(square + dir, neighbor-table[square]);
      end if;
    end for;
  end for;
  define method neighbors (square)
    // Return a list of all squares adjacent to a square.
    neighbor-table[square];
  end method neighbors;
end;

begin
  let square-names
      = cross-product(symbol,
                      #(#"?", #"a", #"b", #"c", #"d", #"e", #"f", #"g", #"h",
                        #"?"),
                      #(#"?", 1, 2, 3, 4, 5, 6, 7, 8, #"?"));
  define method h8->88 (str)
    // Convert from alphanumeric to numeric square notation.
    find-key(square-names, curry(\==, as(<string>, str))) | str;
  end method h8->88;
  define method 88->h8 (num)
    // Convert from numeric to alphanumeric square notation.
    if (valid-p(num)) square-names[num]; else num; end if;
  end method 88->h8;
end;

define method human (player, board)
  // A human player for the game of Othello
  format-out("\n%c to move %S: ", name-of(player),
             map(88->h8, legal-moves(player, board)));
  h8->88(// LTD: Function READ not yet implemented.
         read());
end method human;

// The number of the move to be played
define variable *move-number* = 1;

define method othello (bl-strategy, wh-strategy, #key print = #t,
                       minutes = 30)
  // Play a game of othello.  Return the score, where a positive
  //   difference means black, the first player, wins.
  let board = initial-board();
  let clock = make(<array>, dimensions: 1 + max(black, white));
  block (game-over)
    for (*move-number* from 1,
         player = black then next-to-play(board, player, print),
         strategy = if (player == black)
                      bl-strategy;
                    else
                      wh-strategy;
                    end if then if (player == black)
                                  bl-strategy;
                                else
                                  wh-strategy;
                                end if,
         until empty?(player))
      get-move(strategy, player, board, print, clock);
    end for;
    if (print)
      format-out("\nThe game is over.  Final result:");
      print-board(board, clock);
    end if;
    count-difference(black, board);
  end block;
end method othello;

// A copy of the game clock
define variable *clock* = make(<vector>, size: 3);

// A copy of the game board
define variable *board* = initial-board();

define method get-move (strategy, player, board, print, clock)
  // Call the player's strategy function to get a move.
  //   Keep calling until a legal move is made.
  //  Note we don't pass the strategy function the REAL board.
  //  If we did, it could cheat by changing the pieces on the board.
  if (print) print-board(board, clock); end if;
  replace-subsequence!(*clock*, clock);
  let t0 = get-internal-real-time();
  let move = strategy(player, replace-subsequence!(*board*, board));
  let t1 = get-internal-real-time();
  dec!(clock[player], t1 - t0);
  if (clock[player] < 0)
    format-out("\n%c has no time left and forfeits.", name-of(player));
    game-over(if (player == black) -64; else 64; end if);
  elseif (move == #"resign")
    game-over(if (player == black) -64; else 64; end if);
  elseif (valid-p(move) & legal-p(move, player, board))
    if (print)
      format-out("\n%c moves to %S.", name-of(player), 88->h8(move));
    end if;
    make-move(move, player, board);
  else
    format-out("Illegal move: ~a", 88->h8(move));
    get-move(strategy, player, board, print, clock);
  end if;
end method get-move;

define method print-board (#key board = *board*, clock)
  // Print a board, along with some statistics.
  //  First print the header and the current score
  (method (s, #rest args)
     apply(maybe-initiate-xp-printing,
           method (xp, #rest args)
             begin
               multiple-newlines1(xp, fresh: 2);
               write-string++("    a b c d e f g h   [", xp, 0, 23);
               using-format(xp, "~c", pop!(args));
               write-char++('=', xp);
               using-format(xp, "~2a", pop!(args));
               write-char++(' ', xp);
               using-format(xp, "~c", pop!(args));
               write-char++('=', xp);
               using-format(xp, "~2a", pop!(args));
               write-string++(" (", xp, 0, 2);
               using-format(xp, "~@d", pop!(args));
               write-string++(")]", xp, 0, 2);
             end;
             if (args) copy-sequence(args); end if;
           end method,
           s, args);
   end method)(#t, name-of(black), cl-count(black, board), name-of(white),
               cl-count(white, board), count-difference(black, board));
  //  Print the board itself
  for (row from 1 to 8)
    format-out("\n  %d ", row);
    for (col from 1 to 8,
         piece = bref(board, col + 10 * row) then bref(board, col + 10 * row))
      format-out("%c ", name-of(piece));
    end for;
  end for;
  //  Finally print the time remaining for each player
  if (clock)
    (method (s, #rest args)
       apply(maybe-initiate-xp-printing,
             method (xp, #rest args)
               begin
                 write-string++("  [", xp, 0, 3);
                 using-format(xp, "~c", pop!(args));
                 write-char++('=', xp);
                 fluid-bind (*print-escape* = #f)
                   write+(pop!(args), xp);
                 end fluid-bind;
                 write-char++(' ', xp);
                 using-format(xp, "~c", pop!(args));
                 write-char++('=', xp);
                 fluid-bind (*print-escape* = #f)
                   write+(pop!(args), xp);
                 end fluid-bind;
                 write-char++(']', xp);
                 multiple-newlines1(xp, fresh: 2);
               end;
               if (args) copy-sequence(args); end if;
             end method,
             s, args);
     end method)(#t, name-of(black), time-string(clock[black]),
                 name-of(white), time-string(clock[white]));
  end if;
end method print-board;

define method time-string (time)
  // Return a string representing this internal time in min:secs.
  let (min, sec) = floor/(round/(time, $internal-time-units-per-second), 60);
  (method (s, #rest args)
     apply(maybe-initiate-xp-printing,
           method (xp, #rest args)
             begin
               using-format(xp, "~2d", pop!(args));
               write-char++(':', xp);
               using-format(xp, "~2,'0d", pop!(args));
             end;
             if (args) copy-sequence(args); end if;
           end method,
           s, args);
   end method)(#f, min, sec);
end method time-string;

define method random-othello-series (strategy1, strategy2, n-pairs,
                                     #key n-random = 10)
  // Play a series of 2*n games, starting from a random position.
  othello-series(switch-strategies(random-strategy, n-random, strategy1),
                 switch-strategies(random-strategy, n-random, strategy2),
                 n-pairs);
end method random-othello-series;

define method switch-strategies (strategy1, m, strategy2)
  // Make a new strategy that plays strategy1 for m moves,
  //   then plays according to strategy2.
  method (player, board)
    (if (*move-number* <= m) strategy1; else strategy2; end if)(player,
                                                                board);
  end method;
end method switch-strategies;

define method othello-series (strategy1, strategy2, n-pairs)
  // Play a series of 2*n-pairs games, swapping sides.
  let scores
      = begin
          let _acc = make(<deque>);
          for (% from 1 to n-pairs,
               random-state = // LTD: Function MAKE-RANDOM-STATE not yet implemented.
                              make-random-state() then // LTD: Function MAKE-RANDOM-STATE not yet implemented.
                                                       make-random-state())
            push-last(_acc, othello(strategy1, strategy2, #f));
            *random-state* := random-state;
            push-last(_acc, - othello(strategy2, strategy1, nil));
          finally
            _acc;
          end for;
        end;
  //  Return the number of wins (1/2 for a tie),
  //  the total of the point differences, and the
  //  scores themselves, all from strategy1's point of view.
  values(cl-count-if(positive?, scores) + cl-count-if(zero?, scores) / 2,
         apply(\+, scores), scores);
end method othello-series;

define method round-robin (strategies, n-pairs, #key n-random = 10,
                           names = strategies)
  // Play a tournament among the strategies.
  //   N-PAIRS = games each strategy plays as each color against
  //   each opponent.  So with N strategies, a total of
  //   N*(N-1)*N-PAIRS games are played.
  let n = size(strategies);
  let totals = make(<array>, dimensions: n);
  let scores = make(<array>, dimensions: list(n, n));
  //  Play the games
  for (i from 0 below n)
    for (j from i + 1 to n - 1)
      begin
        let wins
            = random-othello-series(strategies[i], strategies[j], n-pairs,
                                                              n-random);
        let losses = 2 * n-pairs - wins;
        inc!(scores[i, j], wins);
        inc!(scores[j, i], losses);
        inc!(totals[i], wins);
        inc!(totals[j], losses);
      end;
    end for;
  end for;
  //  Print the results
  for (i from 0 below n)
    (method (s, #rest args)
       apply(maybe-initiate-xp-printing,
             method (xp, #rest args)
               begin
                 pprint-newline+(fresh: xp);
                 fluid-bind (*print-escape* = #f)
                   write+(pop!(args), xp);
                 end fluid-bind;
                 pprint-tab+(line: 20, 1, xp);
                 write-char++(' ', xp);
                 using-format(xp, "~4f", pop!(args));
                 write-string++(": ", xp, 0, 2);
               end;
               if (args) copy-sequence(args); end if;
             end method,
             s, args);
     end method)(#t, names[i], totals[i]);
    for (j from 0 below n)
      (method (s, #rest args)
         apply(maybe-initiate-xp-printing,
               method (xp, #rest args)
                 begin
                   using-format(xp, "~4f", pop!(args));
                   write-char++(' ', xp);
                 end;
                 if (args) copy-sequence(args); end if;
               end method,
               s, args);
       end method)(#t, if (i = j) #"---"; else scores[i, j]; end if);
    end for;
  end for;
end method round-robin;

define method mobility (player, board)
  // The number of moves a player has.
  size(legal-moves(player, board));
end method mobility;

