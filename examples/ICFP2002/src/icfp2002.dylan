module: icfp2002
synopsis: Dylan Hackers entry in the Fifth Annual (2002) ICFP Programming Contest
copyright: this program may be freely used by anyone, for any purpose


define function play-the-game(bot :: <class>, input :: <stream>, output :: <stream>) => ();
  send-player(output);
  force-output(output);
  let (my-id :: <integer>, 
       my-capacity :: <integer>, 
       my-money :: <integer>, 
       state :: <state>) = receive-initial-setup(input);
  
  let agent = make(bot, agent-id: my-id);
  find-robot(state, my-id).capacity := my-capacity;

  debug("board is %=", state.board);
//  test-path-finding(state.board);

  let current-bot = bot;
  let running = #t;
  let last-bot = find-robot(state, agent.agent-id);
  while(running)
    debug("Robot state: %=\n", last-bot);
    *debug* | always-print("Where: %=, Score: %d, Money: %=\n",
                           last-bot.location,  last-bot.score, last-bot.money);
    state := receive-server-packages(input, state, last-bot.location);
    let move = #f;
    let not-crashed = #f;
    until(not-crashed = #t)
      block()
	move := generate-next-move(agent, state);
	not-crashed := #t
      exception(cond :: <condition>)
	always-print("!!!!!!!!!! BOT CRASH !!!!!!!!!!\n");
	always-print("!!!!!!!!!! BOT CRASH !!!!!!!!!!\n");
	always-print("!!!!!!!!!! BOT CRASH !!!!!!!!!!\n");
	if(current-bot = <gabot>)
	  always-print("changing to pushbot\n");
	  current-bot := <pushbot>;
	elseif(current-bot = <pushbot>)
	  always-print("changing to thomas\n");
	  current-bot := <thomas>;
	else
	  always-print("changing to gabot\n");
	  current-bot := <gabot>;
	end;
	agent := make(current-bot, agent-id: my-id,
		      //visited-bases: last-bot.visited-bases,
		      id: last-bot.id, location: last-bot.location,
		      money: last-bot.money, inventory: last-bot.inventory,
		      score: last-bot.score);
	
      end block;
    end;
    send-command(output, move);
    state := receive-server-command-reply(input, state);
    let bot = find-robot(state, agent.agent-id);
    if(~bot)
      debug("Game Over. Last bot: %=\n", last-bot);
      running := #f;
    else
      last-bot := bot;
    end if;
  end while;  
end function play-the-game;



// Testing path finding.
define method test-path-finding(board :: <board>)
  debug("Trying a simple path finding from (1, 1) to (2, 12).\n");
  let path = find-path(point(x: 1, y: 1),
                       point(x: 2, y: 12),
                       board);
  if (path = #f)
    debug("Sorry, no path found.\n");
  else
    debug("Resulting path of length: %=.\n", path.size);

    debug("Here is the board with the path on it:\n");
    debug("Size: width is %=, height is %=.\n",
          board.width, board.height);
    for (r from board.height to 1 by -1)
      for (c from 1 to board.width)
        if (member?(point(x: c, y: r), path, test: \=))
          debug("0");
        else
          debug("%=", board[r, c]);
        end if;
      end for;
      debug("\n");
    end for;
  end if;
end method test-path-finding;

// Test tour finding

define function test-the-tour(input :: <stream>, output :: <stream>) => ();
  send-player(output);
  force-output(output);
  let (my-id :: <integer>, 
       my-capacity :: <integer>, 
       my-money :: <integer>, 
       state :: <state>) = receive-initial-setup(input);
  
  test-tour-finding(state.board);
end function test-the-tour;

define method test-tour-finding (board :: <board>)
  debug("The board is:\n%=", board);
  let to-visit = list(point(x: 1, y:1),
                      point(x: 2, y:9),
                      point(x: 3, y:7),
                      point(x: 11, y: 11));
  block()
    let seq = find-tour(to-visit.head, to-visit, board);
    debug("seq = %=\n", seq);
    let t = make(<table>);
    let first-step? = #t;
    let start = seq.first;
    //
    // Initialize t so that we know about every point on the tour.
    // 
    t[start] := #"visit-point";
    for (dest in subsequence(seq, start: 1))
      for (pt in find-path(start, dest, board))
        t[pt] := #t;
      finally 
        start := dest;
        t[start] := #"visit-point";        
      end for;
    end for;
    //
    for (r from board.height to 1 by -1)
      for (c from 1 to board.width)
        select (element(t, point(x: c, y: r), default: #f))
          #t             => debug("*");
          #"visit-point" => debug("O");
          #f             => debug("%=", board[r, c]);
        end select;
      end for;
      debug("\n");
    end for;
  exception (e :: <no-path-error>)
    debug("There is no path between %= and %=.\n", e.path-start, e.path-finish);
  end block;
end method test-tour-finding;


define function figure-out-which-bot(bot-type :: <string>)
 => bot-class :: <class>;
  debug("bot-type: %s\n", bot-type);
  case
    bot-type = "dumber-bot"
      => <dumber-bot>;
    bot-type = "dumbot"
      => <dumbot>;
    bot-type = "pushbot"
      => <pushbot>;
    bot-type = "thomas"
      => <thomas>;
    bot-type = "gabot"
      => <gabot>;
    otherwise => <dumber-bot>;
  end case;
end;


define function main(name, arguments)

  if(arguments.size < 2)
    format-out("Wrong number of arguments passed.\n");
  end if;
  let (input-stream, output-stream) 
    = tcp-client-connection(arguments[0], string-to-integer(arguments[1]));
  let bot-type = arguments.size > 2
                 & arguments[2]
                 | "dumber-bot";
  when (arguments.size > 3 & arguments[3] = "-nodebug")
    *debug* := #f;
  end when;
  when (arguments.size > 3 & arguments[3] = "-debug")
    *debug* := #t;
  end when;
  block ()
    if (bot-type = "tour-test")
      test-the-tour(input-stream, output-stream)
    else 
        play-the-game(figure-out-which-bot(bot-type),
                      input-stream,
                      output-stream);
    end if;
  cleanup
    close(output-stream);
  exception (err :: <error>)
    report-and-flush-error(err);
    exit-application(1);
  end block;

  exit-application(0);
end function main;


main(application-name(), application-arguments());
