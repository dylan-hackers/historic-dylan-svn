module: icfp2002
synopsis: Dylan Hackers entry in the Fifth Annual (2002) ICFP Programming Contest
copyright: this program may be freely used by anyone, for any purpose


define function read-configuration(stream :: <stream>)
 => (id :: <integer>, capacity :: <integer>, money :: <integer>);
  values(map(string-to-integer, split(" ", read-line(stream))))
end function read-configuration;


define function play-the-game(bot :: <class>, input :: <stream>, output :: <stream>) => ();
  send-player(output);
  force-output(output);
  let (my-id :: <integer>, 
       my-capacity :: <integer>, 
       my-money :: <integer>, 
       state :: <state>) = receive-initial-setup(input);
  let agent = make(bot, robot: find-robot(state, my-id));


  debug("board is %=", state.board);
  test-path-finding(state.board);

  let running = #t;
  while(running)
    state := receive-server-packages(input, state, find-robot(state, agent.robot.id).location);
    let move = generate-next-move(agent, state);
    send-command(output, move);
    state := receive-server-command-reply(input, state);
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

  play-the-game(figure-out-which-bot(bot-type), input-stream, output-stream);

  /*
  let running = #t;
  while(running)
    let line = read-line(input-stream, on-end-of-stream: #f);
    if(line)
      write-line(output-stream, line);
      force-output(output-stream);
    else
      running := #f;
    end if;
  end while;
  */

  close(output-stream);
  exit-application(0);
end function main;


main(application-name(), application-arguments());
