module: icfp2002
synopsis: Dylan Hackers entry in the Fifth Annual (2002) ICFP Programming Contest
copyright: this program may be freely used by anyone, for any purpose

define function debug(#rest args)
  apply(format, *standard-error*, args);
  force-output(*standard-error*);
end function debug;

/*
define function play-the-game(input :: <stream>, output :: <stream>) => ();
  write-line(output, "Player");
  force-output(*standard-error*);
  let board = read-board(input);
  let (my-id, my-capacity, my-money) = read-configuration(input);
  let agent = make(<robot-agent>, id: my-id, 
                   capacity: my-capacity, money: my-money);
  board := read-robots(input, board);

  let running = #t;
  while(running)
    board := read-packages(input, board);
    let move = generate-next-move(agent, board);
    send-move(output, move);
    board := read-movements(input, board);
  end while;
end function play-the-game;
  
generate-next-move(agent :: <robot-agent>, state :: <board>) => (action :: <command>)
*/

define function main(name, arguments)
  if(arguments.size < 2)
    format-out("Wrong number of arguments passed.\n");
  end if;
  let (input-stream, output-stream) 
    = tcp-client-connection(arguments[0], string-to-integer(arguments[1]));
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
  close(input-stream);
  close(output-stream);
  exit-application(0);
end function main;

main(application-name(), application-arguments());
