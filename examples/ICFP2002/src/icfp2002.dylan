module: icfp2002
synopsis: Dylan Hackers entry in the Fifth Annual (2002) ICFP Programming Contest
copyright: this program may be freely used by anyone, for any purpose

define function debug(#rest args)
  apply(format, *standard-error*, args);
  force-output(*standard-error*);
end function debug;

define function read-configuration(stream :: <stream>)
 => (id :: <integer>, capacity :: <integer>, money :: <integer>);
  values(map(string-to-integer, split(" ", read-line(stream))))
end function read-configuration;

define function play-the-game(input :: <stream>, output :: <stream>) => ();
  send-player(output);
  force-output(output);
  let (my-id :: <integer>, 
       my-capacity :: <integer>, 
       my-money :: <integer>, 
       state :: <state>) = receive-initial-setup(input);
  let agent = make(<dumbot>, id: my-id, 
                   capacity: my-capacity, money: my-money);

  debug("board is %=", state.board);

/*
  let running = #t;
  while(running)
    state := read-packages(input, state);
    let move = generate-next-move(agent, state);
    send-move(output, move);
    state := read-movements(input, state);
  end while;
  */
end function play-the-game;

define function main(name, arguments)
  if(arguments.size < 2)
    format-out("Wrong number of arguments passed.\n");
  end if;
  let (input-stream, output-stream) 
    = tcp-client-connection(arguments[0], string-to-integer(arguments[1]));

  play-the-game(input-stream, output-stream);

/*  let running = #t;
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
