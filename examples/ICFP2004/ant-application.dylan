module: ants

define function play-game(red-brain :: <string>,
                          black-brain :: <string>,
                          world :: <string>,
                          #key dump? = #f,
                               iterations = 100000)
 => ()
  with-open-file(brain = red-brain)
    *red-brain* := read-state-machine(brain)
  end with-open-file;
  with-open-file(brain = black-brain)
    *black-brain* := read-state-machine(brain)
  end with-open-file;
  with-open-file(world-stream = world)
    *world* := read-map(world-stream)
  end with-open-file;

  // Produce a dump.
  // format-out("random seed: %d\n", *initial-random-seed*);
  for (round from 0 to iterations)
    if (dump?)
      format-out("\nAfter round %d...\n", round);
      dump-world-state(*world*);
    end if;
    
    for(i from 0 below *ants*.size)
      step(i);
    end for;
  end for;

  unless(dump?)
    // Produce summary of "the match".
    dump-world-summary(*world*, red-brain, black-brain, world);
  end unless;
end function play-game;

define function usage()
  format-out("usage: ant-application "
               "[-dump] [-iterations n] red-brain black-brain world\n");
  exit-application(1);
end function;

begin
  let args = application-arguments();
  let dump? = #f;
  let iterations = 100000;
  
  iterate loop (i = 0)
    if (i = args.size)
      usage();
    elseif (args[i] = "-dump")
      dump? := #t;
      loop(i + 1);
    elseif (args[i] = "-iterations" & i + 1 < args.size)
      iterations := string-to-integer(args[i + 1]);
      loop(i + 2);
    elseif (i + 3 = args.size)
      play-game(args[i], args[i + 1], args[i + 2],
                dump?: dump?, iterations: iterations);
    end if;
  end iterate;
end;
