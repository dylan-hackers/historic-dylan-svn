module: ants

define function play-game(red-brain :: <string>,
                          black-brain :: <string>,
                          world :: <string>)
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
  format-out("random seed: %d\n", *initial-random-seed*);
  for(round from 0 below 100000)
    format-out("\nAfter round %d...\n", round);
    dump-world-state(*world*);
    for(i from 0 below *ants*.size)
      step(i);
    end for;
  end for;
end function play-game;

begin
  apply(play-game, application-arguments());
end;
