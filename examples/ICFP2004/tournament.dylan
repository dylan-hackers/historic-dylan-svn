module: ants

define function win-percent(brain) => (percentage)
    if (brain.played == 0)
        "n/a";
    else
        round(100.0s0 * (as(<single-float>, brain.score) /
                         as(<single-float>, brain.played)));
    end;
end;

define function run-single-tournament(brains, worlds)
  let brain1 = brains[random(brains.size)];
  let brain2 = brains[random(brains.size)];
  while(brain1 == brain2)
    brain2 := brains[random(brains.size)];
  end while;
  let world = worlds[random(worlds.size)];
  
  brain1.played := brain1.played + 1;
  brain2.played := brain2.played + 1;

  *red-brain* := brain1.code;
  *black-brain* := brain2.code;
  *ants* := make(<stretchy-vector>);
  with-open-file(world-stream = world)
    *world* := read-map(world-stream);
  end with-open-file;

  for (round from 0 to 100000)
    for(i from 0 below *ants*.size)
      step(i);
    end for;
  end for;

  // Produce summary of "the match".
  let winner = dump-world-summary(*world*, brain1.name, brain2.name, world);

  if(winner == #"red")
    brain1.score := brain1.score + 1;
  else
    brain2.score := brain2.score + 1;
  end if;
end function run-single-tournament;

define function run-tournament()
  let brains =
    with-open-file(brain-stream = "contestants")
      map(method(name) make(<brain>, name: name) end, 
          read-lines(brain-stream));
    end with-open-file;
  do(load-brain, brains);

  let worlds = 
    with-open-file(worlds-stream = "world-list")
      read-lines(worlds-stream)
    end with-open-file;

  for(i from 0 below 100)
    run-single-tournament(brains, worlds);
    do(method(x) 
           format-out("%=\ttotal rounds: %=\twin %%: %=\n", 
                      x.name, x.played, win-percent(x))
       end method, brains);
    force-output(*standard-output*);
  end for;
end;

define function read-lines(s :: <stream>)
 => (<vector>)
  let result = make(<stretchy-vector>);
  while(~stream-at-end?(s))
    let line = read-line(s);
    add!(result, line);
  end while;
  result;
end function read-lines;

run-tournament();

/*
begin
  let b = make(<brain>, name: application-arguments()[0]);
  load-brain(b);
  let b* = clone-brain(b);
  mutate-brain(b*);
  let b** = clone-brain(b);
  mutate-brain(b*);
  mutate-brain(b**);
  crossover-brain(b*, b**);
  save-brain(b*);
  save-brain(b**);
end;*/