module: ants

define class <brain> (<object>)
  slot name :: <byte-string>, required-init-keyword: name:;
  slot code :: <vector> = #[];
  slot played :: <integer> = 0;
  slot score :: <integer> = 0;
  slot food :: <integer> = 0;
end class <brain>;

define function load-brain(b :: <brain>)
  with-open-file(s = b.name)
    b.code := read-state-machine(s)
  end with-open-file;
end function load-brain;

define function save-brain(b :: <brain>)
  with-open-file(s = b.name, direction: #"output", if-exists: #"overwrite")
    write-brain(s, b);
  end with-open-file;
end;

define function clone-brain(b :: <brain>)
 => (b* :: <brain>)
  let (match-begin, match-end) 
    = regexp-position(b.name, "\\.clone\\.(\\d+)$");
  let clone-count = 
    if(match-begin)
      string-to-integer(b.name, start: match-begin, end: match-end)
    else
      0
    end if;
  let new-name = "";
  iterate loop()
    clone-count := random(65536);
    new-name := concatenate(copy-sequence(b.name, end: match-begin),
                            ".clone.",
                            integer-to-string(clone-count, base: 16, size: 4));
    if(file-exists?(new-name))
      loop()
    end if;
  end iterate;

  let new-brain = make(<brain>, name: new-name);
  with-open-file(brain-stream = b.name)
    new-brain.code := read-state-machine(brain-stream)
  end with-open-file;
  save-brain(new-brain);
  new-brain;
end function clone-brain;

define function write-brain(s :: <stream>, b :: <brain>)
  map(compose(curry(write-line, s), unparse), b.code)
end function write-brain;

define function mutate-brain(b :: <brain>, #key deviation = 1.0)
  for(insn in b.code)
    if(instance?(insn, <flip>))
      let delta = random-gaussian(standard-deviation: deviation);
      let adjusted-probability = insn.probability + round(delta);
      if(adjusted-probability < 1)
        let temp = insn.state-success;
        insn.state-success := insn.state-failure;
        insn.state-failure := temp;
        adjusted-probability := -1 * (adjusted-probability - 1) + 1;
      end if;
      insn.probability := adjusted-probability;
    end if;
  end for;
end function mutate-brain;

define function crossover-brain(b :: <brain>, other :: <brain>)
  block(return)
    if(b.code.size ~= other.code.size)
      format-out("Warning: trying to breed incompatible genes!\n");
      return();
    end if;
    for(ins in b.code, other-ins :: <instruction> in other.code, i from 0)
      if(instance?(ins, <flip>))
        unless(instance?(other-ins, <flip>))
          format-out("Warning: trying to breed incompatible genes!\n");
          return();
        end unless;
        if(random(2) == 0)
          ins.probability := other-ins.probability;
          ins.state-success := other-ins.state-success;
          ins.state-failure := other-ins.state-failure;
        end if;
      end if;
    end for;
  end block;
end function crossover-brain;

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

define function run-breeding()
  let b = make(<brain>, name: application-arguments()[0]);
  load-brain(b);

  let brains = make(<stretchy-vector>);
  add!(brains, b);

  for(i from 0 below 20)
    let b* = clone-brain(b);
    mutate-brain(b*);
    save-brain(b*);
    add!(brains, b*);
  end for;

  let worlds = 
    with-open-file(worlds-stream = "world-list")
      read-lines(worlds-stream)
    end with-open-file;

  for(i from 0 below 100)
    run-single-tournament(brains, worlds);
    do(method(x) 
           format-out("%=\ttotal rounds: %=\twin %%: %=\n", 
                      x.name, x.played, win-percent(x))
       end method, sort(brains, 
                        test: method(x, y) 
                                  let xx = win-percent(x);
                                  let yy = win-percent(y);
                                  if(xx = "n/a")
                                    #f
                                  else
                                    if(yy = "n/a")
                                      #t
                                    else
                                      xx > yy
                                    end if;
                                  end if;
                              end));
    force-output(*standard-output*);
  end for;
end function run-breeding;

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