module: ants

define class <brain> (<object>)
  slot name :: <byte-string>, required-init-keyword: name:;
  slot code :: <vector> = #[];
end class <brain>;

define function load-brain(b :: <brain>)
  with-open-file(s = b.name)
    b.code := read-state-machine(s)
  end with-open-file;
end function load-brain;

define function save-brain(b :: <brain>)
  with-open-file(s = b.name)
    write-brain(s, b);
  end with-open-file;
end;

define function write-brain(s :: <stream>, b :: <brain>)
  map(compose(curry(write-line, s), unparse), b.code)
end function write-brain;

define function run-single-tournament(brains, worlds)
  let brain1 = brains[random(brains.size)];
  let brain2 = brains[random(brains.size)];
  let world = worlds[random(worlds.size)];

  format-out("Ready to battle %= (red) against %= (black) on %=\n",
             brain1.name, brain2.name, world);
  
  *red-brain* := brain1.code;
  *black-brain* := brain2.code;
  with-open-file(world-stream = world)
    *world* := read-map(world-stream);
  end with-open-file;

  for (round from 0 to 100000)
    for(i from 0 below *ants*.size)
      step(i);
    end for;
  end for;

  // Produce summary of "the match".
  dump-world-summary(*world*, brain1.name, brain2.name, world);

end function run-single-tournament;

begin
  let args = application-arguments();
  let brains =
    with-open-file(brain-stream = args[0])
      map(method(name) make(<brain>, name: name) end, 
          read-lines(brain-stream));
    end with-open-file;
  do(load-brain, brains);

  let worlds = 
    with-open-file(worlds-stream = args[1])
      read-lines(worlds-stream)
    end with-open-file;

  run-single-tournament(brains, worlds);
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