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

