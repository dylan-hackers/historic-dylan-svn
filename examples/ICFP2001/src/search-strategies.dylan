module: icfp2001
synopsis: Dylan Hackers entry in the Fourth Annual (2001) ICFP Programming Contest
authors: Andreas Bogk, Chris Double, Bruce Hoult
copyright: this program may be freely used by anyone, for any purpose

define method breadth-first-search(initial-state, make-successors, cost-order, finished?)
  let states = list(initial-state);
  let terminal-states = #();

  while(states ~= #())
  let new-states = #();
    check-timeout();
    let new-states = #();
    for(i in make-successors(states))
      if(finished?(i))
        terminal-states := pair(i, terminal-states);
      else
        new-states := pair(i, new-states);
      end if;
    end for;
    states := new-states;
  end while;

  values(sort(terminal-states, test: cost-order), #t);
end method breadth-first-search;

define method beam-search(initial-state, make-successors, cost-order, finished?, #key beam-width = 12)
  let states = list(initial-state);
  let terminal-states = #();
  let exhausted = #t;

  while(states ~= #())
    check-timeout();
    GC-gcollect();
    states := subsequence(sort(states, test: cost-order), end: beam-width);
    let new-states = #();
    for(i in make-successors(states))
//      print-state(i);
      if(finished?(i))
        terminal-states := pair(i, terminal-states);
      else
        new-states := pair(i, new-states);
      end if;
    end for;
    debug("%= states generated.\n", new-states.size);
    if (new-states ~= #() & new-states.size > beam-width)
      exhausted := #f;
    end;
    states := new-states;
  end while;

  if(terminal-states = #())
    values(#(), exhausted);
  else
    values(sort(terminal-states, test: cost-order), exhausted);
  end if;
end method beam-search;