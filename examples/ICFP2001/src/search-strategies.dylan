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

define inline method beam-search(initial-state :: <generator-state>, make-successors :: <function>, cost-order :: <function>, finished? :: <function>, #key beam-width = 12, callback)
  let states :: <subsequence> = subsequence(vector(initial-state));
  let terminal-states :: <list> = #();
  let exhausted :: <boolean>    = #t;
  let best-match = initial-state.maximum-cost;

  while(states.size > 0)
    check-timeout();
//    GC-gcollect();
    states := subsequence(sort(states, test: cost-order), end: beam-width);
    let new-states :: <stretchy-object-vector> = make(<stretchy-object-vector>);
    for(i in make-successors(states))
//      print-state(i);
      if(i.output-state == #"finished")
        if(i.maximum-cost < best-match)
//          print-state(i);
          terminal-states := pair(i, terminal-states);
          callback(i);
          best-match := i.maximum-cost;
        end if;
      else
        if(i.maximum-cost < best-match + 77)
          new-states := add!(new-states, i);
        end if;
      end if;
    end for;
//    debug("%= states generated.\n", new-states.size);
    if (new-states.size > beam-width)
      exhausted := #f;
    end;
    states := subsequence(new-states);
  end while;

  if(terminal-states = #())
    values(#(), exhausted);
  else
    values(sort(terminal-states, test: cost-order), exhausted);
  end if;
end method beam-search;