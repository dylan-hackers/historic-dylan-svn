module: icfp2001
synopsis: Dylan Hackers entry in the Fourth Annual (2001) ICFP Programming Contest
authors: Andreas Bogk, Chris Double, Bruce Hoult
copyright: this program may be freely used by anyone, for any purpose

define method breadth-first-search(initial-state, make-successors, cost-function, finished?)
  let states = list(initial-state);
  let terminal-states = #();

  local 
    method cost-order (x :: <generator-state>, y :: <generator-state>)
      x.cost-function < y.cost-function;
    end method cost-order;

  while(states ~= #())
  let new-states = #();
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

  sort(terminal-states, test: cost-order);
end method breadth-first-search;

define method beam-search(initial-state, make-successors, cost-function, finished?, #key beam-width = 12)
  let states = list(initial-state);
  let terminal-states = #();

  local 
    method cost-order (x :: <generator-state>, y :: <generator-state>)
      x.cost-function < y.cost-function;
    end method cost-order;

  while(states ~= #())
  let new-states = #();
    let new-states = #();
    for(i in make-successors(states))
      if(finished?(i))
        terminal-states := pair(i, terminal-states);
      else
        new-states := pair(i, new-states);
      end if;
    end for;
    states := subsequence(sort(new-states, test: cost-order), end: beam-width);
  end while;

  sort(terminal-states, test: cost-order);
end method beam-search;