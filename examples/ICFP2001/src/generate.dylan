module: icfp2001
synopsis: Dylan Hackers entry in the Fourth Annual (2001) ICFP Programming Contest
authors: Andreas Bogk, Chris Double, Bruce Hoult
copyright: this program may be freely used by anyone, for any purpose

define method generate-output(input :: <stretchy-object-vector>)
 => strings :: <list>;
  let state = make(<generator-state>, text-runs: input);
  force-output(*standard-error*);

  while(state.remaining-text-runs.size > 0)
    check-timeout();
    let from :: <attribute> = state.from;
    let to   :: <attribute> = state.to;

    if(#f)
      print-state(state);
    end if;

    local
      method can-pop-to-attr(name)
        from.name ~== to.name &
          member?(to, state.attribute-stack,
                  test: method(x :: <attribute>, y :: <attribute>)
                            x.name == y.name;
                        end);
      end method can-pop-to-attr;
      
    let pop =
      (state.attribute-stack ~== #() 
         & state.attribute-stack.tail ~== #() 
         & to.value = state.attribute-stack.second.value) |
      (from.bold & ~to.bold) |
      (from.italic & ~to.italic) |
      (from.strong & ~to.strong) |
      (from.typewriter & ~to.typewriter) |
      (from.underline > to.underline) |
      (from.font-size & ~to.font-size) |
      (from.color & ~to.color) |
      can-pop-to-attr(emphasis) |
      can-pop-to-attr(font-size) |
      can-pop-to-attr(color);

    if(from.value = to.value)
      // same attributes .. just output the text
      emit-text!(state);
    elseif(pop)
      pop-tag!(state);
    else
      do(curry(push-tag!, state), applicable-tags(state));
    end if;
  end while;
  while(state.open-tag-stack.head ~== #())
    pop-tag!(state);
  end while;
  reverse!(state.output-tokens);
end method generate-output;

define method generate-optimized-output(input :: <stretchy-object-vector>)
 => strings :: <list>;
  let state = make(<generator-state>, text-runs: input);
  local
    method finished? (x :: <generator-state>) => result :: <boolean>;
      x.remaining-text-runs.size = 0;
    end method finished?;
  local
    method successor-states (x) => states;
      apply(concatenate, map(generate-next-run, x));
    end method successor-states;
              
  let result-states = beam-search(state, successor-states, maximum-cost, finished?);
  for(i in result-states)
    print-state(i);
  end for;
  reverse!(result-states.head.output-tokens);
end method generate-optimized-output;

// returns all states reachable by popping, including itself (no pop)
// when chance to finish, return only finished state
define method generate-pops(state :: <generator-state>)
 => successor-states :: <list>;
  if(state.remaining-text-runs.size = 0) // We're done.
    while(state.open-tag-stack.head ~== #())
      state := pop-tag(state);
    end while;
    list(state);
  else
    let pops = list(state);
    while(state.open-tag-stack.head ~== #())
      state := pop-tag(state);
      pops := pair(state, pops);
    end while;
    pops;
  end;
end method generate-pops;

// returns itself and itself with applied PL
define method generate-pl(state :: <generator-state>)
 => successor-states :: <list>;
  let from :: <attribute> = state.from;
  let to   :: <attribute> = state.to;

  if((from.strong       & ~to.strong) |
       (from.typewriter & ~to.typewriter) |
       (from.emphasis   & ~to.emphasis) |
       (from.italic     & ~to.italic) |
       (from.bold       & ~to.bold) |
       (from.underline > 0 & to.underline = 0))
    list(push-tag(state, tag-PL), state);
  else
    list(state);
  end if;
end method generate-pl;

// returns all states that lead to actual emission of text
define method generate-pushes(state :: <generator-state>)
 => successor-states :: <list>;
  block(return)
    let from :: <attribute> = state.from;
    let to   :: <attribute> = state.to;

    if(from.value = to.value)
      return(list(emit-text(state)));
    end;

    let tags :: <list>      = applicable-tags(state);

    apply(concatenate, #(), map(generate-pushes, map(curry(push-tag, state), tags)));
  end;
end method generate-pushes;

// applies PL, pushes and pops and returns all result states
define method generate-next-run(state :: <generator-state>)
 => successor-states :: <list>;
  let new-states = generate-pl(state);

  new-states := apply(concatenate, #(), map(generate-pushes, new-states));
  new-states := apply(concatenate, #(), map(generate-pops, new-states));
  new-states;
end method generate-next-run;