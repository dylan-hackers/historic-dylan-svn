module: icfp2001
synopsis: Dylan Hackers entry in the Fourth Annual (2001) ICFP Programming Contest
authors: Andreas Bogk, Chris Double, Bruce Hoult
copyright: this program may be freely used by anyone, for any purpose

define method generate-output(input :: <stretchy-object-vector>)
 => strings :: <list>;
  let state = make(<generator-state>, text-runs: input);

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

/*
define method concatenate-lists(v :: <list>)
 => result :: <list>;
  let length = for (total = 0 then total + l.size,
		    l :: <list> in v)
	       finally total;
	       end for;
  
  let result :: <list> = make(<list>, size: length);
  let (init-state, limit, next-state, done?, current-key, current-element,
       current-element-setter) = forward-iteration-protocol(result);
  
  for (result-state = init-state
	 then for (item in lst,
		   state = result-state then next-state(result, state))
		current-element(result, state) := item;
	      finally state;
	      end for,
       lst :: <list> in v)
  end for;
  result;
end method concatenate-lists;
*/

define method concatenate-lists(v :: <sequence>)
 => result :: <simple-object-vector>;
  let length = for (total = 0 then total + l.size,
		    l :: <list> in v)
	       finally total;
	       end for;
  
  let result :: <simple-object-vector> = make(<simple-object-vector>, size: length);
  let (init-state, limit, next-state, done?, current-key, current-element,
       current-element-setter) = forward-iteration-protocol(result);
  
  for (result-state = init-state
	 then for (item in lst,
		   state = result-state then next-state(result, state))
		current-element(result, state) := item;
	      finally state;
	      end for,
       lst :: <list> in v)
  end for;
  result;
end method concatenate-lists;


define method generate-optimized-output(input :: <stretchy-object-vector>, #key run = 0, callback)
 => (strings :: <list>, exhausted :: <boolean>);
  let state = make(<generator-state>, text-runs: input);
  local
    method finished? (x :: <generator-state>) => result :: <boolean>;
      x.remaining-text-runs.size = 0 & x.open-tag-stack = #();
    end method finished?;
  local
    method successor-states (x) => states;
      let lists = map(generate-next-run, x);
      concatenate-lists(lists);
    end method successor-states;
  local 
    method cost-order (x :: <generator-state>, y :: <generator-state>) // XXX Tweak here
      if(x.maximum-cost == y.maximum-cost)
        maximum-transition-cost(x.from, x.to) < maximum-transition-cost(y.from, y.to);
      else
        x.maximum-cost < y.maximum-cost;
      end if;
    end method cost-order;
  local
    method see-if-best (x)
              callback(reverse!(x.output-tokens));
    end method see-if-best;

  if(run < 8)
    let beam-width = (run + 1) * 4;
    debug("Beam width: %=\n", beam-width);
    let (result-states, exhausted) = beam-search(state, successor-states, cost-order, finished?, beam-width: beam-width, callback: see-if-best);
    debug("Search done, exhaustive: %=\n", exhausted);
    
    if(result-states == #())
      values(#(), exhausted);
    else
      values(#() /* reverse!(result-states.head.output-tokens)*/ , exhausted);
    end if;
  else
    values(#(), #t);
  end if;
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
    while((~state.to.font-size & state.from.font-size) |
            (~state.to.color & state.from.color))
      state := pop-tag(state);
    end while;
    let pops = list(pop-done(state));
    while(state.open-tag-stack.head ~== #())
      state := pop-tag(state);
      pops := pair(pop-done(state), pops);
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
    list(push-tag(state, tag-PL));
  else
    list(push-empty-tag(state));
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

    map(curry(push-tag, state), applicable-tags(state));
  end;
end method generate-pushes;

// applies PL, pushes and pops and returns all result states
define method generate-next-run(state :: <generator-state>)
 => successor-states :: <list>;
  if(state.output-state == #"closing")
    generate-pops(state);
  elseif(state.output-state == #"emit-pl")
    generate-pl(state);
  else
    generate-pushes(state);
  end if;
end method generate-next-run;