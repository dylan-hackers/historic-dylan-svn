module: icfp2001
synopsis: Dylan Hackers entry in the Fourth Annual (2001) ICFP Programming Contest
authors: Andreas Bogk, Chris Double, Bruce Hoult
copyright: this program may be freely used by anyone, for any purpose

define class <opt-state> (<object>)
  slot stack-depth :: <integer> = 0,
    init-keyword: stack-depth:;
  slot stack-cost :: <integer> = 0,
    init-keyword: stack-cost:;
  slot tag-stack   :: <list> = #(),  // contains <tag>s
    init-keyword: tags:;
  slot attr-stack   :: <list> = #(), // contains <attribute>s
    init-keyword: attrs:;

  slot transitions :: <list> = #(),  // contains strings
    init-keyword: text:;
  slot output-size :: <integer> = 0,
    init-keyword: size:;
  slot attr :: <attribute> = make(<attribute>),
    init-keyword: attr:;
end class <opt-state>;

define sealed domain make(singleton(<opt-state>));
define sealed domain initialize(<opt-state>);


define method dump-state(i :: <integer>, s :: <opt-state>) => ();
  debug("State %d: %d + %d = %d bytes (vuln=%d) ",
	i,
	s.output-size, s.stack-cost,
	s.output-size + s.stack-cost,
	// an approx to how easy it is to get to this state from elsewhere
	s.output-size - (s.stack-cost - s.stack-depth));
  describe-attributes(s.attr);
  debug("    stack %d: ", s.stack-depth);
  for (e :: <tag> in s.tag-stack)
    debug("%s", e.close-tag);
  end;
  debug("\n");

//  for (s :: <byte-string> in s.transitions.reverse)
//    debug("%s", s);
//  end;
//  debug("\n");
end method dump-state;

define function dump-states(states :: <stretchy-object-vector>) => ();
  for (state :: <opt-state> keyed-by i in states)
    dump-state(i, state);
  end;
end function dump-states;


define function compress!(v :: <stretchy-object-vector>) => ();
  // remove any elements that are #f
  let out = 0;
  for (elt in v)
    if (elt)
      v[out] := elt; // can overwrite self
      out := out + 1;
    end;
  end;
  v.size := out;
end function compress!;


define function optimize-output(input :: <stretchy-object-vector>)
 => strings :: <list>;

  debug("starting optimize-output\n");

  let states :: <stretchy-object-vector> = make(<stretchy-vector>);
  add!(states, make(<opt-state>));

  let num-steps = input.size;
  let last-pct = 0;
  for (fragment :: <attributed-string> keyed-by i in input)
    debug("\n--------------------------------\n");
    dump-attributed-string(fragment);
    debug("#starting states %d\n", states.size);
    dump-states(states);
    debug("--------------------------------\n");

    let desired = fragment.attributes;
    let text = fragment.string;
    let next-states :: <stretchy-object-vector> = make(<stretchy-vector>);
    for (state :: <opt-state> in states)
      check-timeout();
      emit-transitions(state.attr, desired, text,
		       state.tag-stack, state.attr-stack,
		       state.stack-depth, state.stack-cost,
		       state.transitions, state.output-size,
		       0, 2, // gratuitous pop limit
		       #f, next-states);
    end;

    dump-states(next-states);
    debug("\n .. about to prune identical stacks ...\n");

    next-states :=
      sort!(next-states,
	    test: method(left :: <opt-state>, right :: <opt-state>)
		   => left-less :: <boolean>;
		      if (left.stack-cost ~= right.stack-cost)
			left.stack-cost < right.stack-cost
		      elseif (left.stack-depth ~= right.stack-depth)
			left.stack-depth < right.stack-depth
		      else
			left.output-size > right.output-size
		      end;
		  end);

    // check for identical stacks and remove all but the shortest
    for (left :: <opt-state> keyed-by left-i in next-states)
      block (goto-next-state)
	for (right-i from left-i + 1 below next-states.size)
	  let right :: <opt-state> = next-states[right-i];
	  if (left.stack-cost ~== right.stack-cost |
		left.stack-depth ~== right.stack-depth)
	    goto-next-state();
	  end;
	  if (every?(\==, left.tag-stack, right.tag-stack))
	    // just delete it!
	    next-states[left-i] := #f;
	    goto-next-state();
	  end;
	end for;
      end;
    end for;
    compress!(next-states);

    dump-states(next-states);
    debug("\n\n .. about to prune states that can be got to more easily ...\n\n");

    // first find what is the shortest if you close all its tags...
    let shortest = 999999999;
    for (elt :: <opt-state> in next-states)
      let len = elt.output-size + elt.stack-cost;
      if (len < shortest)
	shortest := len;
      end;
    end;

    debug("smallest is %d bytes\n", shortest);

    // ... then delete anything vulnerable to it
    for (elt :: <opt-state> keyed-by i in next-states)
      let vulnerability = elt.output-size - (elt.stack-cost - elt.stack-depth);
      if (shortest <= vulnerability &
	    // be careful not to delete the shortest!
	    elt.stack-depth ~== 0)
	debug("deleting item %d\n", i);
	next-states[i] := #f;
      end;
    end;
    compress!(next-states);


    debug("\n\ntrying to do more accurate transition cost estimate\n\n");

    // I don't know what sort order to use here, so try to wing it
    // let's try output-size descending
    next-states :=
      sort!(next-states,
	    test: method(left :: <opt-state>, right :: <opt-state>)
		   => left-less :: <boolean>;
		      left.output-size > right.output-size;
		  end);
    dump-states(next-states);

    local
      method can-jaunt-state?
	  (from-stack :: <list>, from-depth :: <integer>,
	   to-stack :: <list>, to-depth :: <integer>,
	   funds :: <integer>, loan :: <integer>)
       => ans :: <boolean>;
	//debug("Entering can-jaunt-state? with $%d ($%d)\n", funds, loan);
	//debug("  %d %=\n", from-depth, from-stack ~= #() & from-stack.head.open-tag);
	//debug("  %d %=\n", to-depth, to-stack ~= #() & to-stack.head.open-tag);

	if (funds < 0)
	  #f;
	elseif (from-depth == to-depth)
	  if (from-depth == 0)
	    // we'll clear the stack and rebuild,
	    // but we made it!
	    #t;
	  elseif (from-stack == to-stack)
	    // same object means they're all the same
	    // from here on, and we still have funds
	    #t;
	  else
	    let from-tag :: <tag> = from-stack.head;
	    let to-tag :: <tag> = to-stack.head;
	    if (from-tag == to-tag)
	      // maybe .. better take out a loan
	      can-jaunt-state?
		(from-stack.tail, from-depth - 1,
		 to-stack.tail, to-depth - 1,
		 funds, loan + from-tag.close-tag.size + to-tag.open-tag.size);
	    else
	      // oops .. but we're OK if we can cash in our loan
	      can-jaunt-state?
		(from-stack.tail, from-depth - 1,
		 to-stack.tail, to-depth - 1,
		 funds - loan -
		   from-tag.close-tag.size - to-tag.open-tag.size,
		 0);
	    end;
	  end;
	elseif (from-depth > to-depth)
	  let from-tag :: <tag> = from-stack.head;
	  can-jaunt-state?
	    (from-stack.tail, from-depth - 1,
	     to-stack, to-depth,
	     funds - from-tag.close-tag.size,
	     0)
	else // must be <	  
	  let to-tag :: <tag> = to-stack.head;
	  can-jaunt-state?
	    (from-stack, from-depth,
	     to-stack.tail, to-depth - 1,
	     funds - to-tag.close-tag.size,
	     0)
	end;
      end method can-jaunt-state?;

    for (elt1 :: <opt-state> keyed-by i in next-states)
      block (next-elt1)
	for (j from i + 1 below next-states.size)
	  let elt2 :: <opt-state> = next-states[j];
	  let diff = elt1.output-size - elt2.output-size;
	  if (diff > 0)
	    if (can-jaunt-state?
		  (elt2.tag-stack, elt2.stack-depth,
		   elt1.tag-stack, elt1.stack-depth,
		   diff, 0))
	      debug("Can jaunt from %d to %d, deleting the latter\n", j, i);
	      next-states[i] := #f;
	      next-elt1();
	    end;
	  end;
	end for;
      end block;
    end for;
    compress!(next-states);

    let limit = 100;
    if (next-states.size > limit)
      debug("\n\nThrowing away all but %d items\n\n", limit);
      for (i from 0 below next-states.size - limit)
	next-states[i] := #f;
      end;
      compress!(next-states);
    end;

    states := next-states;
    //let pct = truncate/(i * 100, num-steps);
    //if (pct ~== last-pct)
    debug("\n%%%% done %d of %d %%%%\n", i, num-steps);
    //last-pct := pct;
    //end;
  end;

  debug("\n\nFinal states\n------------------------------\n");
  debug("#final states = %d\n", states.size);
  let best-state :: false-or(<opt-state>) = #f;
  for (state :: <opt-state> keyed-by i in states)
    dump-state(i, state);
    for (e :: <tag> in state.tag-stack)
      let s = e.close-tag;
      state.transitions := pair(s, state.transitions);
      state.output-size := state.output-size + s.size;
    end;

    if (~best-state | state.output-size < best-state.output-size)
      debug("  ^-- new best\n");
      best-state := state;
    end;
  end for;

  if (best-state)
    best-state.transitions.reverse!
  else
    #()
  end;

end function optimize-output;


define function count-tags-to-clear(from :: <attribute>, to :: <attribute>)
 => (tags :: <integer>, bytes :: <integer>);
  let tags = 0;
  let bytes = 0;
  if (from.bold & ~to.bold) tags := tags + 1; bytes := bytes + 3 end;
  if (from.italic & ~to.italic) tags := tags + 1; bytes := bytes + 3 end;
  if (from.strong & ~to.strong) tags := tags + 1; bytes := bytes + 3 end;
  if (from.typewriter & ~to.typewriter) tags := tags + 1; bytes := bytes + 4 end;
  if (from.underline > to.underline)
    let diff = from.underline - to.underline;
    tags := tags + diff;
    bytes := bytes + 3 * diff;
  end;
  values (tags, bytes);
end function count-tags-to-clear;


define function emit-transitions
    (from :: <attribute>,
     to :: <attribute>,
     text :: <byte-string>,
     tag-stack :: <list>,
     attr-stack :: <list>,
     stack-depth :: <integer>,
     stack-cost :: <integer>,
     token-list :: <list>,
     len :: <integer>,
     stage :: <integer>,
     pop-depth :: <integer>,
     dont-push-tag :: false-or(<tag>),
     next-states :: <stretchy-object-vector>)
 => ();

  local
    method report(s :: <byte-string>)
      if(#f)
	debug("emit transition ");
	describe-attributes(from);
	debug(" -> ");
	describe-attributes(to);
	debug(": %s\n", s);
      end;
    end report,

    method pop-tag() => ();
      let tag :: <tag> = tag-stack.head;
      let tag-text = tag.close-tag;
      report(tag-text);
      emit-transitions
	(attr-stack.head, to, text,
	 tag-stack.tail,
	 attr-stack.tail,
	 stack-depth - 1,
	 stack-cost - tag-text.size,
	 pair(tag-text, token-list),
	 len + tag-text.size,
	 0, pop-depth - 1, tag, next-states);
    end pop-tag,

    method push-tag(tag :: <tag>)
      if (tag ~== dont-push-tag)
	let tag-text = tag.open-tag;
	let new-attr = apply-op(from, tag);
	report(tag-text);
	emit-transitions
	  (new-attr, to, text,
	   pair(tag, tag-stack),
	   pair(from, attr-stack),
	   stack-depth + 1,
	   stack-cost + tag.close-tag.size,
	   pair(tag-text, token-list),
	   len + tag-text.size,
	   1, pop-depth, #f, next-states);
      end;
    end method push-tag;

//  debug("In emit transition with ");
//  describe-attributes(from);
//  debug(" and  ");
//  describe-attributes(to);
//  debug("\n");

  block (exit)
    if (from.value = to.value)
      // same attributes .. finally output the new state
      let new-state =
	make(<opt-state>,
	     tags: tag-stack,
	     attrs: attr-stack,
	     text: pair(text, token-list),
	     size: len + text.size,
	     attr: to,
	     stack-depth: stack-depth,
	     stack-cost: stack-cost);
      add!(next-states, new-state);
      exit();
    end;

    if ((from.font-size & ~to.font-size) |
	  (from.color & ~to.color))
      // gotta pop all the way to the bottom
      pop-tag();
      exit();
    end;

    let (tags-to-clear, bytes-to-clear) = count-tags-to-clear(from, to);
    if (tags-to-clear > 0)
      // 2 seems a good compromise
      //let then-rebuild = count-tags-to-clear(to, make(<attribute>));
      if (tags-to-clear > 2)
	push-tag(tag-PL);
      end;

      pop-tag();
      exit();
    end; // if must clear

    if (stage == 0 & tag-stack ~== #() & pop-depth > 0)
      pop-tag();
    end;

    // don't try pushing EM if we could pop it instead
    if (from.emphasis ~== to.emphasis &
	  (attr-stack == #() |
	     begin
	       let attr :: <attribute> = attr-stack.head;
	       attr.emphasis ~== to.emphasis;
	     end))
      push-tag(tag-EM);
    end;

    // don't try pushing font size if we could pop it instead
    if (to.font-size &
	  from.font-size ~== to.font-size &
	  (attr-stack == #() |
	     begin
	       let attr :: <attribute> = attr-stack.head;
	       attr.font-size ~== to.font-size;
	     end))
      let tag = 
	select(to.font-size)
	  0 => tag-0;
	  1 => tag-1;
	  2 => tag-2;
	  3 => tag-3;
	  4 => tag-4;
	  5 => tag-5;
	  6 => tag-6;
	  7 => tag-7;
	  8 => tag-8;
	  9 => tag-9;
	end;
      push-tag(tag);
    end;

    // don't try pushing color if we could pop it instead
    if (to.color &
	  from.color ~== to.color &
	  (attr-stack == #() |
	     begin
	       let attr :: <attribute> = attr-stack.head;
	       attr.color ~== to.color;
	     end))
      let tag = 
	select(to.color)
	  #"red"     => tag-r;
	  #"green"   => tag-g;
	  #"blue"    => tag-b;
	  #"cyan"    => tag-c;
	  #"magenta" => tag-m;
	  #"yellow"  => tag-y;
	  #"black"   => tag-k;
	  #"white"   => tag-w;
	end;
      push-tag(tag);
    end;
    
    if (~from.bold & to.bold)
      push-tag(tag-BB);
    end;
    
    if (~from.italic & to.italic)
      push-tag(tag-I);
    end;

    if (~from.strong & to.strong)
      push-tag(tag-S);
    end;

    if (~from.typewriter & to.typewriter)
      push-tag(tag-TT);
    end;

    if (from.underline < to.underline)
      push-tag(tag-U);
    end;

  end block;

end function emit-transitions;