module: icfp2001
synopsis: Dylan Hackers entry in the Fourth Annual (2001) ICFP Programming Contest
authors: Andreas Bogk, Chris Double, Bruce Hoult
copyright: this program may be freely used by anyone, for any purpose

define class <opt-state> (<object>)
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


define method dump-state(s :: <opt-state>, f :: <stream>) => ();
  format(f, "State: ");
  force-output(f);
  describe-attributes(s.attr, f);
  format(f, "     ");
  for (e :: <tag> in s.tag-stack)
    format(f, "%s", e.close-tag);
  end;
  format(f, "\n");
  format(f, "%d: ", s.output-size);
  for (s :: <byte-string> in s.transitions.reverse)
    format(f, "%s", s);
  end;
  format(f, "\n");
  force-output(f);
end method dump-state;


define function optimize-output(input :: <stretchy-object-vector>)
 => strings :: <list>;

  force-output(*standard-error*);

  let states = make(<stretchy-vector>);
  add!(states, make(<opt-state>));

  let num-steps = input.size;
  let last-pct = 0;
  for (fragment :: <attributed-string> keyed-by i in input)
    force-output(*standard-error*);
    format(*standard-error*, "\n--------------------------------\n");
    dump-attributed-string(fragment);
    format(*standard-error*, "--------------------------------\n");

    let desired = fragment.attributes;
    let text = fragment.string;
    let next-states = make(<stretchy-vector>);
    for (state :: <opt-state> in states)
      check-timeout();
      emit-transitions(state.attr, desired, text,
		       state.tag-stack, state.attr-stack,
		       state.transitions, state.output-size,
		       0, next-states);
      let pct = truncate/(i * 100, num-steps);
      if (pct ~== last-pct)
	format(*standard-error*, "\n%%%% %d%% done %%%%\n", pct);
	force-output(*standard-error*);
	last-pct := pct;
      end;
    end;
    states := next-states;
    force-output(*standard-error*);
  end;

  format(*standard-error*, "\n\nFinal states\n------------------------------\n");
  let best-state :: false-or(<opt-state>) = #f;
  for (state :: <opt-state> in states)
    for (e :: <tag> in state.tag-stack)
      let s = e.close-tag;
      state.transitions := pair(s, state.transitions);
      state.output-size := state.output-size + s.size;
    end;
    dump-state(state, *standard-error*);
    force-output(*standard-error*);

    if (~best-state | state.output-size < best-state.output-size)
      format(*standard-error*, "  ^-- new best\n");
      best-state := state;
    end;
  end for;

  force-output(*standard-error*);

  if (best-state)
    best-state.transitions.reverse!
  else
    #()
  end;

end function optimize-output;


define function emit-transitions
    (from :: <attribute>,
     to :: <attribute>,
     text :: <byte-string>,
     tag-stack :: <list>,
     attr-stack :: <list>,
     token-list :: <list>,
     len :: <integer>,
     stage :: <integer>,
     next-states :: <stretchy-object-vector>)
 => ();

  force-output(*standard-error*);

  local
    method report(s :: <byte-string>)
      let f = *standard-error*;
      format(f, "emit transition ");
      describe-attributes(from, f);
      format(f, " -> ");
      describe-attributes(to, f);
      format(f, ": %s\n", s);
      force-output(f);
    end report,

    method pop-tag() => ();
      let f = *standard-error*;
      //format(f, "entering pop-tag\n");
      //force-output(f);
      let tag :: <tag> = tag-stack.head;
      let tag-text = tag.close-tag;
      report(tag-text);
      emit-transitions
	(attr-stack.head, to, text,
	 tag-stack.tail, attr-stack.tail,
	 pair(tag-text, token-list),
	 len + tag-text.size,
	 0, next-states);
    end pop-tag,

    method push-tag(tag :: <tag>)
      let tag-text = tag.open-tag;
      let new-attr = apply-op(from, tag);
      report(tag-text);
      emit-transitions
	(new-attr, to, text,
	 pair(tag, tag-stack), pair(from, attr-stack),
	 pair(tag-text, token-list),
	 len + tag-text.size,
	 1, next-states);
    end method push-tag;

//  let f = *standard-error*;
//  format(f, "In emit transition with ");
//  describe-attributes(from, f);
//  format(f, " and  ");
//  describe-attributes(to, f);
//  format(f, "\n");

  block (exit)
    if (from.value = to.value)
      // same attributes .. finally output the new state
      let new-state =
	make(<opt-state>,
	     tags: tag-stack,
	     attrs: attr-stack,
	     text: pair(text, token-list),
	     size: len + text.size,
	     attr: to);
      add!(next-states, new-state);
      exit();
    end;

    if (stage == 0)
      
/*
      local
	method can-pop-to-attr(name)
	  from.name ~== to.name &
	    member?(to, attr-stack,
		    test: method(x :: <attribute>, y :: <attribute>)
			      x.name == y.name;
			  end);
	end method can-pop-to-attr;
      
      let pop =
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

      if (pop)
*/
      if (attr-stack ~== #())
	pop-tag();
	push-tag(tag-PL);
//	exit();
      end;
    end;



    if (from.emphasis ~== to.emphasis & ~to.strong)
      push-tag(tag-EM);
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

    if (to.font-size & from.font-size ~== to.font-size)
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

    if (to.color & from.color ~== to.color)
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

  end block;

end function emit-transitions;