module: icfp2001
synopsis: Dylan Hackers entry in the Fourth Annual (2001) ICFP Programming Contest
authors: Andreas Bogk, Chris Double, Bruce Hoult
copyright: this program may be freely used by anyone, for any purpose

define class <generator-state> (<object>)
  slot open-tag-stack      :: <list> = #();
  slot attribute-stack     :: <list> = #();
  slot output-tokens       :: <list> = #();
  slot remaining-text-runs :: <subsequence>;
end class <generator-state>;

define sealed method initialize
    (obj :: <generator-state>, #key clone, text-runs, #all-keys)
 => ();
  if(clone)
    obj.open-tag-stack      := clone.open-tag-stack;
    obj.attribute-stack     := clone.attribute-stack;
    obj.output-tokens       := clone.output-tokens;
    obj.remaining-text-runs := clone.remaining-text-runs;
  else
    obj.attribute-stack     := list(make(<attribute>));
    obj.remaining-text-runs := subsequence(text-runs);
  end;
end method;
    
define method generate-output(input :: <stretchy-object-vector>)
 => strings :: <list>;
  let state = make(<generator-state>, text-runs: input);
  force-output(*standard-error*);
  local
    method pop-tag()
      state.output-tokens :=
	pair(state.open-tag-stack.head.close-tag, state.output-tokens);
      state.open-tag-stack := state.open-tag-stack.tail;
      state.attribute-stack := state.attribute-stack.tail;
    end,
  
    method push-tag(tag)
      state.output-tokens := pair(tag.open-tag, state.output-tokens);
      state.open-tag-stack := pair(tag, state.open-tag-stack);
      state.attribute-stack :=
	pair(apply-op(state.attribute-stack.head, tag), state.attribute-stack);
    end;


  while(state.remaining-text-runs.size > 0)
    check-timeout();
//    debug("%=\n", state.output-tokens);
//    debug("%=\n", state.remaining-text-runs);
//    debug("%=\n", state.attribute-stack);
//    debug("%=\n", state.open-tag-stack);
//    force-output(*standard-error*);
    let from = state.attribute-stack.first;
    let to   = state.remaining-text-runs[0].attributes;
//    describe-attributes(from, *standard-error*);
//    describe-attributes(to, *standard-error*);
//    debug("\n");
//    force-output(*standard-error*);

    if(from.value = to.value)
      state.output-tokens :=
	pair(state.remaining-text-runs[0].string, state.output-tokens);
      state.remaining-text-runs :=
	subsequence(state.remaining-text-runs, start: 1);
    elseif(state.attribute-stack ~= #() 
             & state.attribute-stack.tail ~= #() 
             & to.value = state.attribute-stack.second.value)
      pop-tag();
    elseif(from.bold & ~to.bold)
      pop-tag();
    elseif(from.emphasis & ~to.emphasis)
      pop-tag();
    elseif(from.italic & ~to.italic)
      pop-tag();
    elseif(from.strong & ~to.strong)
      pop-tag();
    elseif(from.typewriter & ~to.typewriter)
      pop-tag();
    elseif(from.underline > to.underline)
      pop-tag();
    elseif(from.font-size & ~to.font-size)
      pop-tag();
    elseif(from.color & ~to.color)
      pop-tag();
    elseif(from.font-size ~= to.font-size &
             member?(to, state.attribute-stack,
		       test: method(x, y) x.font-size = y.font-size end))
      pop-tag();
    elseif(from.color ~= to.color &
             member?(to, state.attribute-stack,
		       test: method(x, y) x.color = y.color end))
      pop-tag();
    elseif(~from.bold & to.bold)
      push-tag(tag-BB);
    elseif(~from.emphasis & to.emphasis)
      push-tag(tag-EM);
    elseif(~from.italic & to.italic)
      push-tag(tag-I);
    elseif(~from.strong & to.strong)
      push-tag(tag-S);
    elseif(~from.typewriter & to.typewriter)
      push-tag(tag-TT);
    elseif(from.underline < to.underline)
      push-tag(tag-U);
    elseif(from.font-size ~= to.font-size)
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
    elseif(from.color ~= to.color)
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
    end if;
  end while;
  while(state.open-tag-stack.head ~= #())
    pop-tag();
  end while;
  reverse!(state.output-tokens);
end method generate-output;
