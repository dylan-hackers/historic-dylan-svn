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
    debug("%=\n", state.output-tokens);
    debug("%=\n", state.remaining-text-runs);
    debug("%=\n", state.attribute-stack);
    debug("%=\n", state.open-tag-stack);
    debug("%=\n", state.maximum-cost);
    force-output(*standard-error*);
    let from :: <attribute> = state.attribute-stack.first;
    let to   :: <attribute> = state.remaining-text-runs[0].attributes;
    describe-attributes(from, *standard-error*);
    describe-attributes(to, *standard-error*);
    debug("\n");
    force-output(*standard-error*);

    if(from.value = to.value)
      // same attributes .. just output the text
      emit-text!(state);
    elseif(state.attribute-stack ~== #() 
             & state.attribute-stack.tail ~== #() 
             & to.value = state.attribute-stack.second.value)
      // the attributes we want are *just* down one level on the stack
      pop-tag!(state);
    elseif(from.bold & ~to.bold)
      pop-tag!(state);
    elseif(from.emphasis & ~to.emphasis)
      pop-tag!(state);
    elseif(from.italic & ~to.italic)
      pop-tag!(state);
    elseif(from.strong & ~to.strong)
      pop-tag!(state);
    elseif(from.typewriter & ~to.typewriter)
      pop-tag!(state);
    elseif(from.underline > to.underline)
      pop-tag!(state);
    elseif(from.font-size & ~to.font-size)
      pop-tag!(state);
    elseif(from.color & ~to.color)
      pop-tag!(state);
    elseif(from.font-size ~== to.font-size &
             member?(to, state.attribute-stack,
		       test: method(x :: <attribute>, y :: <attribute>)
				 x.font-size == y.font-size;
			     end))
      pop-tag!(state);
    elseif(from.color ~== to.color &
             member?(to, state.attribute-stack,
		       test: method(x :: <attribute>, y :: <attribute>)
				 x.color == y.color;
			     end))
      pop-tag!(state);
    elseif(~from.bold & to.bold)
      push-tag!(state, tag-BB);
    elseif(~from.emphasis & to.emphasis)
      push-tag!(state, tag-EM);
    elseif(~from.italic & to.italic)
      push-tag!(state, tag-I);
    elseif(~from.strong & to.strong)
      push-tag!(state, tag-S);
    elseif(~from.typewriter & to.typewriter)
      push-tag!(state, tag-TT);
    elseif(from.underline < to.underline)
      push-tag!(state, tag-U);
    elseif(from.font-size ~== to.font-size)
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
      push-tag!(state, tag);
    elseif(from.color ~== to.color)
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
      push-tag!(state, tag);
    end if;
  end while;
  while(state.open-tag-stack.head ~== #())
    pop-tag!(state);
  end while;
  reverse!(state.output-tokens);
end method generate-output;
