module: icfp2001
synopsis: Dylan Hackers entry in the Fourth Annual (2001) ICFP Programming Contest
authors: Andreas Bogk, Chris Double, Bruce Hoult
copyright: this program may be freely used by anyone, for any purpose

define class <generator-state> (<object>)
  slot open-tags;
  slot attribute-stack;
  slot current-output;
  slot remaining-output;
end class <generator-state>;

define method initialize(obj :: <generator-state>, #key clone, output, #all-keys)
  if(clone)
    obj.open-tags        := clone.open-tags;
    obj.attribute-stack  := clone.attribute-stack;
    obj.current-output   := clone.current-output;
    obj.remaining-output := clone.remaining-output;
  else
    obj.open-tags        := #();
    obj.attribute-stack  := list(make(<attribute>));
    obj.current-output   := #();
    obj.remaining-output := output;
  end;
end method;
    
define method generate-output(input)
  let state = make(<generator-state>, output: input);
  force-output(*standard-error*);
  local method pop-tag()
          state.current-output := pair(state.open-tags.head.close-tag, state.current-output);
          state.open-tags := state.open-tags.tail;
          state.attribute-stack := state.attribute-stack.tail;
        end;
  local method push-tag(tag)
          state.current-output := pair(tag.open-tag, state.current-output);
          state.open-tags := pair(tag, state.open-tags);
          state.attribute-stack := pair(apply-op(state.attribute-stack.head, tag), 
                                        state.attribute-stack);
        end;


  while(state.remaining-output.size > 0)
    check-timeout();
//    debug("%=\n", state.current-output);
//    debug("%=\n", state.remaining-output);
//    debug("%=\n", state.attribute-stack);
//    debug("%=\n", state.open-tags);
//    force-output(*standard-error*);
    let from = state.attribute-stack.first;
    let to   = state.remaining-output[0].attributes;
//    describe-attributes(from, *standard-error*);
//    describe-attributes(to, *standard-error*);
//    debug("\n");
//    force-output(*standard-error*);

    if(from.value = to.value)
      state.current-output := pair(state.remaining-output[0].string, state.current-output);
      state.remaining-output := subsequence(state.remaining-output, start: 1);
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
             member?(to, state.attribute-stack, test: method(x, y) x.font-size = y.font-size end))
      pop-tag();
    elseif(from.color ~= to.color &
             member?(to, state.attribute-stack, test: method(x, y) x.color = y.color end))
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
  while(state.open-tags.head ~= #())
    pop-tag();
  end while;
  reverse(state.current-output);
end method generate-output;

