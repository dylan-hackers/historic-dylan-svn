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
    obj.current-output   := copy-sequence(clone.current-output);
    obj.remaining-output := clone.remaining-output;
  else
    obj.open-tags        := make(<deque>);
    obj.attribute-stack  := make(<deque>);
    push(obj.attribute-stack, make(<attribute>));
    obj.current-output   := make(<stretchy-vector>);
    obj.remaining-output := output;
  end;
end method;
    
define method generate-output(input)
  let state = make(<generator-state>, output: input);
  local method pop-tag()
          add!(state.current-output, state.open-tags.first.close-tag);
          pop(state.open-tags);
          pop(state.attribute-stack);
        end;

  while(state.remaining-output.size > 0)
//    debug("%=\n", state.current-output);
//    debug("%=\n", state.remaining-output);
    let from = state.attribute-stack.first;
    let to   = state.remaining-output[0].attributes;
//    describe-attributes(from, *standard-error*);
//    describe-attributes(to, *standard-error*);

    if(from.value = to.value)
      add!(state.current-output, state.remaining-output[0].string);
      state.remaining-output := subsequence(state.remaining-output, start: 1);
    elseif(state.attribute-stack.size > 1 & to.value = state.attribute-stack.second.value)
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
      add!(state.current-output, tag-BB.open-tag);
      push(state.open-tags, tag-BB);
      push(state.attribute-stack, from.set-bold);
    elseif(~from.emphasis & to.emphasis)
      add!(state.current-output, tag-EM.open-tag);
      push(state.open-tags, tag-EM);
      push(state.attribute-stack, from.set-emphasis);
    elseif(~from.italic & to.italic)
      add!(state.current-output, tag-I.open-tag);
      push(state.open-tags, tag-I);
      push(state.attribute-stack, from.set-italic);
    elseif(~from.strong & to.strong)
      add!(state.current-output, tag-S.open-tag);
      push(state.open-tags, tag-S);
      push(state.attribute-stack, from.set-strong);
    elseif(~from.typewriter & to.typewriter)
      add!(state.current-output, tag-TT.open-tag);
      push(state.open-tags, tag-TT);
      push(state.attribute-stack, from.set-typewriter);
    elseif(from.underline < to.underline)
      add!(state.current-output, tag-U.open-tag);
      push(state.open-tags, tag-U);
      push(state.attribute-stack, from.set-underline);
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
      add!(state.current-output, tag.open-tag);
      push(state.open-tags, tag);
      push(state.attribute-stack, set-font-size(from, to.font-size));
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
      add!(state.current-output, tag.open-tag);
      push(state.open-tags, tag);
      push(state.attribute-stack, set-color(from, to.color));
    end if;
  end while;
  while(state.attribute-stack.size > 1)
    pop-tag();
  end while;
  state.current-output;
end method generate-output;

