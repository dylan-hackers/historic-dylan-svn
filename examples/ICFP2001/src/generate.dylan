module: icfp2001

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
          add!(state.current-output, state.remaining-output[0].string);
          state.remaining-output := subsequence(state.remaining-output, start: 1);
        end;

  while(state.remaining-output.size > 0)
    debug("%=\n", state.current-output);
    debug("%=\n", state.remaining-output);
    let from = state.attribute-stack.first;
    let to   = state.remaining-output[0].attributes;
    if(from.value = to.value)
      add!(state.current-output, state.remaining-output[0].string);
      state.remaining-output := subsequence(state.remaining-output, start: 1);
    elseif(state.attribute-stack.size > 1 & to.value = state.attribute-stack.second.value)
      pop-tag();
    else
      if(~from.bold & to.bold)
        add!(state.current-output, tag-BB.open-tag);
        push(state.open-tags, tag-BB);
        push(state.attribute-stack, from.set-bold);
      end if;
      if(~from.emphasis & to.emphasis)
        add!(state.current-output, tag-EM.open-tag);
        push(state.open-tags, tag-EM);
        push(state.attribute-stack, from.set-emphasis);
      end if;
      if(~from.italic & to.italic)
        add!(state.current-output, tag-I.open-tag);
        push(state.open-tags, tag-I);
        push(state.attribute-stack, from.set-italic);
      end if;
      if(~from.strong & to.strong)
        add!(state.current-output, tag-S.open-tag);
        push(state.open-tags, tag-S);
        push(state.attribute-stack, from.set-strong);
      end if;
      if(~from.typewriter & to.typewriter)
        add!(state.current-output, tag-TT.open-tag);
        push(state.open-tags, tag-TT);
        push(state.attribute-stack, from.set-typewriter);
      end if;
      if(from.underline < to.underline)
        add!(state.current-output, tag-U.open-tag);
        push(state.open-tags, tag-U);
        push(state.attribute-stack, from.set-underline);
      end if;
    end if;
  end while;
end method generate-output;

