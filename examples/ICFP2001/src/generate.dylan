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
    obj.open-tags        := #();
    obj.attribute-stack  := list(make(<attribute>));
    obj.current-output   := make(<stretchy-vector>);
    obj.remaining-output := output;
  end;
end method;
    
define method generate-output(input)
  let state = make(<generator-state>, output: input);

  while(state.remaining-output.size)
    let from = state.attribute-stack.first;
    let to   = state.remaining-output[0].attributes;
    if(from = to)
      add!(state.current-output, state.remaining-output[0].string);
      state.remaining-output := subsequence(state.remaining-output, start: 1);
    elseif(state.attribute-stack.size > 1 & to = state.attribute-stack.second)
      add!(state.current-output, state.open-tags.first.close-tag);
      pop(state.open-tags);
      pop(state.attribute-stack);
      add!(state.current-output, state.remaining-output[0].string);
      state.remaining-output := subsequence(state.remaining-output, start: 1);
    else
      if(~from.bold & to.bold)
        add!(state.current-output, tag-BB.open-tag);
        push(state.open-tags, tag-BB);
        push(state.attribute-stack, from.set-bold);
      end if;
      if(~from.emphasis & to.emphasis)
        add!(state.current-output, tag-EM.open-tag);
        push(state.open-tags, tag-EM);
        push(state.attribute-stack, from.set-bold);
      end if;
    end if;
  end while;
end method generate-output;

