module: icfp2001
synopsis: Dylan Hackers entry in the Fourth Annual (2001) ICFP Programming Contest
authors: Andreas Bogk, Chris Double, Bruce Hoult
copyright: this program may be freely used by anyone, for any purpose

define class <generator-state> (<object>)
  slot open-tag-stack      :: <list> = #();
  slot attribute-stack     :: <list> = #();
  slot output-tokens       :: <list> = #();
  slot remaining-text-runs :: <subsequence>;
  slot maximum-cost        :: <integer> = 0;
end class <generator-state>;

define sealed domain make(singleton(<generator-state>));

define sealed method initialize
    (obj :: <generator-state>, #key clone, text-runs, #all-keys)
 => ();
  if(clone)
    obj.open-tag-stack      := clone.open-tag-stack;
    obj.attribute-stack     := clone.attribute-stack;
    obj.output-tokens       := clone.output-tokens;
    obj.remaining-text-runs := clone.remaining-text-runs;
    obj.maximum-cost        := clone.maximum-cost;
  else
    obj.attribute-stack     := list(make(<attribute>));
    obj.remaining-text-runs := subsequence(text-runs);
    obj.maximum-cost        := reduce(method(x, y)
                                          x + y.attributes.maximum-cost
                                      end,
                                      0,
                                      text-runs);
  end;
end method;

define method pop-tag(old-state :: <generator-state>)
 => new-state :: <generator-state>;
  let state = make(<generator-state>, clone: old-state);
  state := pop-tag!(state);
  state;
end method pop-tag;

define method pop-tag!(state :: <generator-state>)
 => new-state :: <generator-state>;
  state.output-tokens :=
    pair(state.open-tag-stack.head.close-tag, state.output-tokens);
  state.open-tag-stack := state.open-tag-stack.tail;
  state.attribute-stack := state.attribute-stack.tail;
  state;
end method pop-tag!;

define method push-tag(old-state :: <generator-state>, tag :: <tag>)
 => new-state :: <generator-state>;
  let state = make(<generator-state>, clone: old-state);
  state := push-tag!(state, tag);
  state;
end method push-tag;

define method push-tag!(state :: <generator-state>, tag :: <tag>)
 => new-state :: <generator-state>;
  state.output-tokens := pair(tag.open-tag, state.output-tokens);
  state.open-tag-stack := pair(tag, state.open-tag-stack);
  state.attribute-stack :=
    pair(apply-op(state.attribute-stack.head, tag), state.attribute-stack);
  state.maximum-cost := state.maximum-cost + tag.cost;
  state;
end method push-tag!;

define method emit-text(old-state :: <generator-state>)
 => new-state :: <generator-state>;
  let state = make(<generator-state>, clone: old-state);
  state := emit-text!(state);
  state;
end method emit-text;

define method emit-text!(state :: <generator-state>)
 => new-state :: <generator-state>;
  let output = state.remaining-text-runs[0]; 
  state.output-tokens :=
    pair(output.string, state.output-tokens);
  state.maximum-cost := state.maximum-cost - output.attributes.maximum-cost;
  state.remaining-text-runs :=
    subsequence(state.remaining-text-runs, start: 1);
  state;
end method emit-text!;

