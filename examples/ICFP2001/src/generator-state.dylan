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
  slot output-state        :: one-of(#"closing", #"opening", #"emit-pl") = #"opening";
end class <generator-state>;

define sealed domain make(singleton(<generator-state>));
define sealed domain initialize(<generator-state>);

define sealed method initialize
    (obj :: <generator-state>, #key clone, text-runs, #all-keys)
 => ();
  if(clone)
    obj.open-tag-stack      := clone.open-tag-stack;
    obj.attribute-stack     := clone.attribute-stack;
    obj.output-tokens       := clone.output-tokens;
    obj.remaining-text-runs := clone.remaining-text-runs;
    obj.maximum-cost        := clone.maximum-cost;
    obj.output-state        := clone.output-state; 
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

define method print-state(state :: <generator-state>)
 => ();
  debug("%= %=\n", state.output-tokens, state.maximum-cost);
//  debug("%=\n", state.remaining-text-runs);
//  debug("%=\n", state.attribute-stack);
//  debug("%=\n", state.open-tag-stack);
//  debug("%=\n", state.maximum-cost);
//  debug("%=\n", state.output-state);
//  describe-attributes(state.from);
//  describe-attributes(state.to);
//  debug("\n");
end method print-state;

define inline method from(state :: <generator-state>) 
 => att :: <attribute>;
  state.attribute-stack.first;
end method from;

define inline method to(state :: <generator-state>)
 => att :: <attribute>;
  if(state.remaining-text-runs.size > 0)
    state.remaining-text-runs[0].attributes;
  else
    make(<attribute>);
  end if;
end method to;

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
  state.output-state := #"opening";
  state;
end method push-tag!;

define method push-empty-tag(old-state :: <generator-state>)
 => new-state :: <generator-state>;
  let state = make(<generator-state>, clone: old-state);
  state := push-empty-tag!(state);
  state;
end method push-empty-tag;

define method push-empty-tag!(state :: <generator-state>)
 => new-state :: <generator-state>;
  state.output-state := #"opening";
  state;
end method push-empty-tag!;

define method pop-done(old-state :: <generator-state>)
 => new-state :: <generator-state>;
  let state = make(<generator-state>, clone: old-state);
  state := pop-done!(state);
  state;
end method pop-done;

define method pop-done!(state :: <generator-state>)
 => new-state :: <generator-state>;
  state.output-state := #"emit-pl";
  state;
end method pop-done!;

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
  state.output-state := #"closing";
  state;
end method emit-text!;

// works almost like the case macro, but evaluates all tests,
// and collects the expressions where test is true in a list.
define macro collect-case
  { collect-case ?:case-body end } => 
    {  begin let result :: <list> = #(); ?case-body end}
    case-body:
    { ?x:expression => ?:body ; ... } => { if (?x) result := pair(?body, result); end;  ... }
    { } => { result };
end;


// returns list of open tags needed for reaching the desired state
define method applicable-tags(state :: <generator-state>)
 => possible-tags :: <list>;
  let from :: <attribute> = state.from;
  let to   :: <attribute> = state.to;
  let tags =
    collect-case
      ~from.bold & to.bold              => tag-BB;
      ~from.emphasis & to.emphasis      => tag-EM;
//      from.emphasis & ~to.emphasis      => tag-EM;
      ~from.italic & to.italic          => tag-I;
      ~from.strong & to.strong          => tag-S;
      ~from.typewriter & to.typewriter  => tag-TT;
      from.underline < to.underline     => tag-U;
      from.underline < to.underline - 1 => tag-U;
      from.underline < to.underline - 2 => tag-U;
      to.font-size & from.font-size ~== to.font-size
        =>
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
      to.color & from.color ~== to.color
        =>
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
    end collect-case;
  tags;
end method;