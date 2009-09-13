Module: dfmc-flow-graph
Author: Hannes Mehnert (hannes@mehnert.org)
Copyright:(c) 2009; All rights reversed. BSD License

define class <type-graph> (<object>)
  constant slot graph-nodes :: <stretchy-vector> = make(<stretchy-vector>); //actually, limited( , of: <node>)
  constant slot graph-edges :: <stretchy-vector> = make(<stretchy-vector>); //actually, limited( , of: <edge>)
  slot type-environment :: <type-environment>, init-keyword: type-environment:; //only used for debug printing context :[ (in typist/type-debug)
end;

define class <type-environment> (<mutable-explicit-key-collection>)
  constant slot real-environment :: <table> = make(<table>);
  slot %outer-environment :: false-or(<type-environment>) = #f,
    init-keyword: outer:;
  constant slot type-graph :: <type-graph> = make(<type-graph>);
  constant slot %type-lambda :: false-or(type-union(<symbol>, <&lambda>)) = #f,
    init-keyword: lambda:; //once again, only used for debugging in typist/type-debug
  slot finished-initial-typing? :: <boolean> = #f;
  constant slot computation-id :: <integer> = next-computation-id();
  constant slot inners :: <stretchy-vector> = make(<stretchy-vector>);
  //constant slot retype-queue :: <deque> = make(<deque>);
end;

define constant ret-que = make(<table>);

define function retype-queue (te :: <type-environment>) => (res :: <deque>)
  element(ret-que, te, default: #f) | (ret-que[te] := make(<deque>))
end;

define function inner-type-environments (te :: <type-environment>) => (res :: <stretchy-vector>)
  te.inners
end;

define method initialize (t :: <type-environment>, #key outer, #all-keys)
  next-method();
  t.type-graph.type-environment := t;
  if (*computation-tracer*)
    *computation-tracer*(#"new-te", t.type-lambda, t.computation-id, 0);
    *computation-tracer*(#"outer-te", t.type-lambda, t.computation-id, t.outer-environment & t.outer-environment.computation-id | 0);
  end;
  if (outer)
    add!(outer.inners, t)
  end;
end;

define method outer-environment (te :: <type-environment>) => (res :: false-or(<type-environment>))
  te.%outer-environment
end;

define method outer-environment-setter (te :: false-or(<type-environment>), t :: <type-environment>)
 => (res :: false-or(<type-environment>))
  if (t.%outer-environment)
    remove!(t.%outer-environment.inners, t)
  end;
  t.%outer-environment := te;
  if (te)
    add!(te.inners, t);
    if (*computation-tracer*)
      *computation-tracer*(#"outer-te", t.type-lambda, t.computation-id, te & te.computation-id | 0);
    end;
  end;
  te;
end;

//only used for debug context in typist/type-debug
define function type-lambda (t :: <type-environment>) => (res :: type-union(<symbol>, <&lambda>))
  t.%type-lambda | t.outer-environment.type-lambda
end;

define function in-type-environment? (te :: <type-environment>, o) => (res :: <boolean>)
  member?(o, te.real-environment.key-sequence) |
    (te.outer-environment & in-type-environment?(te.outer-environment, o))
end;

define compiler-open generic deep-copy-node
 (node-or-type, graph :: <type-graph>)
 => (res /* :: <node> */);

define compiler-open generic solve
 (te :: <type-environment>) => ();

define method element (table :: <type-environment>, key, #key default = #f) => (res)
  let result = element(table.real-environment, key, default: default);
  if (result == default & table.outer-environment & in-type-environment?(table.outer-environment, key))
    solve(table.outer-environment);
    let res = element(table.outer-environment, key, default: default);
    if (res ~= default)
      table.real-environment[key] := deep-copy-node(res, table.type-graph)
    else
      res
    end
  else
    result
  end
end;

define method element-setter (new, table :: <type-environment>, key) => (new)
  element-setter(new, table.real-environment, key)
end;

define method forward-iteration-protocol (t :: <type-environment>)
  => (initial-state :: <object>, limit :: <object>,
      next-state :: <function>, finished-state? :: <function>,
      current-key :: <function>,
      current-element :: <function>, current-element-setter :: <function>,
      copy-state :: <function>)
  let (initial-state, limit, next-state, finished-state?,
       current-key, current-element, current-element-setter, copy-state) 
    = forward-iteration-protocol(t.real-environment);
  values(initial-state,
         limit,
         method (table :: <type-environment>, state) 
           next-state(table.real-environment, state) 
         end method,
         method (table :: <type-environment>, state, limit) 
           finished-state?(table.real-environment, state, limit) 
         end method,
         method (table :: <type-environment>, state) 
           current-key(table.real-environment, state) 
         end method,
         method (table :: <type-environment>, state) 
           current-element(table.real-environment, state) 
         end method,
         method (new-value, table :: <type-environment>, state) 
           current-element-setter(new-value, table.real-environment, state) 
         end method,
         method (table :: <type-environment>, state) 
           copy-state(table.real-environment, state)
         end method)
end;

/*
//If I enable this, I get "Access violation", which can't be browsed...
//(compiler/IDE bug?!)
define method member? (value, coll :: <type-environment>, #key test)
 => (res :: <boolean>)
  member?(value, coll.real-environment, test: test) |
    (coll.outer-environment & member?(value, coll.outer-environment, test: test))
end;
*/
