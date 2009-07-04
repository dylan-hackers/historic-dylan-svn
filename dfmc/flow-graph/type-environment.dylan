Module: dfmc-flow-graph
Author: Hannes Mehnert (hannes@mehnert.org)
Copyright:(c) 2009; All rights reversed. BSD License

define class <type-graph> (<object>)
  constant slot graph-nodes :: <stretchy-vector> = make(<stretchy-vector>); //actually, limited( , of: <node>)
  constant slot graph-edges :: <stretchy-vector> = make(<stretchy-vector>); //actually, limited( , of: <edge>)
  slot type-environment :: <type-environment>, init-keyword: type-environment:;
end;

define class <type-environment> (<mutable-explicit-key-collection>)
  constant slot real-environment :: <table> = make(<table>);
  constant slot outer-environment :: false-or(<type-environment>) = #f,
    init-keyword: outer:;
  constant slot type-graph :: <type-graph> = make(<type-graph>);
  constant slot %type-lambda :: false-or(<&lambda>) = #f,
    init-keyword: lambda:;
  constant slot type-constraints :: <stretchy-vector> =
    make(<stretchy-vector>);
  slot finished-initial-typing? :: <boolean> = #f;
end;

define method initialize (t :: <type-environment>, #key, #all-keys)
  next-method();
  t.type-graph.type-environment := t;
end;
    
define function type-lambda (t :: <type-environment>) => (res :: <&lambda>)
  t.%type-lambda | t.outer-environment.type-lambda
end;

define compiler-open generic deep-copy-node
 (node-or-type, graph :: <type-graph>)
 => (res /* :: <node> */);

define method element (table :: <type-environment>, key, #key default = #f) => (res)
  let result = element(table.real-environment, key, default: default);
  if (result == default & table.outer-environment)
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
