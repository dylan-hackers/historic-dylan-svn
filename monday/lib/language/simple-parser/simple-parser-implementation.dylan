Module: simple-parser-implementation


define class <simple-parser> (<sequence>)
  constant slot parser-automaton :: <lr-parser-automaton>,
    required-init-keyword: automaton:;
  constant slot parser-rangemap :: <source-location-rangemap>,
    required-init-keyword: rangemap:;
  constant slot parser-consumer-data :: <object>,
    required-init-keyword: consumer-data:;
  
  slot parser-stack-top :: <integer>, init-value: 0;
  slot parser-state-stack :: <simple-object-vector>
    = make(<simple-object-vector>, size: $initial-stack-size);
          
  slot parser-semantic-value-stack :: <simple-object-vector>
    = make(<simple-object-vector>, size: $initial-stack-size);
          
  slot parser-source-position-stack :: limited(<vector>, of: <integer>)
    = make(limited(<vector>, of: <integer>), size: $initial-stack-size);
  slot parser-end-position :: <integer>, init-value: 0;
          
  slot parser-rhs-size :: <integer>, init-value: 0;
            
end class;
              
define function simple-parser-reset
    (parser :: <simple-parser>, #key start-symbol)
 => ();
  
fill!(parser.parser-semantic-value-stack, #f,
      start: 0, end: parser.parser-stack-top + 1);
parser.parser-stack-top := 0;
parser.parser-state-stack[0]
  := lr-parser-automaton-initial-state(parser.parser-automaton, start-symbol);
            
end function;
              
define function simple-parser-consume-token
    (parser :: <simple-parser>,
     token-number, token-name, token-value,
     start-position :: <integer>, end-position :: <integer>)
 => (viable-prefix?);
  
local
  method dispatch (stack-top :: <integer>, state) => (viable-prefix?);
    let(action, data)
      = lr-parser-automaton-terminal-transition(parser.parser-automaton,
                                                state, token-name);
    select(action)
      
#"shift" =>
  parser-push-stacks(parser, stack-top + 1, data, token-value, start-position);
  parser.parser-end-position := end-position;
  
if (parser.parser-stack-top > stack-top + 1)
  fill!(parser.parser-semantic-value-stack, #f,
        start: stack-top + 2, end: parser.parser-stack-top + 1);
end if;
            
  parser.parser-stack-top := stack-top + 1;
  #t;
            
#"reduce" =>
  let rhs-size = data.production-derives.size;
  let reduce-stack-top = stack-top - rhs-size;
  let reduce-state = parser.parser-state-stack[reduce-stack-top];
  
  let new-state
    = lr-parser-automaton-nonterminal-transition
        (parser.parser-automaton, reduce-state, data.production-nonterminal);

  
parser.parser-rhs-size := rhs-size;
parser.parser-stack-top := reduce-stack-top + 1;
            

  if (rhs-size.zero?)
    let reduce-semantic-value
      = data.production-reduce-action(parser, parser.parser-consumer-data,
                                      start-position, start-position);
    parser-push-stacks(parser, reduce-stack-top + 1,
                       new-state, reduce-semantic-value, start-position);
  else
    let reduce-semantic-value
      = data.production-reduce-action(parser, parser.parser-consumer-data,
                                      parser.parser-source-position-stack[reduce-stack-top + 1],
                                      parser.parser-end-position);
    parser.parser-state-stack[reduce-stack-top + 1] := new-state;
    parser.parser-semantic-value-stack[reduce-stack-top + 1]
      := reduce-semantic-value;
  end if;

  dispatch(reduce-stack-top + 1, new-state);
            
#"error" =>
  let srcloc
    = simple-parser-source-location(parser, start-position, end-position);
  source-error(srcloc, "syntax error at %=, expected one of %=",
               token-name,
               lr-parser-automaton-transition-terminals(parser.parser-automaton,
                                                        state));
            
#"accept" =>
  #t;
            
    end select;
  end method;
let stack-top = parser.parser-stack-top;
dispatch(stack-top, parser.parser-state-stack[stack-top])
            
end function;
              
define function simple-parser-can-consume-token?
    (parser :: <simple-parser>, token-number, token-name)
 => (viable-prefix?);
  
local
  method dispatch
      (stack-top :: <integer>, stack :: false-or(<stretchy-object-vector>),
       state)
   => (viable-prefix?);
    let(action, data)
      = lr-parser-automaton-terminal-transition(parser.parser-automaton,
                                                state, token-name);
    select(action)
      #"shift" =>
        #t;
      #"reduce" =>
        
  let stack = stack
    | begin
        let stack = make(<stretchy-object-vector>, size: stack-top + 1);
        for (i from 0 to stack-top)
          stack[i] := parser.parser-state-stack[i];
        end;
        stack;
      end;
  let rhs-size = data.production-derives.size;
  let reduce-stack-top = stack-top - rhs-size;
  let reduce-state = stack[reduce-stack-top];
  
  let new-state
    = lr-parser-automaton-nonterminal-transition
        (parser.parser-automaton, reduce-state, data.production-nonterminal);

  stack[reduce-stack-top + 1] := new-state;
  dispatch(reduce-stack-top + 1, stack, new-state);
            
      #"error" =>
        #f;
      #"accept" =>
        #t;
    end select;
  end method;
let stack-top = parser.parser-stack-top;
dispatch(stack-top, #f, parser.parser-state-stack[stack-top])
            
end function;
              
define function simple-parser-source-location
    (parser :: <simple-parser>,
     start-position :: <integer>, end-position :: <integer>)
 => (source-location :: <file-source-location>);
  range-source-location(parser.parser-rangemap, start-position, end-position)
end function;
              
define constant $initial-stack-size = 200;
            
define sealed method initialize
    (parser :: <simple-parser>,
     #key automaton :: <lr-parser-automaton>, start-symbol,
     #all-keys)
 => ();
  next-method();
  parser.parser-state-stack[0]
    := lr-parser-automaton-initial-state(automaton, start-symbol);
end method;
          
define function parser-push-stacks
    (parser :: <simple-parser>, new-stack-top :: <integer>,
     new-state, token-value, start-position :: <integer>)
 => ();
  if (new-stack-top >= parser.parser-state-stack.size)
    
let new-size = truncate/(new-stack-top * 3, 2);
let old-state-stack = parser.parser-state-stack;
parser.parser-state-stack
  := make(<simple-object-vector>, size: new-size);
let old-semantic-value-stack = parser.parser-semantic-value-stack;
parser.parser-semantic-value-stack
  := make(<simple-object-vector>, size: new-size);
let old-source-position-stack = parser.parser-source-position-stack;
parser.parser-source-position-stack
  := make(limited(<vector>, of: <integer>), size: new-size);
for (i from 0 below new-stack-top)
  parser.parser-state-stack[i] := old-state-stack[i];
  parser.parser-semantic-value-stack[i] := old-semantic-value-stack[i];
  parser.parser-source-position-stack[i] := old-source-position-stack[i];
end for;
            
  end if;
  parser.parser-state-stack[new-stack-top] := new-state;
  parser.parser-semantic-value-stack[new-stack-top] := token-value;
  parser.parser-source-position-stack[new-stack-top] := start-position;
end function;
          
define sealed method size (parser :: <simple-parser>) => (size :: <integer>);
  parser.parser-rhs-size
end method;
            
define sealed method element
    (parser :: <simple-parser>, index :: <integer>, #key default = $unsupplied)
 => (value :: <object>);
  if (0 <= index & index < parser.parser-rhs-size)
    parser.parser-semantic-value-stack[parser.parser-stack-top + index]
  elseif (supplied?(default))
    default
  else
    error("index %d is out of range", index);
  end if
end method;
            
define sealed inline method type-for-copy
    (parser :: <simple-parser>)
 => (type :: <type>);
  <simple-object-vector>
end method;
            
define sealed inline method forward-iteration-protocol
    (parser :: <simple-parser>)
 => (initial-state :: <integer>, limit :: <integer>, next-state :: <function>,
     finished-state? :: <function>, current-key :: <function>,
     current-element :: <function>, current-element-setter :: <function>,
     copy-state :: <function>);
  values(// initial-state
         parser.parser-stack-top,
         // limit
         parser.parser-stack-top + parser.parser-rhs-size,
         // next-state
         method (parser :: <simple-parser>, state :: <integer>)
          => (new-state :: <integer>);
           state + 1
         end,
         // finished-state?
         method (parser :: <simple-parser>, state :: <integer>,
                 limit :: <integer>)
          => (finished? :: <boolean>);
           state >= limit
         end method,
         // current-key
         method (parser :: <simple-parser>, state :: <integer>)
          => (key :: <integer>)
           state
         end method,
         // current-element
         method (parser :: <simple-parser>, state :: <integer>)
           parser.parser-semantic-value-stack[state]
         end method,
         // current-element-setter,
         method (parser :: <simple-parser>, state :: <integer>, value)
           error("The <simple-parser> semantic stack may not be changed");
         end method,
         // copy-state
         method (parser :: <simple-parser>, state :: <integer>)
          => (state :: <integer>);
           state
         end method)
end method;
            
define macro production-user-reduce-action-function
  { production-user-reduce-action-function([?result:*]; [];
                                           []; ?:body) }
    => { method (p :: <simple-parser>, data, s, e) => (?result);
           ?body
         end }
  { production-user-reduce-action-function([?result:*]; [?symbols];
                                           []; ?:body) }
    => { method (p :: <simple-parser>, data, s, e) => (?result);
           let (?symbols) = apply(values, p);
           ?body
         end }

  { production-user-reduce-action-function([?result:*]; [];
                                           [?data:variable]; ?:body) }
    => { method (p :: <simple-parser>, ?data, s, e) => (?result);
           ?body
         end }
  { production-user-reduce-action-function([?result:*]; [?symbols];
                                           [?data:variable]; ?:body) }
    => { method (p :: <simple-parser>, ?data, s, e) => (?result);
           let (?symbols) = apply(values, p);
           ?body
         end }

  { production-user-reduce-action-function([?result:*]; [];
                                           [?data:variable, ?srcloc:variable];
                                           ?:body) }
    => { method
             (p :: <simple-parser>, ?data, s :: <integer>, e :: <integer>)
          => (?result);
           let ?srcloc = simple-parser-source-location(p, s, e);
           ?body
         end }
  { production-user-reduce-action-function([?result:*]; [?symbols];
                                           [?data:variable, ?srcloc:variable];
                                           ?:body) }
    => { method
             (p :: <simple-parser>, ?data, s :: <integer>, e :: <integer>)
          => (?result);
           let (?symbols) = apply(values, p);
           let ?srcloc = simple-parser-source-location(p, s, e);
           ?body
         end }

symbols:
  { } => { }
  { ?symbol:name ... } => { ?symbol, ... }
end macro;
            
define method production-auto-reduce-action-function
    (production :: <simple-production>, kind == #"inert",
     symbol-value-kind :: <object-table>)
 => (function :: <function>);
  method (parser, data, s, e)
    #f
  end
end method;
            
define method production-auto-reduce-action-function
    (production :: <simple-production>, kind == #"pass",
     symbol-value-kind :: <object-table>)
 => (function :: <function>);
  block (return)
    for (symbol in production.production-derives, index from 0)
      if (element(symbol-value-kind, symbol, default: #f))
        return(method(p :: <simple-parser>, data, s, e)
                 p[index]
               end);
      end if
    end for;
    error("Couldn't find a value symbol in %=", production);
  end block;
end method;
            
define method production-auto-reduce-action-function
    (production :: <simple-production>, kind == #"sequence-empty",
     symbol-value-kind :: <object-table>)
 => (function :: <function>);
  method (parser, data, s, e)
    make(<stretchy-object-vector>)
  end
end method;
            
define method production-auto-reduce-action-function
    (production :: <simple-production>, kind == #"sequence-start",
     symbol-value-kind :: <object-table>)
 => (function :: <function>);
  block (return)
    for (symbol in production.production-derives, index from 0)
      if (element(symbol-value-kind, symbol, default: #f))
        return(method(p :: <simple-parser>, data, s, e)
                 let sequence = make(<stretchy-object-vector>);
                 add!(sequence, p[index]);
                 sequence
               end);
      end if
    end for;
    error("Couldn't find a value symbol in %=", production);
  end block;
end method;
            
define method production-auto-reduce-action-function
    (production :: <simple-production>, kind == #"sequence-add",
     symbol-value-kind :: <object-table>)
 => (function :: <function>);
  block (return)
    for (symbol in production.production-derives, index from 0)
      if (index > 0 & element(symbol-value-kind, symbol, default: #f))
        return(method(p :: <simple-parser>, data, s, e)
                 add!(p[0], p[index]);
                 p[0]
               end);
      end if
    end for;
    error("Couldn't find a value symbol in %=", production);
  end block;
end method;
            
define method production-auto-reduce-action-function
    (production :: <simple-production>, kind == #"too-many",
     symbol-value-kind :: <object-table>)
 => (function :: <function>);
  signal("Production %= has too many right-hand side values", production);
  method (parser, data, s, e)
    #f
  end
end method;
            
