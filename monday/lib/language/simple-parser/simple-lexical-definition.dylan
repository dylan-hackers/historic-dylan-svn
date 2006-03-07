Module: simple-lexical-definition


define class <simple-lexical-definition> (<object>)
  constant slot lexical-clauses :: <sequence>,
    required-init-keyword: clauses:;
  slot lexical-automaton :: <regular-expression-dfa-state>;
  
  constant slot lexical-clauses-by-token = make(<object-table>);
  constant slot lexical-clauses-by-token-number = make(<stretchy-vector>);
            
end class;
              
define macro simple-lexical-definition
  { simple-lexical-definition ?clauses end }
    => { make(<simple-lexical-definition>, clauses: vector(?clauses)) }
clauses:
  { } => { }
  { ?clause; ... } => { ?clause, ... }

clause:
  { name ?:name = ?def:expression }
    => { 
make(<token-clause>,
     recognized?: #f, name: ?#"name", regular-expression: ?def)
             }
  { token ?:name }
    => { 
make(<token-clause>, name: ?#"name")
             }
  { token ?:name, #rest ?options:expression }
    => { 
make(<token-clause>, name: ?#"name", ?options)
             }
  { token ?:name :: ?type:expression }
    => { 
make(<token-clause>, name: ?#"name", type: ?type)
             }
  { token ?:name :: ?type:expression, #rest ?options:expression }
    => { 
make(<token-clause>, name: ?#"name", type: ?type, ?options)
             }
  { token ?:name = ?def:expression,
      #rest ?options:expression }
    => { 
make(<token-clause>,
     name: ?#"name", regular-expression: ?def, ?options)
             }
  { token ?:name :: ?type:expression = ?def:expression,
      #rest ?options:expression }
    => { 
make(<token-clause>,
     name: ?#"name", type: ?type, regular-expression: ?def, ?options)
             }
  { inert ?def:expression }
    => { 
make(<token-clause>, regular-expression: ?def)
             }
end macro;
              
define method lexical-token-number
    (definition :: <simple-lexical-definition>, token :: <symbol>)
 => (number :: <integer>);
  
definition.lexical-clauses-by-token[token].token-number;
            
end method;
              
define method lexical-token-type
    (definition :: <simple-lexical-definition>, token :: <symbol>)
 => (number :: false-or(<type>));
  
let clause = element(definition.lexical-clauses-by-token, token, default: #f);
if (clause)
  clause.token-type
else
  error("%s is not defined as a token", token);
end if

              
end method;
              
define class <token-clause> (<object>)
  constant slot token-recognized? :: <boolean>,
    init-value: #t, init-keyword: recognized?:;
  constant slot token-name :: false-or(<symbol>),
    init-value: #f, init-keyword: name:;
  constant slot token-type :: false-or(<type>),
    init-value: #f, init-keyword: type:;
  constant slot token-priority :: <integer>,
    init-value: 0, init-keyword: priority:;
  constant slot token-regular-expression :: false-or(<byte-string>),
    init-value: #f, init-keyword: regular-expression:;
  constant slot token-semantic-value-function :: false-or(<function>),
    init-value: #f, init-keyword: semantic-value-function:;
  
  slot token-number :: false-or(<integer>), init-value: #f;
            
end class;
            
define function parse-regular-expression
    (string :: <byte-string>,
     named-regular-expressions :: <object-table>)
 => (node :: <regular-expression>);
  local
    
method parse-regexp0
    (string :: <byte-string>, start :: <integer>)
 => (node :: <regular-expression>, pos :: <integer>);
  let (node :: <regular-expression>, pos :: <integer>)
    = parse-regexp1(string, start);
  while (pos < string.size & string[pos] == '|')
    let (new-node :: <regular-expression>, new-pos :: <integer>)
      = parse-regexp1(string, pos + 1);
    node := make(<union-regular-expression>, union1: node, union2: new-node);
    pos := new-pos;
  end while;
  values(node, pos);
end,
            
method parse-regexp1
    (string :: <byte-string>, start :: <integer>)
 => (node :: <regular-expression>, pos :: <integer>);
  let (node :: <regular-expression>, pos :: <integer>)
    = parse-regexp2(string, start);
  while (pos < string.size & string[pos] ~== '|' & string[pos] ~== ')')
    let (new-node :: <regular-expression>, new-pos :: <integer>)
      = parse-regexp2(string, pos);
    node := make(<concatenation-regular-expression>,
                 head: node, tail: new-node);
    pos := new-pos;
  end while;
  values(node, pos);
end,
            
method parse-regexp2
    (string :: <byte-string>, start :: <integer>)
 => (node :: <regular-expression>, pos :: <integer>);
  let (node :: <regular-expression>, pos :: <integer>)
    = parse-regexp3(string, start);
  if (pos < string.size)
    if (string[pos] == '*')
      let new-node = make(<closure-regular-expression>, of: node);
      values(new-node, pos + 1);
    elseif (string[pos] == '+')
      let new-node = make(<concatenation-regular-expression>,
                          head: node,
                          tail: make(<closure-regular-expression>,
                                     of: copy-regular-expression(node)));
      values(new-node, pos + 1);
    elseif (string[pos] == '?')
      let new-node = make(<union-regular-expression>,
                          union1: make(<epsilon-regular-expression>),
                          union2: node);
      values(new-node, pos + 1);
    else
      values(node, pos);
    end if;
  else
    values(node, pos);
  end if;
end,
            
method parse-regexp3
    (string :: <byte-string>, start :: <integer>)
 => (node :: <regular-expression>, pos :: <integer>);
  if (start >= string.size)
    error("regexp missing at end of '%s'", string);
  else
    if (string[start] == '(')
      
let (node :: <regular-expression>, pos :: <integer>)
    = parse-regexp0(string, start + 1);
if (pos >= string.size | string[pos] ~== ')')
  error("closing ')' missing in regular expression '%s'", string);
else
  values(node, pos + 1);
end if;
            
    elseif (string[start] == '\\' & start < string.size - 1)
      values(make(<symbol-regular-expression>,
                  symbol: as(<integer>, string[start + 1])),
             start + 2);
    elseif (string[start] == '.')
      
let dot-set = make(<bit-set>, upper-bound-hint: 256);
for (symbol :: <integer> from 0 below 256)
  if (symbol ~= as(<integer>, '\n'))
    set-add!(dot-set, symbol);
  end;
end for;
values(make(<symbol-set-regular-expression>, symbol-set: dot-set),
       start + 1);
            
    elseif (string[start] == '[')
      
let cclass-set = make(<bit-set>, upper-bound-hint: 256);
let pos = start + 1;
let complement? = if (pos < string.size & string[pos] = '^')
                    start := start + 1;
                    pos := pos + 1;
                    #t;
                  else #f end if;
while (pos < string.size & (string[pos] ~== ']' | pos = start + 1))
  
if (pos + 2 < string.size & string[pos + 1] == '-')
  for (symbol :: <integer> from as(<integer>, string[pos])
                                 to as(<integer>, string[pos + 2]))
    set-add!(cclass-set, symbol);
  end for;
  pos := pos + 3;
else
  set-add!(cclass-set, as(<integer>, string[pos]));
  pos := pos + 1;
end;
            
end while;
if (pos = string.size)
  error("closing ']' missing in regexp '%s'", string);
end if;
if (complement?)
  let complement-set = make(<bit-set>, upper-bound-hint: 256);
  for (symbol :: <integer> from 0 below 256)
    unless (member?(symbol, cclass-set)) add!(complement-set, symbol) end;
  end for;
  values(make(<symbol-set-regular-expression>, symbol-set: complement-set),
         pos + 1);
else
  values(make(<symbol-set-regular-expression>, symbol-set: cclass-set),
         pos + 1);
end;
            
    elseif (string[start] == '{')
      
let name-start = start + 1;
let name-end
  = for(i from name-start below string.size, until: string[i] == '}')
    finally i;
    end for;
let name
  = as(<symbol>, copy-sequence(string, start: name-start, end: name-end));
values(copy-regular-expression(named-regular-expressions[name]), name-end + 1);
            
    else
      values(make(<symbol-regular-expression>,
                  symbol: as(<integer>, string[start])),
             start + 1);
    end if;
  end if;
end
            ;
  let (node :: <regular-expression>, pos :: <integer>)
      = parse-regexp0(string, 0);
  if (pos < string.size)
    error("regular expression \"%s\" ended prematurely at position %d",
          string, pos)
  end if;
  node;
end function;
              
define sealed method initialize
    (definition :: <simple-lexical-definition>, #key)
 => ();
  next-method();
  let named-regular-expressions = make(<object-table>);
  let recognizer-regular-expression = make(<epsilon-regular-expression>);
  for (clause :: <token-clause> in definition.lexical-clauses)
    if (clause.token-name)
      definition.lexical-clauses-by-token[clause.token-name] := clause;
      if (clause.token-recognized?)
        clause.token-number := definition.lexical-clauses-by-token-number.size;
        add!(definition.lexical-clauses-by-token-number, clause);
      end if;
    end if;
    if (clause.token-regular-expression)
      let regex
        = parse-regular-expression(clause.token-regular-expression,
                                   named-regular-expressions);
      if (clause.token-name)
        named-regular-expressions[clause.token-name] := regex;
      end if;
      if (clause.token-recognized?)
        
let accept-function
  = token-accept-function(clause.token-number,
                          clause.token-name,
                          clause.token-semantic-value-function);
let clause-regular-expression
  = make(<concatenation-regular-expression>,
         head: regex,
         tail: make(<simple-accept-regular-expression>,
                    clause: clause,
                    accept-function: accept-function));
recognizer-regular-expression
  := make(<union-regular-expression>,
          union1: recognizer-regular-expression,
          union2: clause-regular-expression);
            
      end if;
    end if;
  end for;
  
definition.lexical-automaton
  := regular-expression-dfa(recognizer-regular-expression,
                            transition-collection-class: 
                              <simple-object-vector>,
                            transition-collection-size: 256,
                            state-class: <simple-lexical-state>);
            
end method;
            
define class <simple-accept-regular-expression>
    (<accept-regular-expression>)
  constant slot simple-accept-regular-expression-clause :: <token-clause>,
    required-init-keyword: clause:;
  constant slot simple-accept-regular-expression-accept-function :: <function>,
    required-init-keyword: accept-function:;
end class;
            
define class <simple-lexical-state> (<regular-expression-dfa-state>)
  slot lexical-state-accept-clause :: false-or(<token-clause>),
    init-value: #f;
  slot lexical-state-accept-function :: false-or(<function>),
    init-value: #f;
end class;  
            
define sealed method do-regular-expression-dfa-state-position
    (state :: <regular-expression-dfa-state>,
     position :: <simple-accept-regular-expression>,
     #key deterministic? = #f)
 => ();
  if (~state.lexical-state-accept-clause
        | state.lexical-state-accept-clause.token-priority
            < position.simple-accept-regular-expression-clause.token-priority)
    state.lexical-state-accept-clause
      := position.simple-accept-regular-expression-clause;
    state.lexical-state-accept-function
      := position.simple-accept-regular-expression-accept-function;    
  elseif(state.lexical-state-accept-clause.token-priority
           = position.simple-accept-regular-expression-clause.token-priority)
    error("ambiguous token rules: '%s' vs. '%s'",
          state.lexical-state-accept-clause.token-regular-expression,
          position.simple-accept-regular-expression-clause
            .token-regular-expression);
          
  end if;
end method;
            
