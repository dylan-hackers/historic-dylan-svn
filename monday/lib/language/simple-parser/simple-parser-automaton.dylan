Module: simple-parser-automaton


define macro simple-grammar-productions
  { simple-grammar-productions ?clauses end }
    => { vector(?clauses) }

clauses:
  { } => { }

  { production ?:name => [?symbols] (?variables:*)
      ?:body ... }
    => { 
make(<simple-production>,
     nonterminal: ?#"name",
     derives: simple-production-derives(?symbols),
     action: production-user-reduce-action-function([]; [?symbols];
                                                    [?variables]; ?body))
            , ... }
  { production ?:name :: ?type:expression => [?symbols] (?variables:*)
      ?:body ... }
    => { 
make(<simple-production>,
     nonterminal: ?#"name",
     nonterminal-type: ?type,
     derives: simple-production-derives(?symbols),
     action: production-user-reduce-action-function([result :: ?type];
                                                    [?symbols];
                                                    [?variables]; ?body))
            , ... }

  { production ?:name => [?symbols], #rest ?options:expression; ... }
    => { 
make(<simple-production>,
     nonterminal: ?#"name",
     derives: simple-production-derives(?symbols),
     ?options)
            , ... }
  { production ?:name :: ?type:expression => [?symbols],
      #rest ?options:expression; ... }
    => { 
make(<simple-production>,
     nonterminal: ?#"name",
     nonterminal-type: ?type,
     derives: simple-production-derives(?symbols),
     ?options)
            , ... }

  { make-production ?:name :: ?type:expression => [?symbols],
      #rest ?initializers:expression; ... }
    => { 
make(<simple-production>,
     nonterminal: ?#"name",
     nonterminal-type: ?type,
     derives: simple-production-derives(?symbols),
     action:
       if (subtype?(?type, <source-location-mixin>))
         production-user-reduce-action-function([result :: ?type]; [?symbols];
                                                [data, srcloc];
                                                make(?type,
                                                     source-location: srcloc,
                                                     ?initializers))
       else
         production-user-reduce-action-function([result :: ?type]; [?symbols];
                                                []; make(?type, ?initializers))
       end)
                                                           
            , ... }

symbols:
  { } => { }
  { ?symbol:name ... } => { ?symbol ... }
end macro;
              
define method simple-parser-automaton
    (lexical-definition :: <simple-lexical-definition>,
     productions :: <sequence>,
     start-symbols :: <sequence>,
     #key end-symbol :: <symbol> = #"EOF",
          class :: <symbol> = #"LALR-1")
 => (automaton :: <lr-parser-automaton>);
  let grammar = make(<grammar>, productions: productions);
  
let worklist = as(<deque>, productions);
let in-worklist-set = make(<object-set>);

let symbol-value-kind = make(<object-table>);
let nonterminal-dependent-productions = make(<object-table>);
            
let production-classification = make(<object-table>);
            
local
  
method requeue-production (production)
  unless (member?(production, in-worklist-set))
    push-last(worklist, production);
    add!(in-worklist-set, production);
  end unless;
end method,
            
method note-nonterminal-kind (production, nonterminal, kind)
  let previous-kind = element(symbol-value-kind, nonterminal, default: $unfound);
  if (previous-kind == #f)
    do(requeue-production,
       element(nonterminal-dependent-productions, nonterminal, default: #()));
    remove-key!(nonterminal-dependent-productions, nonterminal);
  end if;
  if (kind == #"sequence" & kind ~== previous-kind)
    do(method(other-production)
         unless(production == other-production)
           requeue-production(other-production)
         end unless;
       end,
       grammar-symbol-productions(grammar, nonterminal));
  end if;
  symbol-value-kind[nonterminal] := kind;
end method
            ;
until (empty?(worklist))
  let production :: <simple-production> = pop(worklist);
  remove!(in-worklist-set, production);
  let nonterminal = production.production-nonterminal;
  
if (production.production-nonterminal-type
    & element(symbol-value-kind, nonterminal, default: #f) == #f)
  note-nonterminal-kind(production, nonterminal, #"simple");
elseif (slot-initialized?(production, production-reduce-action)
        & unfound?(element(symbol-value-kind, nonterminal, default: $unfound)))
  symbol-value-kind[nonterminal] := #f;
end if;
            
  unless (slot-initialized?(production, production-reduce-action))
    
block (next)
  local
    method check-value (symbol)
      let kind = element(symbol-value-kind, symbol, default: $unfound);
      if (found?(kind))
        kind
      elseif (grammar-symbol-nonterminal?(grammar, symbol))
        requeue-production (production);
        next();
      elseif(lexical-token-type(lexical-definition, symbol))
        symbol-value-kind[symbol] := #"simple"
      else
        symbol-value-kind[symbol] := #f;
      end if
    end method;
  for (symbol in production.production-derives,
       count = 0 then if (symbol ~== nonterminal & check-value(symbol))
                        count + 1
                      else
                        count
                      end)
  finally
    
select (count by \=)
  0 =>
    
select (element(symbol-value-kind, nonterminal, default: $unfound))
  $unfound =>
    symbol-value-kind[nonterminal] := #f;
    production-classification[production] := #"inert";
  #f, #"simple" =>
    production-classification[production] := #"inert";
  #"sequence" =>
    production-classification[production] := #"sequence-empty";
end select;

for (symbol in production.production-derives)
  if (grammar-symbol-nonterminal?(grammar, symbol))
    nonterminal-dependent-productions[symbol]
      := add(element(nonterminal-dependent-productions, symbol, default: #()),
             production);
  end if;
end for;
            
  1 =>
    
if (production.production-derives[0] == nonterminal)
  note-nonterminal-kind(production, nonterminal, #"sequence");
  production-classification[production] := #"sequence-add";
else
  select (element(symbol-value-kind, nonterminal, default: #f))
    #f =>
      note-nonterminal-kind(production, nonterminal, #"simple");
      production-classification[production] := #"pass";
    #"simple" =>
      production-classification[production] := #"pass";
    #"sequence" =>
      production-classification[production] := #"sequence-start";
  end select;
end if;
            
  otherwise =>
    
unless (element(symbol-value-kind, nonterminal, default: #f))
  note-nonterminal-kind(production, nonterminal, #"simple");
end;
production-classification[production] := #"too-many";
            
end select;
            
  end for;
end block;
            
  end unless;
end until;
            
  
for (classification keyed-by production :: <simple-production>
       in production-classification)
  production.production-reduce-action
    := production-auto-reduce-action-function(production, classification,
                                              symbol-value-kind);
end for;
            
  make(<parser-automaton>,
       grammar: grammar,
       start-symbols: start-symbols,
       end-symbol: end-symbol,
       class: class);
end method;
              
define class <simple-production> (<production>)
  constant slot production-nonterminal-type :: false-or(<type>),
    init-value: #f, init-keyword: nonterminal-type:;
  slot production-reduce-action :: <function>,
    init-keyword: action:;
end class;
            
define macro simple-production-derives
  { simple-production-derives(?symbols) } => { #[?symbols] }
symbols:
  { } => { }
  { ?symbol:name ... } => { ?#"symbol", ... }
end macro;
            
