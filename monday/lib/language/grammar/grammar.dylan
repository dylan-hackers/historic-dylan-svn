Module: grammar


define open abstract class <grammar> (<object>)
  // No additional slots
end class;
                  
define open generic grammar-symbol-productions
    (grammar :: <grammar>,
     nonterminal-symbol :: <object>)
 => (productions :: <sequence>);
                  
define open generic grammar-symbol-nonterminal?
    (grammar :: <grammar>,
     symbol :: <object>)
 => (nonterminal? :: <boolean>);
                  
define open generic grammar-symbol-nullable?
    (grammar :: <grammar>,
     symbol :: <object>)
 => (nullable? :: <boolean>);
                  
define open generic grammar-symbol-left-recursive?
    (grammar :: <grammar>,
     symbol :: <object>)
 => (left-recursive? :: <boolean>);
                  
define open generic grammar-symbol-first-symbols
    (grammar :: <grammar>,
     symbol :: <object>)
 => (first-symbols :: <sequence>);
                  
define open primary class <production> (<object>)
  constant slot production-nonterminal :: <object>,
    required-init-keyword: nonterminal:;
  constant slot production-derives :: <vector>,
    required-init-keyword: derives:;
end class;
                  
define class <concrete-grammar> (<grammar>)
  constant slot grammar-nonterminals :: <object-table>
    = make(<object-table>);
end class;
            
define sealed method make
    (class == <grammar>, #key productions :: <sequence>)
 => (object :: <concrete-grammar>);
  make(<concrete-grammar>, productions: productions);
end method; 
            
define class <nonterminal> (<object>)
  slot nonterminal-productions :: <list> = #();
  slot nonterminal-nullable? :: <boolean> = #f;
  slot nonterminal-left-recursive? :: <boolean> = #f;
  slot nonterminal-first-symbols :: <list>;
end class;
            
define sealed method initialize
    (instance :: <concrete-grammar>,
     #key productions :: <sequence>, #all-keys)
 => ();
  next-method();
  let nonterminals = instance.grammar-nonterminals;

  for (production in productions)
    let nonterminal
       = element(nonterminals, production.production-nonterminal, default: #f)
         | (nonterminals[production.production-nonterminal]
              := make(<nonterminal>));

    nonterminal.nonterminal-productions
      := add!(nonterminal.nonterminal-productions, production);

    if (empty?(production.production-derives))
      nonterminal.nonterminal-nullable? := #t;
    end if;
  end for;

  compute-nullable-nonterminals!(instance);
end method;
            
define method grammar-symbol-productions
    (grammar :: <concrete-grammar>,
     nonterminal-symbol :: <object>)
 => (productions :: <sequence>);
  let nonterminal = element(grammar.grammar-nonterminals,
                            nonterminal-symbol,
                            default: #f);
  if (nonterminal)
     nonterminal.nonterminal-productions
  else
     error("asked to find productions for terminal symbol %=",
           nonterminal-symbol);
  end if;
end method;
            
define method grammar-symbol-nonterminal?
    (grammar :: <concrete-grammar>, symbol :: <object>)
 => (nonterminal? :: <boolean>);
  if (element(grammar.grammar-nonterminals, symbol, default: #f))
     #t
  else
     #f
  end if;
end method;
            
define method grammar-symbol-nullable?
    (grammar :: <concrete-grammar>, symbol :: <object>)
 => (nullable? :: <boolean>);
  let nonterminal = element(grammar.grammar-nonterminals,
                            symbol,
                            default: #f);
  if (nonterminal)
     nonterminal.nonterminal-nullable?
  else
     #f
  end if;
end method;
            
define method grammar-symbol-first-symbols
    (grammar :: <concrete-grammar>, symbol :: <object>)
 => (first-symbols :: <sequence>);
  let nonterminal :: false-or(<nonterminal>)
    = element(grammar.grammar-nonterminals, symbol,
              default: #f);
  if (nonterminal)
    if (~slot-initialized?(nonterminal, nonterminal-first-symbols))
      compute-first-symbols!(grammar, nonterminal);
    end if;
    nonterminal.nonterminal-first-symbols;
  else
    list(symbol);
  end if;
end method;
            
define method grammar-symbol-left-recursive?
    (grammar :: <concrete-grammar>, symbol :: <object>)
 => (left-recursive? :: <boolean>);
  let nonterminal :: false-or(<nonterminal>)
    = element(grammar.grammar-nonterminals, symbol,
              default: #f);
  if (nonterminal)
    if (~slot-initialized?(nonterminal, nonterminal-first-symbols))
      compute-first-symbols!(grammar, nonterminal);
    end if;
    nonterminal.nonterminal-left-recursive?
  else
    #f
  end if
end method;
            
define method compute-nullable-nonterminals!
    (grammar :: <concrete-grammar>) => ();
  let nonterminals :: <object-table> = grammar.grammar-nonterminals;
  let changed? :: <boolean> = #t;

  while (changed?)
    changed? := #f;
    for (nonterminal in nonterminals)
      
if (~nonterminal.nonterminal-nullable?)
  if (any?(method(production)
             every?(curry(grammar-symbol-nullable?, grammar),
                    production.production-derives);
           end method,
           nonterminal.nonterminal-productions))
    nonterminal.nonterminal-nullable? := #t;
    changed? := #t;
  end if;
end if;
            
    end for;
  end while;
end method;
            
define method compute-first-symbols!
    (grammar :: <concrete-grammar>,
     nonterminal :: <nonterminal>)
  => ();
  local
    
method visit
    (nonterminal :: <nonterminal>,
     traverse :: <function>)
 => ();
  nonterminal.nonterminal-first-symbols := #();

  for (production in nonterminal.nonterminal-productions,
       first-symbols = #()
         then for (symbol in production.production-derives,
                   production-first-symbols = first-symbols
                     then
                       begin
                         let child :: false-or(<nonterminal>)
                           = element(grammar.grammar-nonterminals,
                                     symbol, default: #f);
                         if (child)
                           if (~slot-initialized?(child,
                                                  nonterminal-first-symbols))
                             traverse(child);
                           elseif (child == nonterminal)
                             nonterminal.nonterminal-left-recursive? := #t;
                           end if;
                           union(child.nonterminal-first-symbols,
                                 production-first-symbols);
                         else
                           add-new(production-first-symbols, symbol);
                         end if;
                       end,
                   prefix? = #t
                     then grammar-symbol-nullable?(grammar, symbol),
                   while: prefix?)
              finally
                production-first-symbols;
              end for)
  finally
    nonterminal.nonterminal-first-symbols := first-symbols;
  end for;
end method,
            
method collapse
    (scc-head :: <nonterminal>,
     scc-other :: <nonterminal>)
 => ();
  scc-other.nonterminal-first-symbols
    := scc-head.nonterminal-first-symbols;
  scc-other.nonterminal-left-recursive?
    := scc-head.nonterminal-left-recursive?
    := #t;
end method;
            
  find-rooted-scc(nonterminal, visit, collapse);
end method;
            
