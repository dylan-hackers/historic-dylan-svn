Module: parser-automaton


define open abstract class <parser-automaton> (<object>)
  // No additional slots
end class;
                
define sealed method make
    (type == <parser-automaton>,
     #rest key-value-pairs,
     #key class :: <symbol> = #"LALR-1",
     #all-keys)
 => (instance :: <parser-automaton>);
  apply(make-parser-automaton, class, key-value-pairs);
end method;
                
define open generic make-parser-automaton
    (grammar-class :: <symbol>, #rest key-value-pairs, #key)
 => (instance :: <parser-automaton>);
                
define open abstract class <parser-automaton-error> (<error>)
  constant slot parser-automaton-error-productions :: <sequence>,
    required-init-keyword: productions:;
  constant slot parser-automaton-error-inputs :: <sequence> = #(),
    init-keyword: inputs:;
end class;
                
define open abstract class <lr-parser-automaton> (<parser-automaton>)
  // No additional slots
end class;
                
define open generic lr-parser-automaton-initial-state
    (automaton :: <lr-parser-automaton>, start-symbol :: <object>)
 => (state :: <object>);
                
define open generic lr-parser-automaton-terminal-transition
    (automaton :: <lr-parser-automaton>,
     state :: <object>,
     terminal :: <object>)
 => (action :: one-of(#"shift", #"reduce", #"error", #"accept"),
     data :: <object>);
                
define open generic lr-parser-automaton-nonterminal-transition
    (automaton :: <lr-parser-automaton>,
     state :: <object>,
     nonterminal :: <object>)
 => (new-state :: <object>);
                
define open generic lr-parser-automaton-transition-terminals
    (automaton :: <lr-parser-automaton>,
     state :: <object>)
 => (terminals :: false-or(<sequence>));
                
define open generic lr-parser-automaton-transition-nonterminals
    (automaton :: <lr-parser-automaton>,
     state :: <object>)
 => (nonterminals :: false-or(<sequence>));
                
define class <parser-automaton-reduce/reduce-error>
    (<parser-automaton-error>)
  // No additional slots
end class;
                
define class <parser-automaton-shift/reduce-error>
    (<parser-automaton-error>)
  // No additional slots
end class;
                
define class <parser-automaton-reduce/reduce-restart> (<restart>)
  constant slot parser-automaton-restart-action :: <production>,
    required-init-keyword: action:;
end class;
                
define class <parser-automaton-shift/reduce-restart> (<restart>)
  constant slot parser-automaton-restart-action :: 
    type-union(singleton(#"shift"), <production>),
    required-init-keyword: action:;
end class;
                
define abstract class <canonical-lr-parser-automaton>
    (<lr-parser-automaton>)
  
constant slot lr-parser-automaton-itemsets :: <lr-itemset-table>
  = make(<lr-itemset-table>);
              
constant slot lr-parser-automaton-start-itemsets :: <object-table>
  = make(<object-table>);
              
end class;
            
define method initialize
    (automaton :: <canonical-lr-parser-automaton>,
     #next next-method,
     #key grammar :: <grammar>,
          start-symbols :: <object>,
          end-symbol :: <object>,
     #all-keys)
 => ();
  local
    
method locate-lr-itemset
    (kernel-items :: <list>)
 => (itemset :: <lr-itemset>);
  let itemset = element(automaton.lr-parser-automaton-itemsets,
                        kernel-items, default: #f);
  if (itemset)
    itemset;
  else
    let itemset
      = make-lr-itemset(automaton,
                        kernel-items: kernel-items);
    element(automaton.lr-parser-automaton-itemsets, kernel-items) := itemset;
    compute-reductions-and-successors!(itemset,
                                       completion-items(automaton,
                                                        grammar,
                                                        kernel-items));
    itemset;
  end if;
end method,
              
method compute-reductions-and-successors!
    (itemset :: <lr-itemset>, completions :: <list>)
 => ();
  let shift-kernels-table :: <object-table>
    = make(<object-table>);
  let goto-kernels-table :: <object-table>
    = make(<object-table>);
  local
    
method add-item-to-successor-kernel(item :: <lr-item>) => ();
  if (item-dot-at-end?(item))
    itemset.itemset-reductions := add(itemset.itemset-reductions, item);
  else
    let symbol = item-symbol-after-dot(item);
    if (grammar-symbol-nonterminal?(grammar, symbol))
      element(goto-kernels-table, symbol)
        := add(element(goto-kernels-table, symbol, default: #()),
               item-successor(item));
    else
      element(shift-kernels-table, symbol)
        := add(element(shift-kernels-table, symbol, default: #()),
               item-successor(item));
    end if;
  end if;
end method
              ;
  do(add-item-to-successor-kernel, itemset.itemset-kernel-items);
  do(add-item-to-successor-kernel, completions);

  for (kernel keyed-by symbol in shift-kernels-table)
    itemset.itemset-shift-table[symbol] := locate-lr-itemset(kernel);
  end for;

  for (kernel keyed-by symbol in goto-kernels-table)
    itemset.itemset-goto-table[symbol] := locate-lr-itemset(kernel);
  end for;
end method
              ;
  next-method();
  
for(start-symbol in start-symbols)
  let unique-start-symbol = list(start-symbol);
  let start-production = make(<production>,
                              nonterminal: unique-start-symbol,
                              derives: vector(start-symbol, end-symbol));
  let start-item = make-lr-item(automaton,
                                production: start-production,
                                dot: 0);
  let start-state = locate-lr-itemset(list(start-item));
  automaton.lr-parser-automaton-start-itemsets[start-symbol] := start-state;

  
let penultimate-state = start-state.itemset-goto-table[start-symbol];
let accepting-state = penultimate-state.itemset-shift-table[end-symbol];
accepting-state.itemset-accepting? := #t;
              
end for;
              
end method;
            
define abstract class <lr-item> (<object>)
  constant slot item-production :: <production>,
    required-init-keyword: production:;
  constant slot item-dot :: <integer>,
    required-init-keyword: dot:;
end class;
              
define generic make-lr-item
    (automaton :: <lr-parser-automaton>, #key, #all-keys)
 => (instance :: <lr-item>);
              
define method item-dot-at-end?
    (item :: <lr-item>)
 => (at-end? :: <boolean>);
  item.item-dot = item.item-production.production-derives.size;
end method;
              
define method item-symbol-after-dot
    (item :: <lr-item>)
 => (symbol :: <object>);
  item.item-production.production-derives[item.item-dot];
end method;
              
define method item-successor
    (item :: <lr-item>)
 => (new-item :: <lr-item>);
  make(object-class(item),
       production: item.item-production,
       dot: item.item-dot + 1);
end method;
              
define method item=? (item1 :: <lr-item>, item2 :: <lr-item>)
 => (result :: <boolean>);
  item1.item-production == item2.item-production
    & item1.item-dot = item2.item-dot;
end method;
              
define abstract class <lr-itemset> (<object>)
  constant slot itemset-kernel-items :: <list>,
    required-init-keyword: kernel-items:;
  
constant slot itemset-shift-table :: <object-table>
  = make(<object-table>);
constant slot itemset-goto-table :: <object-table>
  = make(<object-table>);
              
slot itemset-reductions :: <list> = #();
              
slot itemset-accepting? :: <boolean> = #f;
              
end class;
              
define generic make-lr-itemset
    (automaton :: <lr-parser-automaton>, #key, #all-keys)
 => (instance :: <lr-itemset>);
              
define class <lr-itemset-table> (<table>)
  // No additional slots
end class;
              
define function itemset-kernel=?
     (items1 :: <list>, items2 :: <list>)
  => (equal? :: <boolean>);
  (size(items1) = size(items2))
    & every?(method(item) member?(item, items2, test: item=?) end,
             items1);
end function;
              
define method item-hash
    (item :: <lr-item>, initial-state :: <object>)
 => (hash-id :: <integer>, hash-state :: <object>);
  let (production-id, production-state)
    = object-hash(item.item-production, initial-state);
  let (dot-id, dot-state) = object-hash(item.item-dot, production-state);
  let merged-id = merge-hash-ids(production-id, dot-id, ordered: #t);
  values(merged-id, dot-state);
end method;
              
define function itemset-kernel-hash
    (items :: <list>, initial-state :: <object>)
 => (hash-id :: <integer>, hash-state :: <object>);
  if (empty?(items))
    values(0, initial-state)
  else
    let (hash-id :: <integer>, hash-state :: <object>)
      = item-hash(first(items), initial-state);

    for (item in items, first? = #t then #f)
      if (~first?)
        let (item-hash-id :: <integer>, item-hash-state :: <object>)
          = item-hash(item, hash-state);
        hash-id := merge-hash-ids(hash-id, item-hash-id, ordered: #f);
        hash-state := item-hash-state;
      end if;
    end for;

    values(hash-id, hash-state);
  end if;
end function;
              
define method table-protocol(table :: <lr-itemset-table>)
  => (test-function :: <function>, hash-function :: <function>);
  values(itemset-kernel=?, itemset-kernel-hash);
end method;
              
define sealed domain table-protocol(<lr-itemset-table>);
              
define method completion-items
    (automaton :: <lr-parser-automaton>,
     grammar :: <grammar>,
     kernel-items :: <sequence>)
 => (completion-items :: <sequence>);
  let completed-nonterminals :: <list> = #();
  let completion-items :: <list> = #();

  local
    
method scan-items
    (unscanned-items :: <sequence>)
 => ();
  for (item :: <lr-item> in unscanned-items)
    if (~item-dot-at-end?(item))
      let symbol = item-symbol-after-dot(item);
      if (grammar-symbol-nonterminal?(grammar, symbol)
            & ~member?(symbol, completed-nonterminals))
        let added-completions
          = map(as-completion-item,
                grammar-symbol-productions(grammar, symbol));
        completed-nonterminals := add(completed-nonterminals, symbol);
        completion-items := concatenate(added-completions,
                                        completion-items);
        scan-items(added-completions);
      end if;
    end if;
  end for;
end method,
              
method as-completion-item
    (production :: <production>)
 => (item :: <lr-item>);
  make-lr-item(automaton, production: production, dot: 0);
end method
              ;

  scan-items(kernel-items);

  completion-items;
end method;
              
define method lr-parser-automaton-initial-state
    (automaton :: <canonical-lr-parser-automaton>,
     start-symbol :: <object>)
 => (state :: <lr-itemset>)
  automaton.lr-parser-automaton-start-itemsets[start-symbol];
end method;
              
define class <lr0-parser-automaton> (<canonical-lr-parser-automaton>)
  // No additional slots
end class;
              
define sealed method make-parser-automaton
    (class == #"LR-0", #rest key-value-pairs, #key, #all-keys)
 => (instance :: <lr0-parser-automaton>);
  apply(make, <lr0-parser-automaton>, key-value-pairs);
end method;
              
define class <lr0-item> (<lr-item>)
  // No additional slots
end class;
              
define method make-lr-item
    (automaton :: <lr0-parser-automaton>,
     #rest key-value-pairs, #key, #all-keys)
 => (instance :: <lr-item>);
  apply(make, <lr0-item>, key-value-pairs);
end method;
              
define class <lr0-itemset> (<lr-itemset>)
  // No additional slots
end class;
              
define method make-lr-itemset
    (automaton :: <lr0-parser-automaton>,
     #rest key-value-pairs, #key, #all-keys)
 => (instance :: <lr-itemset>);
  apply(make, <lr0-itemset>, key-value-pairs);
end method;
              
define method initialize
    (automaton :: <lr0-parser-automaton>,
     #next next-method,
     #key, #all-keys)
 => ();
  next-method();
  for(itemset in automaton.lr-parser-automaton-itemsets)
    if(itemset.itemset-reductions.size > 1)
      error(make(<parser-automaton-reduce/reduce-error>,
                 productions: itemset.itemset-reductions));
    elseif(~empty?(itemset.itemset-reductions)
           & ~empty?(itemset.itemset-shift-table))
      error(make(<parser-automaton-shift/reduce-error>,
            productions: reduce(concatenate,
                                itemset.itemset-reductions,
                                map(itemset-kernel-items,
                                    itemset.itemset-shift-table))));
    end if;
  end for;
end method;
              
define method lr-parser-automaton-terminal-transition
    (automaton :: <canonical-lr-parser-automaton>,
     state :: <lr-itemset>,
     terminal :: <object>)
 => (action :: one-of(#"shift", #"reduce", #"error", #"accept"),
     data :: <object>);
  let dest = element(state.itemset-shift-table, terminal, default: #f);
  if(dest)
    if(itemset-accepting?(dest))
      values(#"accept", #t)
    else
      values(#"shift", dest);
    end if;
  else
    if(~empty?(state.itemset-reductions))
      values(#"reduce", first(state.itemset-reductions));
    else
      values(#"error", #f);
    end if;
  end if;
end method;
              
define method lr-parser-automaton-nonterminal-transition
    (automaton :: <canonical-lr-parser-automaton>,
     state :: <lr-itemset>,
     nonterminal :: <object>)
 => (new-state :: <object>);
  state.itemset-goto-table[nonterminal];
end method;
              
define method lr-parser-automaton-transition-terminals
    (automaton :: <canonical-lr-parser-automaton>,
     state :: <lr-itemset>)
 => (terminals :: false-or(<sequence>))
  let terminals = state.itemset-shift-table.key-sequence;
  if(empty?(terminals))
    #f;
  else
    terminals;
  end if;
end method;
              
define method lr-parser-automaton-transition-nonterminals
    (automaton :: <canonical-lr-parser-automaton>,
     state :: <lr-itemset>)
 => (nonterminals :: false-or(<sequence>))
  let nonterminals = state.itemset-goto-table.key-sequence;
  if(empty?(nonterminals))
    #f;
  else
    nonterminals;
  end if;
end method;
              
define class <slr1-parser-automaton> (<canonical-lr-parser-automaton>)
  // No additional slots
end class;
            
define sealed method make-parser-automaton
    (class == #"SLR-1", #rest key-value-pairs, #key, #all-keys)
 => (instance :: <slr1-parser-automaton>);
  apply(make, <slr1-parser-automaton>, key-value-pairs);
end method;
            
define class <lr1-item> (<lr-item>)
  slot item-lookahead :: <list>;
end class;
            
define method make-lr-item
    (automaton :: <slr1-parser-automaton>,
     #rest key-value-pairs, #key, #all-keys)
 => (instance :: <lr-item>);
  apply(make, <lr1-item>, key-value-pairs);
end method;
            
define class <lr1-itemset> (<lr-itemset>)
  constant slot itemset-reduction-table :: <object-table>
    = make(<object-table>);
end class;
            
define method make-lr-itemset
    (automaton :: <slr1-parser-automaton>,
     #rest key-value-pairs, #key, #all-keys)
 => (instance :: <lr-itemset>);
  apply(make, <lr1-itemset>, key-value-pairs);
end method;
            
define method initialize
    (automaton :: <slr1-parser-automaton>,
     #next next-method,
     #key grammar :: <grammar>,
          start-symbol :: <object>,
          end-symbol :: <object>,
          full-lookahead? :: <object> = #f,
     #all-keys)
 => ();
  next-method();
  
let follow = make(<object-table>);
              
local
  method follow-dependent-set(nonterminal)
   => (set :: <dependent-set>);
    element(follow, nonterminal, default: #f)
      | (follow[nonterminal] := make(<dependent-set>, initial: #()));
  end;
              
for(itemset in automaton.lr-parser-automaton-itemsets)
  for(goto-itemset keyed-by goto-symbol in itemset.itemset-goto-table)
    let (trans-symbols, right-dependency-items)
      = itemset-trans-symbols(grammar, goto-itemset);
    let set = follow-dependent-set(goto-symbol);
    set.dependent-set-initial
      := union(set.dependent-set-initial, trans-symbols);
    for(item in right-dependency-items)
      let rd = item.item-production.production-nonterminal;
      add-set-dependent!(set, follow-dependent-set(rd));
    end for;
  end for;
end for;
              
for(itemset in automaton.lr-parser-automaton-itemsets)
  if(full-lookahead?
     | (itemset.itemset-reductions.size > 1)
     | (itemset.itemset-reductions.size > 0
        & itemset.itemset-shift-table.size > 0))
    for(item in itemset.itemset-reductions)
      let nonterminal = item.item-production.production-nonterminal;
      item.item-lookahead
        := follow-dependent-set(nonterminal).dependent-set-value;
      fill-itemset-reduction-table!(itemset, item);
    end for;
  end if;
end for;
              
end method;
            
define method itemset-trans-symbols
    (grammar :: <grammar>,
     itemset :: <lr1-itemset>)
 => (trans-symbols :: <sequence>,
     right-dependency-items :: <sequence>);
  local
    
method production-first-symbols-after
    (production :: <production>, start :: <integer>)
 => (first-symbols-after :: <sequence>,
     nullable-after? :: <boolean>);
  for (index from start below production.production-derives.size,
       first-symbols = #()
         then union(grammar-symbol-first-symbols
                      (grammar, production.production-derives[index]),
                    first-symbols),
       prefix? = #t
         then grammar-symbol-nullable?
               (grammar, production.production-derives[index]),
       while: prefix?)
  finally
    values(first-symbols, prefix?);
  end for;
end
              ;

  let trans-symbols = #();
  let right-dependency-items = #();
  for(item in itemset.itemset-kernel-items)
    let (first-symbols, nullable?)
      = production-first-symbols-after(item.item-production, item.item-dot);
    trans-symbols := union(trans-symbols, first-symbols);
    if(nullable?)
      right-dependency-items
        := add(right-dependency-items, item);
    end if;
  end for;
  values(trans-symbols, right-dependency-items);
end method;
              
define method fill-itemset-reduction-table!
    (itemset :: <lr1-itemset>, item :: <lr1-item>)
 => ();
  for(lookahead-symbol in item.item-lookahead)
    let production = element(itemset.itemset-reduction-table,
                             lookahead-symbol, default: #f);
    if(production)
       
block ()
  error(make(<parser-automaton-reduce/reduce-error>,
             productions: list(item.item-production, production),
             inputs: list(lookahead-symbol)));
exception (restart :: <parser-automaton-reduce/reduce-restart>)
  if (restart.parser-automaton-restart-action == item.item-production)
    element(itemset.itemset-reduction-table, lookahead-symbol)
      := item.item-production;
  elseif (restart.parser-automaton-restart-action ~== production)
    error("bad recovery action %=", restart.parser-automaton-restart-action);
  end if;
end block;
              
    else
      let shift-itemset = element(itemset.itemset-shift-table,
                                  lookahead-symbol, default: #f);
      if(shift-itemset)
         
block ()
  error(make(<parser-automaton-shift/reduce-error>,
             productions:
               pair(item.item-production,
                    map-as(<list>, item-production,
                           shift-itemset.itemset-kernel-items)),
             inputs: list(lookahead-symbol)));
exception (restart :: <parser-automaton-shift/reduce-restart>)
  if (restart.parser-automaton-restart-action == item.item-production)
    remove-key!(itemset.itemset-shift-table, lookahead-symbol);
    element(itemset.itemset-reduction-table, lookahead-symbol)
      := item.item-production;
  elseif (restart.parser-automaton-restart-action ~== #"shift")
    error("bad recovery action %=", restart.parser-automaton-restart-action);
  end if;
end block;
              
      else
        element(itemset.itemset-reduction-table, lookahead-symbol)
          := item.item-production;
      end if;
    end if;
  end for;
end method;
              
define method lr-parser-automaton-terminal-transition
    (automaton :: <canonical-lr-parser-automaton>,
     state :: <lr1-itemset>,
     terminal :: <object>)
 => (action :: one-of(#"shift", #"reduce", #"error", #"accept"),
     data :: <object>);
  let dest = element(state.itemset-shift-table, terminal, default: #f);
  if(dest)
    if(itemset-accepting?(dest))
      values(#"accept", #t)
    else
      values(#"shift", dest);
    end if;
  else
    let reduce = element(state.itemset-reduction-table, terminal, default: #f);
    if(reduce)
      values(#"reduce", reduce);
    elseif(state.itemset-reductions.size = 1)
      values(#"reduce", first(state.itemset-reductions).item-production);
    else
      values(#"error", #f);
    end if;
  end if;
end method;
              
define method lr-parser-automaton-nonterminal-transition
    (automaton :: <canonical-lr-parser-automaton>,
     state :: <lr1-itemset>,
     nonterminal :: <object>)
 => (new-state :: <object>);
  state.itemset-goto-table[nonterminal];
end method;
              
define method lr-parser-automaton-transition-terminals
    (automaton :: <canonical-lr-parser-automaton>,
     state :: <lr1-itemset>)
 => (terminals :: false-or(<sequence>))
  //  Make sure this is complete
  concatenate(state.itemset-shift-table.key-sequence,
              state.itemset-reduction-table.key-sequence);
end method;
              
define class <lalr1-parser-automaton> (<canonical-lr-parser-automaton>)
  // No additional slots
end class;
            
define sealed method make-parser-automaton
    (class == #"LALR-1", #rest key-value-pairs, #key, #all-keys)
 => (instance :: <lalr1-parser-automaton>);
  apply(make, <lalr1-parser-automaton>, key-value-pairs);
end method;
            
define method make-lr-item
    (automaton :: <lalr1-parser-automaton>,
     #rest key-value-pairs, #key, #all-keys)
 => (instance :: <lr-item>);
  apply(make, <lr1-item>, key-value-pairs);
end method;
            
define method make-lr-itemset
    (automaton :: <lalr1-parser-automaton>,
     #rest key-value-pairs, #key, #all-keys)
 => (instance :: <lr-itemset>);
  apply(make, <lr1-itemset>, key-value-pairs);
end method;
             
define method initialize
    (automaton :: <lalr1-parser-automaton>,
     #next next-method,
     #key grammar :: <grammar>,
          start-symbol :: <object>,
          end-symbol :: <object>,
          full-lookahead? :: <object> = #f,
     #all-keys)
 => ();
  next-method();
  
let lookahead = make(<object-table>);
local
  method lookahead-dependent-set
      (item :: <lr-item>)
   => (set :: <dependent-set>);
    element(lookahead, item, default: #f)
      | (lookahead[item] := make(<dependent-set>, initial: #()));
  end method;
            
let trans = make(<object-table>);
let right-dep = make(<object-table>);
local
  method trans-dependent-set
      (itemset :: <lr1-itemset>)
   => (trans-set :: <dependent-set>,
       right-dependency-items :: <sequence>);
    let trans-set = element(trans, itemset, default: #f);
    if(trans-set)
      values(trans-set, right-dep[itemset]);
    else
      let trans-set
        = make(<dependent-set>,
               initial: as(<list>,
                           key-sequence(itemset.itemset-shift-table)));
      for(goto-itemset keyed-by nonterminal in itemset.itemset-goto-table)
        if(grammar-symbol-nullable?(grammar, nonterminal))
          let (goto-trans, goto-rd) = trans-dependent-set(goto-itemset);
          add-set-dependent!(trans-set, goto-trans);
        end if;
      end for;
      
let right-dependency-items = #();
for(item in itemset.itemset-kernel-items)
  if(item-dot-at-end?(item))
    right-dependency-items := add(right-dependency-items, item);
  else
    let sym = item-symbol-after-dot(item);
    if(grammar-symbol-nullable?(grammar, sym))
      let goto-itemset = itemset.itemset-goto-table[sym];
      if(~empty?(right-dep[goto-itemset]))
        right-dependency-items := add(right-dependency-items, item);
      end if;
    end if;
  end if;
end for;
              
      trans[itemset] := trans-set;
      right-dep[itemset] := right-dependency-items;
      values(trans-set, right-dependency-items);
    end if;
  end method;
              
let equiv-nonterminals = make(<object-table>);
local
  method equiv-dependent-set
      (nonterminal :: <object>)
   => (set :: <dependent-set>);
    element(equiv-nonterminals, nonterminal, default: #f)
      | (equiv-nonterminals[nonterminal]
           := make(<dependent-set>, initial: list(nonterminal)));
  end method;
              
let propagates-dot = make(<object-table>);
for(itemset in automaton.lr-parser-automaton-itemsets)
  for(goto-itemset keyed-by goto-symbol in itemset.itemset-goto-table)
    unless(element(trans, goto-itemset, default: #f))
      let (trans-set, right-dependency-items)
        = trans-dependent-set(goto-itemset);
      for(rd-item in right-dependency-items)
        if(rd-item.item-dot = 1)
          
add-set-dependent!
  (equiv-dependent-set(rd-item.item-production.production-nonterminal),
   equiv-dependent-set(goto-symbol));
              
        else
          
unless(element(propagates-dot, rd-item.item-production, default: #f)
       & propagates-dot[rd-item.item-production] < rd-item.item-dot - 1)
  propagates-dot[rd-item.item-production] := rd-item.item-dot - 1;
end unless;
              
        end if;
      end for;
    end unless;
  end for;
end for;
              
local
  method goto
      (itemset :: <lr1-itemset>, symbols :: <sequence>,
       start-dot :: <integer>, end-dot :: <integer>)
   => (destination :: <lr1-itemset>);
    if(start-dot == end-dot)
      itemset;
    else
      let symbol = symbols[start-dot];
      let goto-itemset
        = element(itemset.itemset-goto-table, symbol, default: #f);
      if(goto-itemset)
        goto(goto-itemset, symbols, start-dot + 1, end-dot);
      else
        goto(itemset.itemset-shift-table[symbol], symbols,
             start-dot + 1, end-dot);
      end if;
    end if;
  end method,
              
  method contribute!
      (itemset :: <lr1-itemset>, nonterminal :: <object>,
       set :: <dependent-set>)
   => ();
    for(production in grammar-symbol-productions(grammar, nonterminal))
      block(done)
        for(kernel-item in if(empty?(production.production-derives))
                             itemset.itemset-reductions
                           else
                             let min-dot
                               = element(propagates-dot, production,
                                         default:
                                           production.production-derives.size);
                             let dest
                               = goto(itemset, production.production-derives,
                                      0, min-dot);
                             dest.itemset-kernel-items
                           end if)
          if(kernel-item.item-production == production)
            add-set-dependent!(lookahead-dependent-set(kernel-item), set);
            done();
          end if;
        end for;
      end block;
    end for;
  end method;
              
for(itemset in automaton.lr-parser-automaton-itemsets)
  
for(goto-itemset keyed-by goto-symbol in itemset.itemset-goto-table)
  let (trans-set, right-dependency-items)
    = trans-dependent-set(goto-itemset);
  unless(empty?(trans-set.dependent-set-value))
    for(nonterminal in equiv-dependent-set(goto-symbol).dependent-set-value)
      contribute!(itemset, nonterminal, trans-set);
    end for;
  end unless;
end for;
              
  
for(kernel-item in itemset.itemset-kernel-items)
  unless(item-dot-at-end?(kernel-item))
    let prop = element(propagates-dot, kernel-item.item-production,
                       default: #f);
    if(prop & prop <= kernel-item.item-dot)
      let dot-symbol = item-symbol-after-dot(kernel-item);
      let my-lookahead = lookahead-dependent-set(kernel-item);
      for(nonterminal in equiv-dependent-set(dot-symbol).dependent-set-value)
        contribute!(itemset, nonterminal, my-lookahead);
      end for;

      
let dest = itemset.itemset-goto-table[dot-symbol];
block(done)
  for(dest-item in dest.itemset-kernel-items)
    if(dest-item.item-production == kernel-item.item-production)
      add-set-dependent!(lookahead-dependent-set(dest-item), my-lookahead);
      done();
    end if;
  end for;
end block;
              
    end if;
  end unless;
end for;
              
end for;
              
for(itemset in automaton.lr-parser-automaton-itemsets)
  if(full-lookahead?
     | (itemset.itemset-reductions.size > 1)
     | (itemset.itemset-reductions.size > 0
        & itemset.itemset-shift-table.size > 0))
    for(item in itemset.itemset-reductions)
      let nonterminal = item.item-production.production-nonterminal;
      item.item-lookahead
        := lookahead-dependent-set(item).dependent-set-value;
      fill-itemset-reduction-table!(itemset, item);
    end for;
  end if;
end for;
              
end method;
            
