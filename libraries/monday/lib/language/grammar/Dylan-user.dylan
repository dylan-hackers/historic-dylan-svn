Module: Dylan-user


define library grammar
  use common-dylan;
  
export digraph;
          
export grammar;
          
export parser-automaton;
          
export dependent-set;
              
end library;
          
define module digraph
  use common-dylan;
  export find-rooted-scc;
end module;
          
define module dependent-set
  use common-dylan;
  use digraph;
  export <dependent-set>,
         dependent-set-initial,
         dependent-set-initial-setter,
         dependent-set-value,
         dependent-set-dependents,
         add-set-dependent!;
end module;
              
define module grammar
  use common-dylan;
  
use digraph;
            
  
export <grammar>,
       grammar-symbol-productions,
       grammar-symbol-nonterminal?,
       grammar-symbol-nullable?,
       grammar-symbol-left-recursive?,
       grammar-symbol-first-symbols;
           
export <production>, production-nonterminal, production-derives;
           
end module;
          
define module parser-automaton
  use common-dylan;
  use grammar;
  
use dependent-set;
              
  
export <parser-automaton>, make-parser-automaton;
            
export
  <parser-automaton-error>,
  parser-automaton-error-productions,
  parser-automaton-error-inputs;
            
export <lr-parser-automaton>;
export lr-parser-automaton-initial-state,
       lr-parser-automaton-terminal-transition,
       lr-parser-automaton-nonterminal-transition,
       lr-parser-automaton-transition-terminals,
       lr-parser-automaton-transition-nonterminals;
            
export
  <parser-automaton-reduce/reduce-error>,
  <parser-automaton-shift/reduce-error>,
  <parser-automaton-reduce/reduce-restart>,
  <parser-automaton-shift/reduce-restart>;
            
end module;
          
