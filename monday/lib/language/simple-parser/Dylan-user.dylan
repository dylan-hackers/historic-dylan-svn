Module: Dylan-user


define library simple-parser
  use common-dylan;
  use collections;
  use regular;
  use grammar;
  use source-location;
  
export simple-parser;
          
export simple-lexical-definition;
          
export simple-lexical-scanner;
          
export simple-parser-automaton;
          
end library;
          
define module simple-parser
  create
    <simple-lexical-definition>,
    \simple-lexical-definition,
    <simple-lexical-scanner>,
    scan-tokens,
    simple-grammar-productions,
    simple-parser-automaton,
    <simple-parser>,
    simple-parser-reset,
    simple-parser-consume-token,
    simple-parser-can-consume-token?,
    simple-parser-source-location;
end module;
          
define module simple-lexical-definition
  use common-dylan;
  use bit-set;
  use regular-expression,
    rename: { regular-expression-dfa-state-transitions
                => lexical-state-transitions },
    export: { lexical-state-transitions };
  use simple-parser;
  export
    lexical-token-number,
    lexical-token-type,
    lexical-automaton,
    <simple-lexical-state>,
    lexical-state-accept-function;
  create
    token-accept-function;
end module;
          
define module simple-lexical-scanner
  use common-dylan;
  use byte-vector;
  use simple-parser,
    export: { <simple-lexical-scanner>, scan-tokens };
  use simple-lexical-definition;
  use source-location-rangemap;
  use source-location-conditions;

  export
    scanner-lexical-definition,
    scanner-lexical-definition-setter,
    scanner-source-position,
    scanner-source-position-setter;
end module;
          
define module simple-parser-automaton
  use common-dylan;
  use set;
  use simple-parser;
  use simple-lexical-definition;
  use grammar;
  use parser-automaton;
  use source-location;
  export
    <simple-production>,
    production-reduce-action;
  create
    \production-user-reduce-action-function,
    production-auto-reduce-action-function;
end module;
          
define module simple-parser-implementation
  use common-dylan;
  use grammar;
  use parser-automaton;
  use simple-parser;
  use simple-parser-automaton;
  use source-location;
  use source-location-rangemap;
  use source-location-conditions;
end module;
          
