Module: Dylan-user


define library regular
  use common-dylan;
  export regular-expression;
end library;
          
define module regular-expression
  use common-dylan;
  
export
  <regular-expression>,
  copy-regular-expression;
          
export
  <epsilon-regular-expression>,
  <symbol-regular-expression>,
  regular-expression-symbol,
  <symbol-set-regular-expression>,
  regular-expression-symbol-set,
  <union-regular-expression>,
  regular-expression-union1,
  regular-expression-union2,
  <concatenation-regular-expression>,
  regular-expression-head,
  regular-expression-tail,
  <closure-regular-expression>,
  regular-expression-enclosed,
  <accept-regular-expression>;
          
export
  <regular-expression-dfa-state>,
  regular-expression-dfa-state-transitions;
          
export regular-expression-dfa;
          
export do-regular-expression-dfa-state-position;
          
end module;
          
