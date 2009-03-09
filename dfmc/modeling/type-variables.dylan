module: dfmc-modeling


define primary &class <polymorphic-type-variable> (<type>)
  constant &slot type-variable-name :: <symbol>,
    required-init-keyword: name:;
  constant &slot type-variable-kind :: <type>,
    init-value: <&object>,
    init-keyword: kind:;
end;

/*
define primary &class <variable-arity-same-type-variable> (<polymorphic-type-variable>)
end;

define primary &class <variable-arity-different-type-variable> (<polymorphic-type-variable>)
  &slot type-variables :: <list>, init-value: #();
end;
*/

	
