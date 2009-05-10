module: dfmc-modeling


define primary &class <polymorphic-type-variable> (<type>)
  constant &slot type-variable-name :: <symbol>,
    required-init-keyword: name:;
  constant &slot type-variable-kind :: <type>,
    //init-value: <type>,
    required-init-keyword: kind:;
end;

/*
define primary &class <variable-arity-same-type-variable> (<polymorphic-type-variable>)
end;

define primary &class <variable-arity-different-type-variable> (<polymorphic-type-variable>)
  &slot type-variables :: <list>, init-value: #();
end;
*/

define method ^subtype? (a :: <&polymorphic-type-variable>, b :: <&polymorphic-type-variable>)
 => (sub? :: <boolean>)
  ^subtype?(a.^type-variable-kind, b.^type-variable-kind)
end;

define method ^subtype? (a :: <&polymorphic-type-variable>, b :: <&type>)
 => (sub? :: <boolean>)
  ^subtype?(a.^type-variable-kind, b);
end;

define method ^known-disjoint? (a :: <&polymorphic-type-variable>, b :: <&polymorphic-type-variable>)
 => (disj? :: <boolean>)
  ^known-disjoint?(a.^type-variable-kind, b.^type-variable-kind);
end;

define method ^known-disjoint? (a :: <&polymorphic-type-variable>, b :: <&type>)
 => (disj? :: <boolean>)
  ^known-disjoint?(a.^type-variable-kind, b);
end;
