module: dfmc-modeling


define primary &class <polymorphic-type-variable> (<type>)
  constant &slot type-variable-name :: <symbol>,
    required-init-keyword: name:;
  constant &slot type-variable-bound :: <type>,
    //init-value: <type>,
    required-init-keyword: bound:;
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
  ^subtype?(a.^type-variable-bound, b.^type-variable-bound)
end;

define method ^subtype? (a :: <&polymorphic-type-variable>, b :: <&singleton>)
 => (sub? :: <boolean>)
  #f
end;

define method ^subtype? (a :: <&polymorphic-type-variable>, b :: <&type>)
 => (sub? :: <boolean>)
  ^subtype?(a.^type-variable-bound, b)
end;

define method ^subtype? (a :: <&type>, b :: <&polymorphic-type-variable>)
 => (sub? :: <boolean>)
  ^subtype?(a, b.^type-variable-bound)
end;

define method ^subtype? (a :: <&top-type>, b :: <&polymorphic-type-variable>)
 => (sub? :: <boolean>)
  ^subtype?(a, b.^type-variable-bound)
end;

define method ^subtype? (a :: <&singleton>, b :: <&polymorphic-type-variable>)
 => (sub? :: <boolean>)
  ^instance?(a.^singleton-object, b.^type-variable-bound)
end;

define method ^known-disjoint? (a :: <&polymorphic-type-variable>, b :: <&polymorphic-type-variable>)
 => (disj? :: <boolean>)
  ^known-disjoint?(a.^type-variable-bound, b.^type-variable-bound)
end;

define method ^known-disjoint? (a :: <&polymorphic-type-variable>, b :: <&type>)
 => (disj? :: <boolean>)
  ^known-disjoint?(a.^type-variable-bound, b)
end;
