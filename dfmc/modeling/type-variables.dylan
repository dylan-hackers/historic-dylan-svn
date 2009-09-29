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

define method ^subtype? (s :: <&subclass>, tv :: <&polymorphic-type-variable>)
 => (sub? :: <boolean>)
  ^subtype?(s, tv.^type-variable-bound)
end;

define method ^subtype? (tv :: <&polymorphic-type-variable>, s :: <&subclass>)
 => (sub? :: <boolean>)
  ^subtype?(tv.^type-variable-bound, s)
end;

define method ^subtype? (u :: <&union>, tv :: <&polymorphic-type-variable>)
 => (sub? :: <boolean>)
  ^subtype?(u, tv.^type-variable-bound)
end;

define method ^subtype? (tv :: <&polymorphic-type-variable>, u :: <&union>)
 => (sub? :: <boolean>)
  ^subtype?(tv.^type-variable-bound, u)
end;

define method ^subtype? (tv :: <&polymorphic-type-variable>, t :: <&top-type>)
 => (res? == #t)
  #t
end;

define method ^subtype? (lct :: <&limited-collection-type>, tv :: <&polymorphic-type-variable>)
 => (sub? :: <boolean>)
  ^subtype?(lct, tv.^type-variable-bound)
end;

define method ^subtype? (tv :: <&polymorphic-type-variable>, lct :: <&limited-collection-type>)
 => (sub? :: <boolean>)
  ^subtype?(tv.^type-variable-bound, lct)
end;

define method ^known-disjoint? (a :: <&polymorphic-type-variable>, b :: <&polymorphic-type-variable>)
 => (disj? :: <boolean>)
  ^known-disjoint?(a.^type-variable-bound, b.^type-variable-bound)
end;

define method ^known-disjoint? (a :: <&polymorphic-type-variable>, b :: <&type>)
 => (disj? :: <boolean>)
  ^known-disjoint?(a.^type-variable-bound, b)
end;
