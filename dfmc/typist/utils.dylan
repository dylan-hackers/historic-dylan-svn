module: dfmc-typist

define function argument-types (lft :: <&limited-function-type>)
 => (res :: <simple-object-vector>)
  lft.^limited-function-argument-types
end;
define function value-types (lft :: <&limited-function-type>)
 => (res :: <simple-object-vector>)
  lft.^limited-function-return-values
end;

define method dynamic? (type ::  <&type>) => (res :: <boolean>)
  type == dylan-value(#"<object>"); //or type-=? ?
end;

define method dynamic? (tv :: <type-variable>) => (res :: <boolean>)
  tv.type-variable-contents.dynamic?
end;

define method arrow? (type :: <&type>) => (res :: <boolean>)
  instance?(type, <&limited-function-type>)
end;

define method arrow? (tv :: <type-variable>) => (res :: <boolean>)
  tv.type-variable-contents.arrow?
end;
