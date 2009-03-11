module: dfmc-typist

define method dynamic? (type ::  <&type>) => (res :: <boolean>)
  type == dylan-value(#"<object>"); //or type-=? ?
end;

define method dynamic? (tv :: <&type-variable>) => (res :: <boolean>)
  tv.^type-variable-contents.dynamic?
end;

define method dynamic? (tv :: <&dynamic-type>) => (res == #t)
  #t
end;

define method arrow? (type :: <&type>) => (res :: <boolean>)
  instance?(type, <&limited-function-type>)
end;

define method arrow? (type :: <&arrow-type>) => (res == #t)
  #t
end;

define method arrow? (tv :: <&type-variable>) => (res :: <boolean>)
  tv.^type-variable-contents.arrow?
end;

