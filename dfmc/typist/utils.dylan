module: dfmc-typist

define function argument-types (lft :: <&limited-function-type>) => (res :: <simple-object-vector>)
  lft.^limited-function-argument-types
end;
define function value-types (lft :: <&limited-function-type>) => (res :: <simple-object-vector>)
  lft.^limited-function-return-values
end;

