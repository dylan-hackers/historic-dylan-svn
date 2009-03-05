module: dfmc-typist

define method type-estimate (o :: <object>) => (te :: <type-estimate>)
  make(<type-estimate-top>);
end;

define method type-estimate (l :: <&lambda>) => (te :: <&type>)
  type-infer(l);
  make(<&top-type>);
end;

/*
define method lookup-type (o :: <object>) => (te :: <&type>)
  make(<&top-type>);
end;
*/
