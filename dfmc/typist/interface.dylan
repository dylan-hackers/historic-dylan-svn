module: dfmc-typist

define method type-estimate (o :: <object>) => (te :: <type-estimate>)
  make(<type-estimate-bottom>);
end;

define method type-estimate (l :: <&lambda>) => (te :: <&type>)
  type-infer(l);
  make(<&bottom-type>);
end;

define method lookup-type (o :: <object>) => (te :: <&type>)
  make(<&bottom-type>);
end;

