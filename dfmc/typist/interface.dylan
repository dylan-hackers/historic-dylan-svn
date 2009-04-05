module: dfmc-typist

define method type-estimate (o :: <object>) => (te :: type-union(<&type>, <type-estimate>))
  let node = element(*type-environment*, o, default: #f);
  if (node)
    if (instance?(node.node-value, <&type-variable>))
      node.node-value.^type-variable-contents;
    else
      node.node-value;
    end;
  else 
    make(<type-estimate-top>);
  end;
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
