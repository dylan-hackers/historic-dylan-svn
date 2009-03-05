module: dfmc-typist

define constant <type-environment> = <table>;

define thread variable *type-environment* :: false-or(<type-environment>) = #f;

define function lookup-type-variable (o :: <object>) => (res :: <node>)
  let tenv = *type-environment*;
  let res = element(tenv, o, default: #f);
  unless (res)
    let te = type-estimate-object(o);
    let tv = make(<type-variable>,
                  contents: te | make(<&top-type>));
    debug-types(#"new-type-variable", tv, o, te);
    let n = make(<node>, graph: *graph*, value: tv);
    tenv[o] := n;
  end;
  res | tenv[o];
end;

define function lookup-type (o :: <object>) => (res :: <node>)
  let tenv = *type-environment*;
  let res = element(tenv, o, default: #f);
  unless (res)
    let te = type-estimate-object(o);
    let n = make(<node>, graph: *graph*, value: te);
    tenv[o] := n;
  end;
  res | tenv[o];
end;

define generic type-estimate-object (o :: <object>) => (res :: false-or(<&type>));

define method type-estimate-object (o :: <object>) => (res == #f)
  #f;
end;

define method type-estimate-object (o :: <&type>) => (res :: <&type>)
  o; //ha, that's easy! :)
end;

define method type-estimate-object (o :: <lexical-required-variable>)
 => (res :: <&type>);
  o.specializer
end;

define method type-estimate-object (o :: <object-reference>) => (res :: false-or(<&type>))
  type-estimate-value(o.reference-value);
end;

define method type-estimate-value (o :: <object>) => (res == #f)
  #f;
end;

define method type-estimate-value (o :: <integer>) => (res :: <&type>)
  dylan-value(#"<integer>");
end;

define thread variable *constraints* :: false-or(<stretchy-vector>) = #f;
define thread variable *graph* :: false-or(<graph>) = #f;

define method add-constraint (c :: <constraint>)
  debug-types(#"constraint", c.left-hand-side, c.right-hand-side);
  add!(*constraints*, c);
end;

define method type-infer (l :: <&lambda>)
  dynamic-bind(*constraints* = make(<stretchy-vector>),
               *type-environment* = make(<type-environment>),
               *graph* = make(<graph>))
    do(lookup-type-variable, l.parameters);
    walk-computations(infer-computation-types, l.body, #f);
    solve(*graph*, *constraints*, *type-environment*);
  end;
end;

define generic infer-computation-types (c :: <computation>) => ();

define method infer-computation-types (c :: <computation>) => ()
  debug-types(#"highlight", c);
  if (c.temporary)
    lookup-type-variable(c.temporary);
  end
end;

define method get-function-object (o :: <object>) => (res :: <&function>)
  error("can't get function object of an <object>");
end;

define method get-function-object (t :: <temporary>) => (f :: <&function>)
  t.generator.computation-closure-method;
end;

define method get-function-object (t :: <object-reference>) => (f :: <&function>)
  if (instance?(t.reference-value, <&function>))
    t.reference-value;
  else
    next-method();
  end;
end;

define method get-function-object (t :: <method-reference>) => (f :: <&function>)
  t.reference-value;
end;
  
define method infer-computation-types (c :: <function-call>) => ()
  next-method();
  let fun = c.function.get-function-object;
  infer-function-type(fun, c.arguments, c.temporary);
end;

define function infer-function-type (fun :: <&function>, arguments :: <vector>, result) => ()
  format-out("got %=\n", fun);
  //keyword-arguments, #rest!
  let specializers = ^function-specializers(fun);
  let sig = ^function-signature(fun);
  let values
    = copy-sequence(sig.^signature-values, end: sig.^signature-number-values);
  let left = make(<node>,
                  graph: *graph*,
                  value: make(<&limited-function-type>,
                              arguments: map(lookup-type, specializers),
                              values: map(lookup-type, values)));

  let args = map(lookup-type-variable, arguments);

  let right = make(<node>,
                   graph: *graph*,
                   value: make(<&limited-function-type>,
                               arguments: args,
                               values: vector(lookup-type-variable(result))));
  add-constraint(make(<equality-constraint>, left: left, right: right));
end;
