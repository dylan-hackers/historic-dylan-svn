module: dfmc-typist

define constant <type-environment> = <table>;

define thread variable *type-environment* :: false-or(<type-environment>) = #f;

define function lookup-type-variable (o :: <object>) => (res :: <node>)
  let tenv = *type-environment*;
  let res = element(tenv, o, default: #f);
  unless (res)
    let te = type-estimate-object(o);
    if (te == dylan-value(#"<object>"))
      te := make(<&top-type>);
    end;
    let tv = make(<&type-variable>,
                  contents: te | make(<&top-type>));
    debug-types(#"new-type-variable", tv, o, te);
    let n = make(<node>, graph: *graph*, value: tv);
    tenv[o] := n;
    if (te & ~instance?(te, <&top-type>))
      add-constraint(make(<equality-constraint>,
                          left: n, right: te.lookup-type));
    end;
  end;
  res | tenv[o];
end;

define function lookup-type (o :: <object>) => (res :: <node>)
  let tenv = *type-environment*;
  let res = element(tenv, o, default: #f);
  unless (res)
    let te = type-estimate-object(o);
    if (te == dylan-value(#"<object>"))
      te := make(<&top-type>);
    end;
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

//list and string literals, floats!

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
  infer-function-type(c, fun, c.arguments, c.temporary);
end;

define method infer-function-type (c :: <function-call>, fun :: <&function>, arguments :: <vector>, result) => ()
  format-out("got %=\n", fun);
  //keyword-arguments, #rest!
  let sig = ^function-signature(fun);
  let values
    = copy-sequence(sig.^signature-values, end: sig.^signature-number-values);
  let specializers
    = begin
        let args = ^function-specializers(fun);
        let nodes = map(lookup-type, args);
        if (nodes.size == 1)
          nodes.first;
        else
          lookup-type(make(<&tuple-type>, tuples: nodes));
        end;
      end;
  
  let vals
    = begin
        let nodes = map(lookup-type, values);
        if (nodes.size == 1)
          nodes.first;
        else
          lookup-type(make(<&tuple-type>, tuples: nodes));
        end;
      end;

  let left = make(<node>,
                  graph: *graph*,
                  value: make(<&arrow-type>,
                              arguments: specializers,
                              values: vals));

  let args
    = begin
        let nodes = map(lookup-type-variable, arguments);
        if (nodes.size == 1)
          nodes.first;
        else
          lookup-type(make(<&tuple-type>, tuples: nodes));
        end;
      end;

  let right = make(<node>,
                   graph: *graph*,
                   value: make(<&arrow-type>,
                               arguments: args,
                               values: lookup-type-variable(result)));
  add-constraint(make(<equality-constraint>, left: left, right: right));
end;

define method infer-function-type (c :: <simple-call>, gf :: <&generic-function>, arguments :: <vector>, result) => ()
  //simple strategy here:
  // first, solve the type graph (at least the partial graph we have so far)
  //this might conflict with the general idea of the type graph (error narrowing),
  //but recording the types of the GF in the constraint is too generic
  solve(*graph*, *constraints*, *type-environment*);

  // then, try to upgrade the GF call to a simple call (narrowing result type)
  let arguments = map(compose(^type-variable-contents, node-value, lookup-type-variable), arguments);
  let effs = estimate-effective-methods(gf, arguments, c);
  if (~empty?(effs) & maybe-upgrade-gf-to-method-call(c, gf, arguments, effs))
    // finally, record type constraint (beta = tau1 -> tau2)
    //well, this will done by the changed call-graph
    //[get replaced method-call and call infer-function-types there]
    //infer-computation-types(c.previous-computation.next-computation);
  else
    // finally, record type constraint (beta = tau1 -> tau2)
    next-method();
  end;
end;
