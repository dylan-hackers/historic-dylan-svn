module: dfmc-typist

define constant <type-environment> = <table>;

define thread variable *type-environment* :: false-or(<type-environment>) = #f;

//doesn't work, needs current-library-description (which is empty during startup)
//for type-cache of library
//define constant $te-bottom = make(<type-estimate-bottom>);
define function lookup-type-variable (o :: <object>) => (res :: <type-variable>)
  let tenv = *type-environment*;
  let res = element(tenv, o, default: #f);
  unless (res)
    let te = type-estimate-object(o);
    tenv[o] := make(<type-variable>, contents: (te & as(<type-estimate>, te)) | make(<type-estimate-bottom>));
  end;
  res | tenv[o];
end;

define generic type-estimate-object (o :: <object>) => (res :: false-or(<&type>));

define method type-estimate-object (o :: <object>) => (res == #f)
  #f;
end;

define method type-estimate-object (o :: <lexical-required-variable>)
 => (res :: <&type>);
  o.specializer
end;

define thread variable *assumptions* :: false-or(<stretchy-vector>) = #f;
define thread variable *constraints* :: false-or(<stretchy-vector>) = #f;

define method add-constraint (c :: <constraint>)
  add!(*constraints*, c);
end;

define method add-assumption (a)
  add!(*assumptions*, a);
end;

define method type-infer (d :: <dfm-ref>)
  //catch-all method
end;

define method type-infer (l :: <&lambda>)
  dynamic-bind(*constraints* = make(<stretchy-vector>),
               *assumptions* = make(<stretchy-vector>),
               *type-environment* = make(<type-environment>))
    do(lookup-type-variable, l.parameters);
    walk-computations(infer-computation-types, l.body, l.body.bind-return);
  end;
end;

define generic infer-computation-types (c :: <computation>);

define method infer-computation-types (c :: <computation>)
  if (c.temporary)
    lookup-type-variable(c.temporary);
  end;
  format-out("skipping %=\n", c);
end;

define method infer-computation-types (c :: <function-call>)
  let fun = c.function.reference-value;
  infer-function-type(fun, c.arguments, c.temporary);
end;

define method infer-function-type (fun :: <&function>, arguments :: <vector>, result)
  format-out("got %=\n", fun);
  //constraints: arg-types equal passed arguments
  //keyword-arguments, #rest!
  let specializers = ^function-specializers(fun);
  let args = map(lookup-type-variable, arguments);
  for (arg in args, spec in specializers)
    add-constraint(make(<equality-constraint>, left: arg, right: spec));
  end;
  //constraints: return type is type of temporary (MV!)
  if (result)
    let sig = ^function-signature(fun);
    if (sig.^signature-number-values == 1) //handle other cases (mvt, #rest)
      add-constraint(make(<equality-constraint>,
                          left: lookup-type-variable(result),
                          right: sig.^signature-values[0]));
    end;
  end;
end;
