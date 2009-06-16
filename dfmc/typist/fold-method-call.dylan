module: dfmc-typist

define method fold-function-call (t == #f,
				  function, arguments) => (call-values)
  apply(function, map(constant-value, arguments));
  // result ignored
  #f
end;

define method replace-call-with-values (call-value,
					call :: <call>,
					t == #f) => ()
  delete-computation!(call);
end;

define method fold-function-call (t :: <multiple-value-temporary>,
				  function, arguments) => (call-values)
  let (#rest call-values) = apply(function, map(constant-value, arguments));
  call-values
end;

define method replace-call-with-values (call-values,
					call :: <call>,
					t :: <multiple-value-temporary>) => ()
  let values-temps = map(make-object-reference, call-values);
  // format-out("XXX doing %=.\n", call);
  let padded-values 
    = apply(pad-multiple-values, 
            call.environment, 
            temporary-value-context(t),
            values-temps);              
  let (values-c, values-t) =
    make-with-temporary(environment(call),
                        <values>,
                        values: padded-values,
                        temporary-class: <multiple-value-temporary>);
  values-t.required-values := size(padded-values);
  values-t.rest-values? := #f;
  // format-out("\tgot %=.\n", values-c);
  replace-computation!(call, values-c, values-c, values-t);
end;

define method fold-function-call (t :: <temporary>,
				  function, arguments) => (call-value)
  apply(function, map(constant-value, arguments))
end;
 
define method replace-call-with-values (call-value,
					call :: <call>,
					t :: <temporary>) => ()
  replace-computation-with-temporary!(call, make-object-reference(call-value));
end;

define function maybe-fold-function-call (call, t, function, arg-t*)
  let (call-values, okay?) =
    block ()
      values(fold-function-call(t, function, arg-t*), #t)
    exception (e :: <error>)
      values(#f, #f)		// silently fail to fold
    end;
  if (okay?)
    re-optimize-generators(call.arguments);
    replace-call-with-values(call-values, call, t);
    #t
  end;
end;

