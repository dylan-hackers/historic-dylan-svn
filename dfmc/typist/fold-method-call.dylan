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

define method fold-if-merge! 
    (c :: <if>, sav-first :: <computation>, sav-last  :: <computation>, 
     sav-value :: false-or(<value-reference>), del-first :: <computation>)
 => ()
  let f = lambda(environment(c));
  let merge-c = next-computation(c);
  // SPLICE SAVED BRANCH BEGINNING IN PLACE OF IF
  let pc = previous-computation(c);
  let nc = next-computation(merge-c);
  re-optimize(pc);
  if (sav-first == merge-c)
    redirect-previous-computations!(c, nc);
  else
    previous-computation(sav-first) := pc;
    // Redirect the previous computations (c in this case) of if, to have
    // sav-first as their next computation
    redirect-previous-computations!(c, sav-first);
  end if;
  // SPLICE SAVED BRANCH ENDING IN PLACE OF MERGE
  re-optimize(nc); 
  if (sav-last == c)
    redirect-next-computations!(merge-c, pc);
  else
    next-computation(sav-last) := nc;
    // Redirect the next computations (nc in this case) of merge-c, to have
    // sav-last as their previous computation
    redirect-next-computations!(merge-c, sav-last);
  end if;
  // REPLACE MERGE'S TEMPORARY WITH TMP FROM TT SRC OF SAVED BRANCH
  replace-temporary-in-users!(merge-c.temporary, sav-value);
  // DELETE MERGE
  remove-computation-references!(merge-c);
  // RIP OUT DEAD BRANCH
  remove-computation-block-references!
    (del-first, merge-c);
  // REMOVE LAST OF IF
  remove-computation-references!(c);
end method;

define method constant-fold-if (c :: <if>, test-value)
  let merge = c.next-computation;
  let merge-next = merge.next-computation;
  let (sav-first, sav-last, sav-value, del-first) 
    = if (test-value) 
	 values(consequent(c), merge-left-previous-computation(merge),  
		 merge-left-value(merge), alternative(c))
      else 
	 values(alternative(c), merge-right-previous-computation(merge),  
		 merge-right-value(merge), consequent(c))
      end if;
  fold-if-merge!(c, sav-first, sav-last, sav-value, del-first);
  if (sav-first == merge)
    merge-next
  else
    sav-first
  end
end method;

define method fold-if (c :: <if>) => (result :: false-or(<computation>))
  let tst = test(c);
  let test-value = type-estimate(c, tst);
  //actually, should also work for union(#t, <integer>) etc.
  if (instance?(test-value, <&singleton>))
    constant-fold-if(c, test-value.^singleton-object);
  elseif (^known-disjoint?(test-value, dylan-value(#"<boolean>")))
    constant-fold-if(c, #t);
  end if;
end method;

