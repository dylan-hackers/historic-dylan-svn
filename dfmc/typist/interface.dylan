module: dfmc-typist


define function type-estimate (context :: <computation>, o :: <object>)
 => (te :: type-union(<collection>, <&type>))
  block()
    solve(context.type-environment);
    let node = element(context.type-environment, o, default: #f);
    if (node)
      node.node-to-type
    else 
      o.type-estimate-object
    end;
  exception (e :: <condition>)
    dynamic-bind(*typist-visualize* = #f)
      o.type-estimate-object
    end;
  end;
end;

define compiler-sideways method re-optimize-type-estimate (c :: <computation>) => ()
end method;

define compiler-sideways method re-type-computations
    (env :: <type-environment>, first :: false-or(<computation>), last :: false-or(<computation>)) => ()
  if (env.finished-initial-typing? & first)
    type-walk(env, first, last.next-computation);

    //let infer = make(<stretchy-vector>);
    //walk-computations(curry(add!, infer), first, last.next-computation);
    ////may change during inference! (the next-computation pointers)
    //do(infer-computation-types, infer);
  end;
end;

define constant guaranteed-disjoint? = ^known-disjoint?;
