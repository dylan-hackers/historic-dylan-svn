module: dfmc-typist


define function type-estimate (c :: <computation>, o :: <object>)
 => (te :: type-union(<collection>, <&type>))
  block()
    let context = c.type-environment;
    solve(context);
    let node = element(context, o, default: #f);
    if (node)
      node.node-to-model-type
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
  dynamic-bind(*upgrade?* = #f)
    infer-computation-types(c);
  end
end method;

define compiler-sideways method re-optimize-type-estimate (c :: <make-cell>) => ()
  next-method();
  maybe-upgrade-cell(c.temporary);
end;

define compiler-sideways method re-optimize-type-estimate (c :: <set-cell-value!>) => ()
  next-method();
  maybe-upgrade-cell(c.computation-cell);
end;

define compiler-sideways method re-optimize-type-estimate (c :: <loop>) => ()
end method;

define function maybe-upgrade-cell (c :: <cell>) => ()
  if (c.finished-conversion?)
    let gens = pair(c.generator, choose(rcurry(instance?, <set-cell-value!>), c.users));
    if (every?(type-environment, gens))
      let cell-t = apply(^type-union, map(method(x) type-estimate(x, x.computation-value) end, gens));
      if (instance?(cell-t, <&top-type>)) cell-t := dylan-value(#"<object>") end; //XXX: this should go away!
      if (^subtype?(cell-t.cell-representation, c.cell-type.cell-representation))
        unless (^subtype?(c.cell-type.cell-representation, cell-t.cell-representation))
          c.cell-type := cell-t;
          re-optimize-users(c);
        end;
      else
        error("this shouldn't happen!");
      end
    end;
  end;
end;

//define compiler-sideways method re-optimize-type-estimate (c :: <merge>) => ()
//  infer-computation-types(c);
//end;

define thread variable *upgrade?* :: <boolean> = #t;

define compiler-sideways method re-type-computations
    (env :: <type-environment>, first :: false-or(<computation>), last :: false-or(<computation>)) => ()
  if (env.finished-initial-typing? & first)
    dynamic-bind(*upgrade?* = #f)
      type-walk(env, first, last.next-computation);
    end;

    //let infer = make(<stretchy-vector>);
    //walk-computations(curry(add!, infer), first, last.next-computation);
    ////may change during inference! (the next-computation pointers)
    //do(infer-computation-types, infer);
  end;
end;

define sideways function initialize-type-environment!
 (env :: <type-environment>, first :: false-or(<computation>), last :: false-or(<computation>)) => ()
  type-walk(env, first, #f, infer?: #f);
  walk-computations(method(x)
                      if (x.computation-type)
                        //x.type-environment.real-environment[x.temporary] := make(<node>, 
                        //no constraints here, please!
                        lookup-type-node(x.temporary, x.type-environment, type: x.computation-type);
                      else //spliced extract or adjust values
                        dynamic-bind(*upgrade?* = #f)
                          unless (instance?(x, <loop>)) //do not upgrade loop (for now)
                            x.infer-computation-types
                          end
                        end
                      end
                    end,
                    first, #f);
end;
define constant guaranteed-disjoint? = ^known-disjoint?;
