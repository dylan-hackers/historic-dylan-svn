module: dfmc-typist

define generic lookup-type-node (o, env :: <type-environment>, #key abstract?) => (res :: <node>);

define constant abstract-and-lookup = rcurry(lookup-type-node, abstract?:, #t);

define method lookup-type-node (o :: <temporary>, env :: <type-environment>, #key abstract?) => (res :: <node>)
  element(env, o, default: #f) |
    begin
      let te = if (abstract?)
                 convert-to-typist-type(o.type-estimate-object, env)
               else
                 make(<&top-type>)
               end;
      let tv = make(<type-variable>, contents: te);
      debug-types(#"new-type-variable", env, tv, o, te);
      let n = make(<node>, graph: env.type-graph, value: tv);
      env[o] := n;
      unless (instance?(te, <&top-type>))
        add-constraint(env, o, lookup-type-node(te, env), n);
      end;
      n;
    end;
end;

define method lookup-type-node (o :: <multiple-value-temporary>, env :: <type-environment>, #key abstract?) => (res :: <node>)
  element(env, o, default: #f) |
    begin
      let tes = make(<simple-object-vector>);
      for (x from 0 below o.required-values)
        tes := add(tes, make(<&top-type>)); //actually estimate the objects!
      end;
      let top = make(<&top-type>);
      let tv = make(<type-variable>, contents: top);
      debug-types(#"new-type-variable", env, tv, o, top);
      let n = make(<node>, graph: env.type-graph, value: tv);
      env[o] := n;
      add-constraint(env, o, lookup-type-node(gen-tuple(map(rcurry(lookup-type-node, env), tes), rest?: o.rest-values?), env), n);
      n;
    end;
end;

define method lookup-type-node (t :: <lexical-required-type-variable>, env :: <type-environment>, #key abstract?)
 => (res :: <node>)
  assert(abstract? == #t);
  lookup-type-node(t.specializer, env, abstract?: abstract?)
end;

define method lookup-type-node (tv :: <&polymorphic-type-variable>, env :: <type-environment>, #key abstract?)
 => (res :: <node>)
  element(env, tv, default: #f) |
    begin
      let node = make(<node>, graph: env.type-graph, value: tv);
      //add constraint between ^type-variable-kind and node!
      env[tv] := node;
    end;
end;

define method lookup-type-node (o :: <object-reference>, env :: <type-environment>, #key abstract?) => (res :: <node>)
  element(env, o, default: #f) |
    begin
      let n = next-method();
      debug-types(#"type-relation", env, n, o);
      env[o] := n;
    end;
end;

define method lookup-type-node (o :: <object>, env :: <type-environment>, #key abstract?) => (res :: <node>)
  make(<node>, graph: env.type-graph,
       value: if (abstract?)
                convert-to-typist-type(o.type-estimate-object, env)
              else
                if (instance?(o, type-union(<typist-type>, <&type>, <&signature>)))
                  convert-to-typist-type(o, env)
                else
                  make(<&top-type>)
                end
              end);
end;

define generic type-estimate-object (o) => (res :: <&type>);

define method type-estimate-object (o) => (res :: <&type>)
  o.&object-class
end;

define method type-estimate-object (mb :: <module-binding>) => (te :: <&type>)
  let type = binding-type-model-object(mb, error-if-circular?: #f);
  if (type & instance?(type, <&type>))
    type
  elseif (type & instance?(type, <collection>))
    error("not sure what to do here...");
  else
    make(<&top-type>)
  end
end;

define method type-estimate-object (const :: <defined-constant-reference>) => (te :: <&type>)
  let mb = const.referenced-binding;
  if (mb & instance?(mb, <module-binding>))
    let type = type-estimate-object(mb);
    //try for a better type
    let mo = binding-model-object(mb, error-if-circular?: #f);
    if (mo)
      let type2 = type-estimate-object(mo);
      if (^subtype?(type2, type))
        type2
      else
        //can this happen?
        type
      end;
    else
      type
    end;
  else
    make(<&top-type>)
  end
end;

//this is <model-value> - (<string>, <vector>, <mapped-unbound>, <heap-deferred-model>)
define method type-estimate-object (o :: type-union(<&top>, <number>, <character>, <boolean>, <list>, <symbol>)) => (t :: <&type>)
  make(<&singleton>, object: o)
end;

//there's a mapping <heap-deferred-all-classes-model> => <simple-object-vector>
//but, <h-d-a-c-m> is not exported.
define method type-estimate-object (o :: <heap-deferred-model>) => (t :: <&type>)
  dylan-value(#"<simple-object-vector>")
end;

define method type-estimate-object (t :: <temporary>) => (t :: <&type>)
  make(<&top-type>)
end;

define method type-estimate-object (o :: <lexical-specialized-variable>)
 => (res :: <&type>)
  o.specializer | make(<&top-type>)
end;

define method type-estimate-object (o :: <lexical-rest-variable>)
 => (res :: <&type>)
  dylan-value(#"<simple-object-vector>")
end;

define method type-estimate-object (o :: <object-reference>)
 => (res :: <&type>)
  type-estimate-object(o.reference-value)
end;

define method type-estimate-object (t :: <&type>)
 => (res :: <&type>)
  if (t == dylan-value(#"<object>"))
    make(<&top-type>)
  elseif (t == dylan-value(#"<type>"))
    make(<&top-type>)
  else
    make(<&singleton>, object: t)
  end
end;

define method type-estimate-object (t :: <typist-type>)
 => (res :: <&type>)
  make(<&singleton>, object: t) //or call model-type here?
end;

define method type-estimate-object (v :: <vector>)
 => (res :: <&type>)
  if (v.size == 0)
    make(<&singleton>, object: v) //actually, (C <: <vector>, E)limited(C, of: E, size: 0) ?
  else
    let types = map(type-estimate-object, v);
    //#[ { 1 }, { 2 }, { 3 } ] -> #[<integer>, <integer>, <integer>] so type-union is useful
    //(might be better done in type-union?)
    let ctypes = map(method(x)
                       if (instance?(x, <&singleton>))
                         x.^singleton-object.&object-class
                       else
                         x
                       end
                     end, types);
    make(<&limited-collection-type>,
         class: v.&object-class,
         concrete-class: v.&object-class,
         element-type: reduce1(^type-union, ctypes))
  end
end;

define method type-estimate-object (s :: <string>)
 => (res :: <&type>)
  make(<&singleton>, object: s)
end;

define method type-estimate-object (l == &unbound)
 => (res :: <&type>)
  make(<&singleton>, object: &unbound)
end;

define generic convert-to-typist-type (t :: type-union(<typist-type>, <&signature>, <&type>), env :: <type-environment>)
 => (res :: type-union(<typist-type>, <&type>));

define method convert-to-typist-type (t :: <&type>, env :: <type-environment>)
 => (res :: <&type>)
  if (t == dylan-value(#"<object>"))
    make(<&top-type>)
  else
    t
  end
end;

define method convert-to-typist-type (t :: <typist-type>, env :: <type-environment>)
 => (res :: <typist-type>)
  t
end;

define method convert-to-typist-type (sig :: <&signature>, env :: <type-environment>)
 => (res :: <typist-type>)
  let args = map(rcurry(lookup-type-node, env), sig.^signature-required-arguments);
  let vals = map(rcurry(lookup-type-node, env), sig.^signature-required-values);
  make(<arrow>,
       arguments: lookup-type-node(gen-tuple(args, rest?: sig.^signature-rest?), env),
       values: lookup-type-node(gen-tuple(vals, rest?: sig.^signature-rest-value), env));
end;

define method convert-to-typist-type (lft :: <&limited-function-type>, env :: <type-environment>)
 => (res :: <typist-type>)
  convert-to-typist-type(lft.^function-signature, env)
end;

define method convert-to-typist-type (lc :: <&limited-collection-type>, env :: <type-environment>)
 => (res :: <typist-type>)
  let class = lookup-type-node(lc.^limited-collection-class, env);
  let etype = lookup-type-node(lc.^limited-collection-element-type, env);
  make(<limited-collection>, class: class, element-type: etype);
end;

define generic model-type (t :: type-union(<node>, <typist-type>, <&type>), #key top?)
 => (t :: type-union(<collection>, <&type>));

define method model-type (t :: <&type>, #key top?) => (t :: <&type>)
  if (top? & instance?(t, <&top-type>))
    dylan-value(#"<object>")
  else
    t
  end
end;

define method model-type (t :: <arrow>, #key top?) => (t :: <&limited-function-type>)
  let mt = rcurry(model-type, top?:, top?);
  let arg = t.arrow-arguments.mt;
  let val = t.arrow-values.mt;
  //doesn't respect #rest and keywords!
  make(<&limited-function-type>,
       signature: ^make(<&signature>,
                        required: arg, number-required: arg.size,
                        values: val, number-values: val.size));
end;

define method model-type (t :: <tuple>, #key top?) => (t :: <collection>)
  let mt = rcurry(model-type, top?:, top?);
  map(mt, t.tuple-types)
end;

define method model-type (t :: <tuple-with-rest>, #key top?) => (t :: <collection>)
  concatenate(next-method(), vector(if (top?) dylan-value(#"<object>") else make(<&top-type>) end))
end;

define method model-type (t :: <limited-collection>, #key top?) => (t :: <&type>)
  let mt = rcurry(model-type, top?:, top?);  
  make(<&limited-collection-type>,
       class: t.collection-class.mt,
       concrete-class: t.collection-class.mt,
       element-type: t.element-type.mt)
end;

define method model-type (t :: <node>, #key top?) => (res :: type-union(<collection>, <&type>))
  let mt = rcurry(model-type, top?:, top?);  
  t.find.node-value.mt
end;

define method model-type (t :: <type-variable>, #key top?) => (res :: <&type>)
  let mt = rcurry(model-type, top?:, top?);  
  t.type-variable-contents.mt
end;

define generic add-constraint
    (type-environment :: <type-environment>,
     origin :: type-union(<computation>, <constraint-edge>, <temporary>),
     left :: false-or(<node>), right :: false-or(<node>))
 => (c :: false-or(<constraint-edge>));

define method add-constraint
    (type-environment :: <type-environment>,
     origin :: type-union(<computation>, <constraint-edge>, <temporary>),
     left == #f, right :: <node>)
 => (c == #f);
  error("left was false");
end;

define method add-constraint
    (type-environment :: <type-environment>,
     origin :: type-union(<computation>, <constraint-edge>, <temporary>),
     left :: <node>, right == #f)
 => (c == #f);
  error("right was false");
end;

define method add-constraint
    (type-environment :: <type-environment>,
     origin :: type-union(<computation>, <constraint-edge>, <temporary>),
     left == #f, right == #f)
 => (c == #f);
  error("left and right were false");
end;

define method add-constraint
    (type-environment :: <type-environment>,
     origin :: type-union(<computation>, <constraint-edge>, <temporary>),
     left :: <node>, right :: <node>)
 => (c :: <constraint-edge>)
  unless (left.graph == right.graph)
    error("constraint must be in same type graph")
  end;
  make(<constraint-edge>, graph: type-environment.type-graph, origin: origin, source: left, target: right);
end;

define constant node-to-type = compose(model-type, node-value, find);
define constant node-to-model-type = compose(rcurry(model-type, top?:, #t), node-value, find);
define constant temporary-type = compose(node-to-type, lookup-type-node);
define constant temporary-type-abstraction = compose(node-to-type, abstract-and-lookup);

define method type-infer (l :: <&lambda>, type-env :: <type-environment>)
  //let type-env = l.type-environment;
  //unless (type-env)
  //  type-env := make(<type-environment>, lambda: l);
  //  l.type-environment := type-env;
  //end;
  if (l.type-environment & ~ any-te-matches?(l.type-environment, type-env))
    //merge those TEs?
    l.type-environment := type-env;
  end;
  do(rcurry(abstract-and-lookup, type-env), l.parameters);
  for (t in l.environment.temporaries)
    if (~empty?(t.assignments))
      maybe-add-variable-constraints(t, type-env);
    end;
  end;
  type-walk(type-env, l.body, #f);
  debug-types(#"highlight", type-env, 0);
  solve(type-env);
  //update values slot of signature
  let result-type = temporary-type(l.body.bind-return.computation-value, type-env);
  //might need to emit type check (top is super of everything!)
  if (~ instance?(result-type, <&top-type>)) //top-type is never more specific than some other type
    let res-type = lookup-type-node(l.body.bind-return.computation-value, type-env).find.node-value;
    let (res, rest?) = if (instance?(res-type, <tuple-with-rest>))
                         values(map(rcurry(model-type, top?:, #t), res-type.tuple-types), #t); //respect rest-type!
                       else
                         values(result-type, #f);
                       end;
    let sig = convert-type-to-signature(l.^function-signature, l.parameters, res, rest?);
    //also check congruency to generic!
    //also check with written down stuff: especially "=> ()"
    //or cases where fewer values are exposed than emitted (warn here!)
    if (signature-compatible?(l.^function-signature, sig))
      if (more-specific?(l.^function-signature, sig))
        l.^function-signature := sig;
      end;
    end;
  end;
  //type-env.finished-initial-typing? := #t;
end;

define function signature-compatible? (old-sig :: <&signature>, new-sig :: <&signature>)
 => (res :: <boolean>);
  //do this on a per-value basis... emitting multiple type checks / upgrade only some values
  //(<object>, <object>, <integer>) and (<integer>, <integer>, <object>)
  // ===> type-check(2, <integer>) & (<integer>, <integer>, <integer>)
  let old-vals = old-sig.^signature-required-values;
  let new-vals = new-sig.^signature-required-values;
  block(return)
    for (o in old-vals, n in new-vals)
      unless (^subtype?(n, o))
        //emit type check!
        return(#f);
      end;
    end;
    return(#t);
  end;
end;

define function more-specific? (sig1 :: <&signature>, sig2 :: <&signature>) => (result :: <boolean>)
  let val1 = sig1.^signature-required-values;
  let val2 = sig2.^signature-required-values;
  (val1.size < val2.size) | any?(complement(^type-equivalent?), val1, val2)
end;

define generic convert-type-to-signature
 (old :: false-or(<&signature>), params :: <collection>, res :: type-union(<collection>, <&type>), rest? :: <boolean>)
 => (signature :: <&signature>);

define inline function extract-params (p :: <collection>) => (key/value :: <list>)
  let rest? = (p.size > 0) & instance?(p.last, <lexical-rest-variable>);
  let off = (rest? & 1) | 0;
  let ps = map(specializer, copy-sequence(p, end: p.size - off));
  list(#"required", ps, #"number-required", ps.size, #"rest?", rest?)
end;

define method convert-type-to-signature (old-sig == #f, parameters :: <collection>, values :: <&type>, rest? :: <boolean>)
 => (signature :: <&signature>)
  let k/v = extract-params(parameters);
  apply(^make, <&signature>, values: vector(values), number-values: 1, rest-value?: rest?,
        rest-value: dylan-value(#"<object>"), k/v)
end;

define method convert-type-to-signature (old-sig == #f, parameters :: <collection>, values :: <collection>, rest? :: <boolean>)
 => (signature :: <&signature>)
  let k/v = extract-params(parameters);
  apply(^make, <&signature>, values: values, number-values: values.size, rest-value?: rest?,
        rest-value: dylan-value(#"<object>"), k/v);
end;

define inline function params-from-sig (sig :: <&signature>) => (k/v :: <list>)
  let rest? = sig.^signature-rest?;
  let ps = sig.^signature-required-arguments;
  let key? = sig.^signature-key?;
  let keys = key? & sig.^signature-keys;
  let key-types = key? & sig.^signature-key-types;
  let sealed? = sig.^signature-sealed-domain?;
  list(#"required", ps, #"number-required", ps.size, #"rest?", rest?,
       #"key?", key?, #"keys", keys, #"key-types", key-types,
       #"sealed-domain?", sealed?)
end;

define method convert-type-to-signature (old-sig :: <&signature>, parameters :: <collection>, values :: <&type>, rest? :: <boolean>)
 => (signature :: <&signature>)
  //we can also upgrade <&signature> -> <&polymorphic-signature>
  apply(^make, <&signature>, values: vector(values), number-values: 1, rest-value?: rest?,
        rest-value: dylan-value(#"<object>"), params-from-sig(old-sig))
end;

define method convert-type-to-signature (old-sig :: <&signature>, parameters :: <collection>, values :: <collection>, rest? :: <boolean>)
 => (signature :: <&signature>)
  //we can also upgrade <&signature> -> <&polymorphic-signature>
  apply(^make, <&signature>, values: values, number-values: values.size, rest-value?: rest?,
        rest-value: dylan-value(#"<object>"), params-from-sig(old-sig))
end;

define method convert-type-to-signature (old-sig :: <&polymorphic-signature>, parameters :: <collection>, values :: <&type>, rest? :: <boolean>)
 => (signature :: <&signature>)
  //or add/remove type variables...
  apply(^make, <&signature>, values: vector(values), number-values: 1,
        rest-value?: rest?, rest-value: dylan-value(#"<object>"),
        type-variables: old-sig.^signature-type-variables, params-from-sig(old-sig.^real-signature))
end;

define method convert-type-to-signature (old-sig :: <&polymorphic-signature>, parameters :: <collection>, values :: <collection>, rest? :: <boolean>)
 => (signature :: <&signature>)
  //or add/remove type variables...
  apply(^make, <&signature>, values: values, number-values: values.size,
        rest-value?: rest?, rest-value: dylan-value(#"<object>"),
        type-variables: old-sig.^signature-type-variables, params-from-sig(old-sig.^real-signature))
end;

define generic maybe-add-variable-constraints (t :: <temporary>, env :: <type-environment>) => ();

define method maybe-add-variable-constraints (t :: <temporary>, env :: <type-environment>) => ()
end;

define method maybe-add-variable-constraints (t :: <lexical-specialized-variable>, env :: <type-environment>) => ()
  let spec = t.specializer;
  if (spec) //spec for type-union(<integer>, <string>) is #f :[
    let spec-node = lookup-type-node(spec, env);
    //and not <object>?
    for (ass in t.assignments) //these are <temporary-transfer> in current representation (SSA-converted)
      add-constraint(env, t, spec-node, abstract-and-lookup(ass.temporary, env));
    end;
  end;
end;

define generic type-walk(env :: <type-environment>, c :: <computation>, last :: false-or(<computation>)) => ();

define constant next-type-step =
    method(env, c, last)
      c.next-computation & type-walk(env, c.next-computation, last)
    end;

define function any-te-matches? (te :: <type-environment>, te2 :: <type-environment>) => (match? :: <boolean>)
  te == te2 | (te.outer-environment & any-te-matches?(te.outer-environment, te2))
end;

define function set-type-environment! (c :: <computation>, t :: <type-environment>) => ()
  if (slot-initialized?(c, type-environment) & c.type-environment)
    if (c.type-environment == t)
      //pass
    elseif (any-te-matches?(c.type-environment, t))
      //widen type environment? is unsafe, should not happen!
      c.type-environment := t;
    elseif (any-te-matches?(t, c.type-environment))
      //t is more specific than c.t-e, so its safe to set
      c.type-environment := t;
    else
      //disjoint, should not happen!
      c.type-environment := t;
    end;
  else
    c.type-environment := t;
  end;
end;

define method type-walk (env :: <type-environment>, c :: <computation>, last :: false-or(<computation>)) => ()
  if (c ~== last)
    set-type-environment!(c, env);
    c.infer-computation-types;
    next-type-step(env, c, last);
  end;
end;

define method type-walk (env :: <type-environment>, c :: <if>, last :: false-or(<computation>)) => ()
  if (c ~== last)
    set-type-environment!(c, env);
    debug-types(#"beginning", env, list("inferring", c));
    debug-types(#"highlight", env, c);
    solve(env);
    c.infer-computation-types; //actually, needs both envs for proper inference
    let fold = c.fold-if;
    if (fold)
      type-walk(env, fold, last);
    else
      local method get-te (comp :: <computation>) => (result :: <type-environment>)
              if (comp == c.next-computation)
                env
              elseif (slot-initialized?(comp, type-environment) & comp.type-environment)
                if (c.type-environment == env) //outer env was more specific, use it
                  comp.type-environment.outer-environment := env; //actually, should re-type all members of comp.t-e.r-e!
                end;
                comp.type-environment
              else
                make(<type-environment>, outer: env)
              end;
            end;
      let con-env = get-te(c.consequent);
      type-walk(con-env, c.consequent, c.next-computation);
      con-env.finished-initial-typing? := #t;
      solve(con-env);
      let alt-env = get-te(c.alternative);
      type-walk(alt-env, c.alternative, c.next-computation);
      alt-env.finished-initial-typing? := #t;
      solve(alt-env);
      next-type-step(env, c, last);
    end;
  end;
end;

define method type-walk (env :: <type-environment>, c :: <bind-exit>, last :: false-or(<computation>)) => ()
  if (c ~== last)
    set-type-environment!(c, env);
    solve(env);
    c.infer-computation-types;
    type-walk(env, c.body, c.next-computation);
    next-type-step(env, c, last);
  end;
end;

define method type-walk (env :: <type-environment>, c :: <loop>, last :: false-or(<computation>)) => ()
  if (c ~== last)
    set-type-environment!(c, env);
    c.infer-computation-types;
    type-walk(env, c.loop-body, c.next-computation);
    next-type-step(env, c, last);
  end;
end;

define method type-walk (env :: <type-environment>, c :: <unwind-protect>, last :: false-or(<computation>)) => ()
  if (c ~== last)
    set-type-environment!(c, env);
    c.infer-computation-types;
    type-walk(env, c.body, c.next-computation);
    type-walk(env, c.cleanups, c.next-computation);
    next-type-step(env, c, last);
  end;
end;

define method type-walk (env :: <type-environment>, c :: <make-closure>, last :: false-or(<computation>)) => ()
  if (c ~== last)
    set-type-environment!(c, env);
    type-infer(c.computation-closure-method, env);
    c.infer-computation-types;
    next-type-step(env, c, last);
  end;
end;

define generic infer-computation-types (c :: <computation>) => ();

define macro type-rule-definer
 { define type-rule ?computation:expression ?body:* end }
 => { define method infer-computation-types (c :: ?computation) => ()
        let type-env = c.type-environment;
        debug-types(#"beginning", type-env, list("inferring", c));
        debug-types(#"highlight", type-env, c);
        let ?=abstract = rcurry(abstract-and-lookup, type-env);
        let ?=constraint = curry(add-constraint, type-env, c);
        let ?=temporary-node = c.temporary & ?=abstract(c.temporary);
        let ?=computation = c;
        let ?=type-env = c.type-environment;
        let ?=estimate = rcurry(temporary-type, type-env);
        let ?=lookup = rcurry(lookup-type-node, type-env);
        ?body
      end; }
end;

define method infer-computation-types (c :: <computation>) => ()
  let type-env = c.type-environment;
  debug-types(#"beginning", type-env, list("inferring", c));
  debug-types(#"highlight", type-env, c);
  c.temporary & abstract-and-lookup(c.temporary, c.type-environment);
end;

define type-rule <variable-reference>
  constraint(computation.referenced-binding.abstract, temporary-node);
end;

define type-rule <temporary-transfer-computation>
  constraint(computation.computation-value.abstract, temporary-node);
end;

define type-rule <values>
  constraint(gen-tuple(map(abstract, computation.fixed-values), rest?: computation.rest-value).lookup,
             temporary-node);
end;

define generic typist-union
 (env :: <type-environment>,
  t1 :: type-union(<collection>, <&type>),
  t2 :: type-union(<collection>, <&type>)) => (union :: <node>);

define method typist-union (env :: <type-environment>, t1 :: <&type>, t2 :: <&type>) => (union :: <node>)
  lookup-type-node(^type-union(t1, t2), env)
end;

define method typist-union (env :: <type-environment>, t1 :: <collection>, t2 :: <collection>) => (union :: <node>)
  lookup-type-node(map(curry(typist-union, env), t1, t2).gen-tuple, env)
end;

define method typist-union (env :: <type-environment>, t1 :: type-union(<collection>, <&type>), t2 :: type-union(<collection>, <&type>))
 => (union :: <node>)
  //if (^instance?(t1, <&top-type>) | ^instance?(t2, <&top-type>) | ^instance?(t1, <collection>) | ^instance?(t2, <collection>))
    lookup-type-node(make(<&top-type>), env);
  //else
end;

define type-rule <merge>
  local method find-te (t :: <value-reference>) => (res :: <type-environment>)
          if (instance?(t, <temporary>) & t.generator & slot-initialized?(t.generator, type-environment) & t.generator.type-environment)
            t.generator.type-environment
          else
            type-env
          end
        end;
  if (computation.merge-left-value & computation.merge-right-value)
    let left-te = find-te(computation.merge-left-value);
    let right-te = find-te(computation.merge-right-value);
    solve(left-te);
    solve(right-te);
    constraint(typist-union(type-env,
                            temporary-type-abstraction(computation.merge-left-value, left-te),
                            temporary-type-abstraction(computation.merge-right-value, right-te)),
               temporary-node);
  elseif (computation.merge-left-value | computation.merge-right-value)
    let v = computation.merge-left-value | computation.merge-right-value;
    constraint(temporary-type-abstraction(v, v.find-te).lookup, temporary-node);
  end;
end;

define method extract-parameter-type (c :: <merge>)
  c.merge-left-value
end;

define method extract-argument-type (c :: <merge>)
  c.merge-right-value
end;

define method infer-computation-types (c :: <loop>) => ()
  next-method();
  let out-env = c.type-environment;
  //in order to get types for parameter types, we have to solve the type graph
  solve(out-env);

  let types = make(<stretchy-vector>);
  for (node = c.loop-body then node.next-computation,
       while: instance?(node, type-union(<loop-merge>, <phi-node>)))
    let type = temporary-type(node.extract-parameter-type, out-env);
    if (instance?(type, <&singleton>)) 
      //inferring a singleton as a loop variable makes no sense
      // because a loop variable is modified/assigned and won't
      // stay constant
      type := type.^singleton-object.&object-class;
    end;
    add!(types, type);
  end;

  if (types.size > 0)
    //copy loop-body for further investigation (find fixpoint by doing
    //inference once with outer type)
    let copier = make(<dfm-copier>);
    //let cfg-first = deep-copy(copier, c.loop-body);
    let cfg-first = #f;
    let temps = make(<stretchy-vector>);
    let c-env = make(<type-environment>, outer: out-env);
    walk-computations(method(old)
                        local method maybe-add-temp (old-t, new-t)
                                if (element(out-env, old-t, default: #f))
                                  if (~ instance?(temporary-type(old-t, out-env), <&top-type>))
                                    add!(temps, pair(new-t, temporary-type(old-t, out-env)));
                                  end;
                                end;
                              end;
                        let new = deep-copy(copier, old);
                        //new.type-environment := c-env;
                        unless (cfg-first) cfg-first := new end;
                        for (tmp in old.used-temporary-accessors)
                          let getter = temporary-getter(tmp);
                          let old-tmp = getter(old);
                          let new-tmp = getter(new);
                          if (instance?(old-tmp, <sequence>))
                            for (old* in old-tmp, new* in new-tmp)
                              maybe-add-temp(old*, new*);
                            end;
                          else
                            maybe-add-temp(old-tmp, new-tmp);
                          end;
                        end;
                        new.computation-id;
                      end, c.loop-body, #f);

    //insert types of temporaries into environment
    do(method(x)
         let tv = abstract-and-lookup(x.head, c-env);
         add-constraint(c-env, cfg-first, lookup-type-node(x.tail, c-env), tv);
       end, temps);

    let phis = make(<stretchy-vector>);
    //collect phi and loop-merge nodes
    for (type in types, node = cfg-first then node.next-computation)
      //assign "outer" types to temporaries
      node.type-environment := c-env;
      let tv = abstract-and-lookup(node.temporary, c-env);
      add-constraint(c-env, node, lookup-type-node(type, c-env), tv);
      add!(phis, node);
    end;
    //solve to assign types for phi-temporaries [will be done during <call> inference]

    //infer body with those assigned type variables
    type-walk(c-env, phis.last.next-computation, #f);

    //solve constraint system!
    solve(c-env);

    //check whether outer and inner types are equal
    // (or subtypes - since GF protocol must not be violated)
    let safe?
      = block(fast-exit)
          for (phi in phis, type in types)
            let inner-t = phi.extract-argument-type;
            let gen = inner-t.generator;
            let inner-type = temporary-type(inner-t, gen & gen.type-environment | c-env);
            unless (^subtype?(inner-type, type))
              fast-exit(#f)
            end;
          end;
          #t
        end;

    //if safe, assign types to phi / loop-merge temporaries
    unless (safe?)
      //empty types collection
      types.size := 0;
    end;
    //remove computations and type graph nodes

    //add type constraint to opposite DF node of phi/loop-merge
    //(is then propagated to temporary of phi by respective inference rule)
    for (node = c.loop-body then node.next-computation, type in types)
      add-constraint(out-env, node, lookup-type-node(type, out-env), abstract-and-lookup(node.extract-argument-type, out-env));
    end;
  end;
end;

define type-rule <if>
  //if test has occurence typing, assign type to temporar(y/ies) involved
  //into alternative and consequent environments!
end;

define type-rule <stack-vector>
  //constraint(map(abstract, computation.arguments).gen-tuple.lookup, temporary-node);
  constraint(dylan-value(#"<simple-object-vector>").lookup, temporary-node);
end;

define type-rule <make-closure>
  //or computation-signature-value? - is #f when sig is statically known
  constraint(computation.computation-closure-method.^function-signature.lookup, temporary-node);
end;

define type-rule <slot-value>
  constraint(computation.computation-slot-descriptor.^slot-type.lookup, temporary-node);
end;

define type-rule <repeated-slot-value>
  constraint(computation.computation-slot-descriptor.^slot-type.repeated-representation.lookup, temporary-node);
end;

define type-rule <slot-value-setter>
  let new-val = computation.computation-new-value.abstract;
  let new-val-t = new-val.model-type;
  unless (instance?(new-val-t, <&singleton>) & (new-val-t.^singleton-object == &unbound) & instance?(computation.environment.lambda, <&initializer-method>))
    constraint(new-val, temporary-node);
    constraint(computation.computation-slot-descriptor.^slot-type.lookup, temporary-node);
  end;
end;

define type-rule <repeated-slot-value-setter>
  let new-val = computation.computation-new-value.abstract;
  let new-val-t = new-val.model-type;
  unless (instance?(new-val-t, <&singleton>) & (new-val-t.^singleton-object == &unbound) & instance?(computation.environment.lambda, <&initializer-method>))
    constraint(new-val, temporary-node);
    constraint(computation.computation-slot-descriptor.^slot-type.repeated-representation.lookup, temporary-node);
  end;
end;

define type-rule <extract-single-value>
  solve(type-env);
  let tt = computation.computation-value.estimate;
  if (instance?(tt, <collection>) & (tt.size > computation.index))
    constraint(tt[computation.index].lookup, temporary-node)
  end
end;

define type-rule <extract-rest-value>
  constraint(make(<&top-type>).lookup, temporary-node);
end;

define type-rule <make-cell>
  constraint(computation.computation-value.lookup, temporary-node);
  constraint(computation.temporary.cell-type.lookup, temporary-node);
end;

define type-rule <get-cell-value>
  constraint(computation.computation-cell.abstract, temporary-node);
end;

define type-rule <set-cell-value!>
  constraint(computation.computation-value.abstract, temporary-node);
  constraint(computation.computation-cell.abstract, temporary-node);
end;

define type-rule <set!>
  if (instance?(computation.assigned-binding, <module-binding>))
    constraint(computation.computation-value.abstract, temporary-node);
    constraint(computation.assigned-binding.abstract, temporary-node);
  else
    error("assignment is not possible!");
  end
end;

//define method infer-computation-types (c :: <adjust-multiple-values>) => ()
//end;

//define method infer-computation-types (c :: <adjust-multiple-values-rest>) => ()
//end;

define method infer-computation-types
 (c :: type-union(<redefinition>, <conditional-update!>, <type-redefinition>)) => ()
  error("didn't expect %=", c);
end;
define generic get-function-object (o :: <object>)
 => (res :: false-or(type-union(<&limited-function-type>, <&function>)));

define method get-function-object (o :: <object>) => (res == #f)
  //format-out("can't get function object of an <object>\n");
  #f;
end;

define method get-function-object (t :: <temporary>)
 => (f :: false-or(type-union(<&limited-function-type>, <&function>)))
  let gen = t.generator;
  if (instance?(gen, <make-closure>))
    gen.computation-closure-method;
  elseif (instance?(gen, <extract-single-value>))
    block()
      let res = temporary-type(gen.computation-value, gen.type-environment)[gen.index];
      instance?(res, <&limited-function-type>) & res
    exception (e :: <condition>)
      #f
    end
  elseif (instance?(gen, <call>))
    let res = temporary-type(gen.temporary, gen.type-environment);
    instance?(res, <&limited-function-type>) & res
  elseif (instance?(gen, <temporary-transfer>))
    get-function-object(gen.computation-value)
  end;
end;

define method get-function-object (t :: <lexical-specialized-variable>)
 => (f :: false-or(<&limited-function-type>))
  instance?(t.specializer, <&limited-function-type>) & t.specializer;
end;

define method get-function-object (t :: <object-reference>)
 => (f :: false-or(type-union(<&limited-function-type>, <&function>)))
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
  //next-method(); -- otherwise, we end up with more type variables than needed in
  //the type environment (c.temporary!, which is suspect to change)
  let type-env = c.type-environment;
  debug-types(#"beginning", type-env, list("inferring", c));
  debug-types(#"highlight", type-env, c);
  let fun = c.function.get-function-object;
  infer-function-type(c, fun);
end;

define method infer-computation-types (c :: <primitive-call>) => ()
  next-method();
  let fn = c.primitive;
  let s = fn.primitive-signature;
  if (fn == dylan-value(#"primitive-object-allocate-filled"))
    // type returned is actually contained in second argument
    let (c?, wrapper) = constant-value?(second(arguments(c)));
    when (c?)
      let iclass = ^mm-wrapper-implementation-class(wrapper);
      let class  = ^iclass-class(iclass);
      let req = s.^signature-required-arguments;
      create-arrow-and-constraint(c, make(<&signature>, number-required: req.size, required: req,
                                          number-values: 1, values: vector(class)));
    end;
  else
    create-arrow-and-constraint(c, s);
  end;
end;

define method infer-function-type (c :: <function-call>, fun == #f) => ()
  //well, need to generate an arrow-type, args and vals, constraint...
  create-arrow-and-constraint(c, make(<&signature>, rest-value?: #t, rest?: #t, values: #[], number-values: 0, required: #[], number-required: 0));
end;

define method infer-function-type (c :: <function-call>, fun :: <&limited-function-type>) => ()
  create-arrow-and-constraint(c, fun.^function-signature);
end;

define function gen-tuple (types :: <collection>, #key rest?) => (res :: <tuple>)
  make(if (rest?) <tuple-with-rest> else <tuple> end, tuples: types)
end;

define function create-arrow-and-constraint
 (c :: <call>, sig :: <&signature>) => ()
  let tenv = c.type-environment;
  let left = lookup-type-node(sig, tenv);

  let args = map(rcurry(abstract-and-lookup, tenv), c.arguments);

  let vals
    = if (c.temporary)
        let res = abstract-and-lookup(c.temporary, tenv);
        if (~instance?(c.temporary, <multiple-value-temporary>))
          lookup-type-node(vector(res).gen-tuple, tenv)
        else
          res
        end;
      else
        lookup-type-node(make(<&top-type>), tenv)
      end;

  let right = make(<node>, graph: tenv.type-graph,
                   value: make(<arrow>,
                               arguments: lookup-type-node(args.gen-tuple, tenv),
                               values: vals));
  debug-types(#"type-relation", tenv, right, c);
  add-constraint(tenv, c, left, right);
end;

define method upgrade-types (l)
end;

define method upgrade-types (l :: <&lambda>)
  for (t in l.^function-signature.^signature-required-arguments, p in l.parameters)
    unless (^type-equivalent?(t, p.specializer))
      p.specializer := t;
      //actually, add a constraint for given specializer,
      //but not yet in correct context (<graph>-wise)
      // actually, *constraint* get bound to #[] in type-infer, thus modifying constraints
      // does not make sense yet
      remove-key!(l.type-environment.real-environment, p);
    end;
  end;
  type-infer(l, l.type-environment);
end;

define function constrain-type-variables
    (c :: <function-call>, sig-args :: <collection>, real-args :: <collection>)
  for (s in sig-args, r in real-args)
    add-constraint(c.type-environment, c, s, r);
  end;
end;

define method infer-function-type (c :: <function-call>, fun :: <&function>) => ()
  //keyword-arguments!
  let sig = ^function-signature(fun);
  let tenv = c.type-environment;
  unless (c.environment.lambda == fun) //updating self-calls is not wise (or, is it?)
    if (instance?(sig, <&polymorphic-signature>))
      //XXX: copy me if not single user!
      solve(tenv);
      //initial-type-constraints(sig);
      constrain-type-variables(c, map(rcurry(lookup-type-node, tenv), sig.^signature-required-arguments),
                               map(rcurry(abstract-and-lookup, tenv), c.arguments));
      let progress? = #t;
      while (progress?)
        let old-types = map(rcurry(temporary-type, tenv), sig.^signature-type-variables);
        let old-argument-types = map(rcurry(temporary-type-abstraction, tenv), c.arguments);
        solve(tenv);
        let new-types = map(rcurry(temporary-type, tenv), sig.^signature-type-variables);
        let new-argument-types = map(rcurry(temporary-type-abstraction, tenv), c.arguments);
        let opt = make(<stretchy-vector>);
        for (o in old-argument-types, n in new-argument-types, a in c.arguments)
          if (~ ^type-equivalent?(o.model-type, n.model-type))
            unless(member?(n, opt))
              add!(opt, pair(a, n))
            end;
          end;
        end;
        for (p in opt)
          let type = p.tail;
          let closure = p.head.get-function-object;
          if (closure)
            closure.^function-signature := type.^function-signature; //assert type is a <l-f-t>
            closure.upgrade-types;
            //update types for p.head
            infer-computation-types(p.head.generator);
          end;
        end;
        solve(tenv);
        let newer-types = map(rcurry(temporary-type, tenv), sig.^signature-type-variables);
        progress? := #f;
        for (n in new-types, n2 in newer-types)
          if (~ ^type-equivalent?(n, n2))
            progress? := #t;
          end;
        end;
      end;
      //upgrade signature (using type variable types)
      let arg = map(rcurry(temporary-type, tenv), sig.^signature-required-arguments);
      let val = map(rcurry(temporary-type, tenv), sig.^signature-required-values);
      fun.^function-signature := make(<&signature>, required:  arg, number-required: arg.size, values: val, number-values: val.size);
      let tv-types = map(rcurry(temporary-type, tenv), sig.^signature-type-variables);
      for (tv in sig.^signature-type-variables, tv-type in tv-types)
        add-constraint(tenv, c, lookup-type-node(tv, tenv), lookup-type-node(tv-type, tenv));
      end;
      solve(tenv);
    end;
  end;
  create-arrow-and-constraint(c, fun.^function-signature);
end;

define method infer-function-type (c :: <simple-call>, gf :: <&generic-function>) => ()
  let folded? = #f;
  let (function-constant?, function) = constant-value?(function(c));
  if (function-constant? & every?(constant-value?, c.arguments))
    let compile-stage-function = lookup-compile-stage-function(function);
    if (compile-stage-function)
      folded? := maybe-fold-function-call(c, c.temporary,
                                          compile-stage-function,
                                          c.arguments);
    end;
  end;

  unless(folded?)
    //simple strategy here:
    // first, solve the type graph (at least the partial graph we have so far)
    //this might conflict with the general idea of the type graph (error narrowing),
    //but recording the types of the GF in the constraint is too generic
    solve(c.type-environment);

    // then, try to upgrade the GF call to a simple call (narrowing result type)
    let arguments = map(compose(node-to-model-type, rcurry(abstract-and-lookup, c.type-environment)),
                        c.arguments);
    let effs = estimate-effective-methods(gf, arguments, c);
    if (~empty?(effs) & maybe-upgrade-gf-to-method-call(c, gf, arguments, effs))
      // finally, record type constraint (beta = tau1 -> tau2)
      //well, this will done by the changed call-graph - walk-computations does this
    else
      //generate specialized dispatch code, if number of dispatch args is
      //low on all effs (<integer>, <string>) vs (<boolean>, <string>)
      //only need to dispatch on first argument
    
      // finally, record type constraint (beta = tau1 -> tau2)
      next-method();
    end;
  end;
end;
