module: dfmc-typist

define constant $lambda-type-caches :: <table> = make(<table>);

define constant <type-environment> = <table>;

define thread variable *type-environment* :: false-or(<type-environment>) = #f;

define generic lookup-type-node (o, #key abstract?) => (res :: <node>);

define constant abstract-and-lookup = rcurry(lookup-type-node, abstract?:, #t);

define method lookup-type-node (o :: <temporary>, #key abstract?) => (res :: <node>)
  element(*type-environment*, o, default: #f) |
    begin
      let te = if (abstract?)
                 o.type-estimate-object.convert-to-typist-type
               else
                 make(<&top-type>)
               end;
      let tv = make(<type-variable>, contents: te);
      debug-types(#"new-type-variable", tv, o, te);
      let n = make(<node>, graph: *graph*, value: tv);
      *type-environment*[o] := n;
      unless (instance?(te, <&top-type>))
        add-constraint(make(<equality-constraint>, origin: o,
                            left: te.lookup-type-node, right: n));
      end;
      n;
    end;
end;

define method lookup-type-node (o :: <multiple-value-temporary>, #key abstract?) => (res :: <node>)
  element(*type-environment*, o, default: #f) |
    begin
      let tes = make(<simple-object-vector>);
      for (x from 0 below o.required-values)
        tes := add(tes, make(<&top-type>)); //actually estimate the objects!
      end;
      let top = make(<&top-type>);
      let tv = make(<type-variable>, contents: top);
      debug-types(#"new-type-variable", tv, o, top);
      let n = make(<node>, graph: *graph*, value: tv);
      *type-environment*[o] := n;
      add-constraint(make(<equality-constraint>, origin: o,
                          left: gen-tuple(map(lookup-type-node, tes), rest?: o.rest-values?),
                          right: n));
      n;
    end;
end;

define method lookup-type-node (t :: <lexical-required-type-variable>, #key abstract?)
 => (res :: <node>)
  assert(abstract? == #t);
  lookup-type-node(t.specializer, abstract?: abstract?)
end;

define method lookup-type-node (tv :: <&polymorphic-type-variable>, #key abstract?)
 => (res :: <node>)
  element(*type-environment*, tv, default: #f) |
    begin
      let node = make(<node>, graph: *graph*, value: tv);
      //add constraint between ^type-variable-kind and node!
      *type-environment*[tv] := node;
    end;
end;

define method lookup-type-node (o :: <object-reference>, #key abstract?) => (res :: <node>)
  element(*type-environment*, o, default: #f) |
    begin
      let n = next-method();
      debug-types(#"type-relation", n, o);
      *type-environment*[o] := n;
    end;
end;

define method lookup-type-node (o :: <object>, #key abstract?) => (res :: <node>)
  make(<node>, graph: *graph*,
       value: if (abstract?)
                o.type-estimate-object.convert-to-typist-type
              else
                if (instance?(o, type-union(<typist-type>, <&type>)))
                  o.convert-to-typist-type
                else
                  make(<&top-type>)
                end
              end);
end;

define generic type-estimate-object (o) => (res :: type-union(<typist-type>, <&type>));

define method type-estimate-object (o) => (res :: <&type>)
  o.&object-class
end;

define method type-estimate-object (mb :: <module-binding>) => (te :: <&type>)
  let type = binding-type-model-object(mb, error-if-circular?: #f);
  if (type & instance?(type, type-union(<collection>, <&type>)))
    type
  else
    make(<&top-type>)
  end
end;

define method type-estimate-object (const :: <defined-constant-reference>) => (te :: <&type>)
  let mb = const.referenced-binding;
  if (mb & instance?(mb, <module-binding>))
    type-estimate-object(mb)
  else
    make(<&top-type>)
  end
end;

//this is <model-value> - (<string>, <vector>, <mapped-unbound>)
define method type-estimate-object (o :: type-union(<&top>, <heap-deferred-model>, <number>, <character>, <boolean>, <list>, <symbol>)) => (t :: <&type>)
  make(<&singleton>, object: o)
end;

define method type-estimate-object (t :: <temporary>) => (t :: <&type>)
  make(<&top-type>)
end;

define method type-estimate-object (o :: <lexical-specialized-variable>)
 => (res :: type-union(<typist-type>, <&type>))
  o.specializer & convert-to-typist-type(o.specializer) | make(<&top-type>)
end;

define method type-estimate-object (o :: <object-reference>)
 => (res :: type-union(<typist-type>, <&type>))
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
 => (res :: type-union(<typist-type>, <&type>))
  if (v.size == 0)
    make(<&singleton>, object: v) //actually, (C <: <vector>, E)limited(C, of: E, size: 0) ?
  else
    let types = map(type-estimate-object, v);
    let ctypes = map(method(x) if (instance?(x, <&singleton>)) x.^singleton-object.&object-class else x end end,
                     types);
    make(<limited-collection>,
         class: make(<node>, graph: *graph*, value: dylan-value(#"<vector>")), //more like subclass(<vector>)
         element-type: reduce1(^type-union, ctypes).lookup-type-node)
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

define generic convert-to-typist-type (t :: type-union(<typist-type>, <&type>))
 => (res :: type-union(<typist-type>, <&type>));

define method convert-to-typist-type (t :: <&type>)
 => (res :: <&type>)
  if (t == dylan-value(#"<object>"))
    make(<&top-type>)
  else
    t
  end
end;

define method convert-to-typist-type (t :: <typist-type>)
 => (res :: <typist-type>)
  t
end;

define method convert-to-typist-type (lft :: <&limited-function-type>)
 => (res :: <typist-type>)
  let sig = lft.^function-signature;
  let args = map(lookup-type-node, sig.^signature-required-arguments);
  let vals = map(lookup-type-node, sig.^signature-required-values);
  make(<arrow>, arguments: gen-tuple(args, rest?: sig.^signature-rest?),
       values: gen-tuple(vals, rest?: sig.^signature-rest-value));
end;

define method convert-to-typist-type (lc :: <&limited-collection-type>)
 => (res :: <typist-type>)
  let class = lc.^limited-collection-class.lookup-type-node;
  let etype = lc.^limited-collection-element-type.lookup-type-node;
  make(<limited-collection>, class: class, element-type: etype);
end;

define generic model-type (t :: type-union(<node>, <typist-type>, <&type>))
 => (t :: type-union(<collection>, <&type>));

define method model-type (t :: <&type>) => (t :: <&type>)
  t
end;

define method model-type (t :: <arrow>) => (t :: <&limited-function-type>)
  let arg = t.arrow-arguments.model-type;
  let val = t.arrow-values.model-type;
  //doesn't respect #rest and keywords!
  make(<&limited-function-type>,
       signature: ^make(<&signature>,
                        required: arg, number-required: arg.size,
                        values: val, number-values: val.size));
end;

define method model-type (t :: <tuple>) => (t :: <collection>)
  map(model-type, t.tuple-types)
end;

define method model-type (t :: <tuple-with-rest>) => (t :: <collection>)
  concatenate(next-method(), vector(make(<&top-type>)))
end;

define method model-type (t :: <limited-collection>) => (t :: <&type>)
  make(<&limited-collection-type>,
       class: t.collection-class.model-type,
       element-type: t.element-type.model-type)
end;

define method model-type (t :: <node>) => (res :: type-union(<collection>, <&type>))
  t.find.node-value.model-type
end;

define method model-type (t :: <type-variable>) => (res :: <&type>)
  t.type-variable-contents.model-type
end;

define thread variable *constraints* :: false-or(<stretchy-vector>) = #f;
define thread variable *graph* :: false-or(<graph>) = #f;

define function add-constraint (c :: <constraint>) => (c :: <constraint>)
  //debug-types(#"constraint", c.left-hand-side, c.right-hand-side);
  add!(*constraints*, c);
  c;
end;

define constant node-to-type = compose(model-type, node-value, find);
define constant temporary-type = compose(node-to-type, lookup-type-node);
define constant temporary-type-abstraction = compose(node-to-type, abstract-and-lookup);

define thread variable *inferring?* :: <boolean> = #f;

define macro with-cached-environment
  { with-cached-environment (?lambda:expression) ?:body end }
 => { 
    let caches = element($lambda-type-caches, ?lambda, default: #f);
    unless (caches)
      caches := pair(make(<type-environment>),
                     pair(make(<graph>, lambda: ?lambda),
                          make(<stretchy-vector>)));
      $lambda-type-caches[?lambda] := caches;
    end;
    dynamic-bind(*constraints* = caches.tail.tail,
                 *type-environment* = caches.head,
                 *graph* = caches.tail.head)
      ?body
    end }
end;

define method type-infer (l :: <&lambda>)
  dynamic-bind(*inferring?* = #t)
    with-cached-environment (l)
      do(abstract-and-lookup, l.parameters);
      for (t in l.environment.temporaries)
        if (~empty?(t.assignments))
          maybe-add-variable-constraints(t);
        end;
      end;
      walk-computations(infer-computation-types, l.body, #f);
      debug-types(#"highlight", 0);
      solve(*graph*, *constraints*, *type-environment*);
      //update values slot of signature
      let result-type = temporary-type(l.body.bind-return.computation-value);
      //might need to emit type check (top is super of everything!)
      if (~ instance?(result-type, <&top-type>)) //top-type is never more specific than some other type
        let res-type = l.body.bind-return.computation-value.lookup-type-node.find.node-value;
        let (res, rest?) = if (instance?(res-type, <tuple-with-rest>))
                             values(map(model-type, res-type.tuple-types), #t); //respect rest-type!
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
    end;
  end;
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

define generic maybe-add-variable-constraints (t :: <temporary>) => ();

define method maybe-add-variable-constraints (t :: <temporary>) => ()
end;

define method maybe-add-variable-constraints (t :: <lexical-specialized-variable>) => ()
  let spec = t.specializer;
  if (spec) //spec for type-union(<integer>, <string>) is #f :[
    //and not <object>?
    for (ass in t.assignments)
      add-constraint(make(<equality-constraint>,
                          origin: t,
                          left: spec.lookup-type-node,
                          right: ass.temporary.abstract-and-lookup));
    end;
  end;
end;

define generic infer-computation-types (c :: <computation>) => ();

define method infer-computation-types (c :: <computation>) => ()
  debug-types(#"highlight", c);
  debug-types(#"beginning", list("inferring", c));
  debug-types(#"relayouted");
  c.temporary & abstract-and-lookup(c.temporary);
end;

define method infer-computation-types (c :: <temporary-transfer-computation>) => ()
  //this is our let!
  next-method();
  add-constraint(make(<equality-constraint>,
                      origin: c,
                      left: c.computation-value.abstract-and-lookup,
                      right: c.temporary.abstract-and-lookup));
end;

define method infer-computation-types (c :: <values>) => ()
  next-method();
  let l = begin
            let types = map(abstract-and-lookup, c.fixed-values);
            gen-tuple(types, rest?: c.rest-value)
          end;
  add-constraint(make(<equality-constraint>,
                      origin: c,
                      left: l,
                      right: c.temporary.abstract-and-lookup));
end;

define generic typist-union
 (t1 :: type-union(<collection>, <&type>),
  t2 :: type-union(<collection>, <&type>)) => (union :: <node>);

define method typist-union (t1 :: <&type>, t2 :: <&type>) => (union :: <node>)
  ^type-union(t1, t2).lookup-type-node;
end;

define method typist-union (t1 :: <collection>, t2 :: <collection>) => (union :: <node>)
  map(typist-union, t1, t2).gen-tuple;
end;

define method infer-computation-types (c :: <phi-node>) => ()
  next-method();
  solve(*graph*, *constraints*, *type-environment*);
  add-constraint(make(<equality-constraint>,
                      origin: c,
                      left: typist-union(c.phi-left-value.temporary-type,
                                         c.phi-right-value.temporary-type),
                      right: c.temporary.abstract-and-lookup));
end;

define method infer-computation-types (c :: <binary-merge>) => ()
  next-method();
  if (c.merge-left-value & c.merge-right-value)
    solve(*graph*, *constraints*, *type-environment*);
    add-constraint(make(<equality-constraint>,
                        origin: c,
                        left: typist-union(c.merge-left-value.temporary-type,
                                           c.merge-right-value.temporary-type),
                        right: c.temporary.abstract-and-lookup));
  elseif (c.merge-left-value | c.merge-right-value)
    let v = c.merge-left-value | c.merge-right-value;
    add-constraint(make(<equality-constraint>,
                        origin: c,
                        left: v.temporary-type.abstract-and-lookup,
                        right: c.temporary.abstract-and-lookup));
  end;
end;

define method extract-parameter-type (c :: <loop-merge>)
  c.loop-merge-parameter
end;

define method extract-parameter-type (c :: <phi-node>)
  c.phi-left-value
end;

define method extract-argument-type (c :: <loop-merge>)
  c.loop-merge-argument
end;

define method extract-argument-type (c :: <phi-node>)
  c.phi-right-value
end;

define method infer-computation-types (c :: <loop>) => ()
  next-method();
  //in order to get types for parameter types, we have to solve the type graph
  solve(*graph*, *constraints*, *type-environment*);

  let types = make(<stretchy-vector>);
  for (node = c.loop-body then node.next-computation,
       while: instance?(node, type-union(<loop-merge>, <phi-node>)))
    add!(types, node.extract-parameter-type.temporary-type);
  end;

  if (types.size > 0)
    //copy loop-body for further investigation (find fixpoint by doing
    //inference once with outer type)
    let copier = make(<dfm-copier>);
    //let cfg-first = deep-copy(copier, c.loop-body);
    let cfg-first = #f;
    let temps = make(<stretchy-vector>);
    walk-computations(method(old)
                        local method maybe-add-temp (old-t, new-t)
                                if (element(*type-environment*, old-t, default: #f))
                                  if (~ instance?(old-t.temporary-type, <&top-type>))
                                    add!(temps, pair(new-t, old-t.temporary-type));
                                  end;
                                end;
                              end;
                        let new = deep-copy(copier, old);
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

    with-cached-environment (cfg-first.environment.lambda)
      //insert types of temporaries into environment
      do(method(x)
           let tv = x.head.abstract-and-lookup;
           add-constraint(make(<equality-constraint>,
                               origin: tv,
                               left: x.tail.lookup-type-node,
                               right: tv));
         end, temps);

      let phis = make(<stretchy-vector>);
      //collect phi and loop-merge nodes
      for (type in types, node = cfg-first then node.next-computation)
        //assign "outer" types to temporaries
        let tv = abstract-and-lookup(node.temporary);
        add-constraint(make(<equality-constraint>,
                            origin: tv,
                            left: type.lookup-type-node,
                            right: tv));
        add!(phis, node);
      end;
      //solve to assign types for phi-temporaries [will be done during <call> inference]

      //infer body with those assigned type variables
      walk-computations(infer-computation-types, phis.last.next-computation, #f);

      //solve constraint system!
      solve(*graph*, *constraints*, *type-environment*);

      //check whether outer and inner types are equal
      // (or subtypes - since GF protocol must not be violated)
      let safe?
        = block(fast-exit)
            for (phi in phis, type in types)
              unless (^subtype?(phi.extract-argument-type.temporary-type,
                                type))
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
    end; //dynamic-bind

    //add type constraint to opposite DF node of phi/loop-merge
    //(is then propagated to temporary of phi by respective inference rule)
    for (node = c.loop-body then node.next-computation, type in types)
      add-constraint(make(<equality-constraint>,
                          origin: node,
                          left: type.lookup-type-node,
                          right: node.extract-argument-type.abstract-and-lookup));
    end;
  end;
end;

define method infer-computation-types (c :: <if>) => ()
  next-method();
  //if test has occurence typing, assign type to temporary involved
  
  //type consequence
  //retract type assignment, assign inverse type
  //type alternative
  //retract types
end;

define method infer-computation-types (c :: <stack-vector>) => ()
  next-method();
  let t = c.temporary;
  add-constraint(make(<equality-constraint>,
                      origin: c,
                      left: map(abstract-and-lookup, c.arguments).gen-tuple,
                      right: t.abstract-and-lookup));
end;

define method infer-computation-types (c :: <make-closure>) => ()
  next-method();
  add-constraint(make(<equality-constraint>,
                      origin: c,
                      left: make(<&limited-function-type>,  //or computation-signature-value? - is #f when sig is statically known
                                 signature: c.computation-closure-method.^function-signature).lookup-type-node,
                      right: c.temporary.abstract-and-lookup));
end;

define method infer-computation-types (c :: type-union(<slot-value>, <repeated-slot-value>)) => ()
  next-method();
  add-constraint(make(<equality-constraint>,
                      origin: c,
                      left: c.computation-slot-descriptor.^slot-type.lookup-type-node,
                      right: c.temporary.abstract-and-lookup));
end;

define method infer-computation-types (c :: type-union(<slot-value-setter>, <repeated-slot-value-setter>)) => ()
  next-method();
  let new-val = c.computation-new-value.abstract-and-lookup;
  let new-val-t = new-val.model-type;
  unless (instance?(new-val-t, <&singleton>) & (new-val-t.^singleton-object == &unbound) & instance?(c.environment.lambda, <&initializer-method>))
    add-constraint(make(<equality-constraint>,
                        origin: c,
                        left: new-val,
                        right: c.temporary.abstract-and-lookup));
    add-constraint(make(<equality-constraint>,
                        origin: c,
                        left: new-val,
                        right: c.computation-slot-descriptor.^slot-type.lookup-type-node));
  end;
end;

define method infer-computation-types (c :: <extract-single-value>) => ()
  next-method();
  solve(*graph*, *constraints*, *type-environment*);
  let tt = c.computation-value.temporary-type;
  if (instance?(tt, <collection>))
    add-constraint(make(<equality-constraint>,
                        origin: c,
                        left: tt[c.index].lookup-type-node,
                        right: c.temporary.abstract-and-lookup));
  end
end;

define method infer-computation-types (c :: <extract-rest-value>) => ()
  next-method();
  //let tt = c.computation-value.lookup-type;
  add-constraint(make(<equality-constraint>,
                      origin: c,
                      left: make(<&top-type>).lookup-type-node, //tt.^tuple-types[index],
                      right: c.temporary.abstract-and-lookup));
end;

define method infer-computation-types (c :: <make-cell>) => ()
  next-method();
  add-constraint(make(<equality-constraint>,
                      origin: c,
                      left: c.computation-value.abstract-and-lookup,
                      right: c.temporary.abstract-and-lookup));
  add-constraint(make(<equality-constraint>,
                      origin: c,
                      left: c.temporary.cell-type.lookup-type-node,
                      right: c.temporary.abstract-and-lookup));
end;

define method infer-computation-types (c :: <get-cell-value>) => ()
  next-method();
  add-constraint(make(<equality-constraint>, origin: c,
                      left: c.computation-cell.abstract-and-lookup,
                      right: c.temporary.abstract-and-lookup));
end;

define method infer-computation-types (c :: <set-cell-value!>) => ()
  next-method();
  add-constraint(make(<equality-constraint>, origin: c,
                      left: c.computation-value.abstract-and-lookup,
                      right: c.temporary.abstract-and-lookup));
  add-constraint(make(<equality-constraint>, origin: c,
                      left: c.computation-value.abstract-and-lookup,
                      right: c.computation-cell.abstract-and-lookup));
end;

define method infer-computation-types (c :: <set!>) => ()
  next-method();
  if (instance?(c.assigned-binding, <module-binding>))
    add-constraint(make(<equality-constraint>, origin: c,
                        left: c.computation-value.abstract-and-lookup,
                        right: c.temporary.abstract-and-lookup));
    add-constraint(make(<equality-constraint>, origin: c,
                        left: c.computation-value.abstract-and-lookup,
                        right: c.assigned-binding.abstract-and-lookup));
  else
    error("assignment is not possible!")
  end
end;
//define method infer-computation-types (c :: <adjust-multiple-values>) => ()
//end;

//define method infer-computation-types (c :: <adjust-multiple-values-rest>) => ()
//end;

//define method infer-computation-types (c :: <keyword-check-type>) => ()
//end;

//define method infer-computation-types (c :: <assignment-check-type>) => ()
//end;

//define method infer-computation-types (c :: <keyword-default>) => ()
//end;

//define method infer-computation-types (c :: <guarantee-type>) => ()
//end;

//define method infer-computation-types (c :: <type-definition>) => ()
//end;

//define method infer-computation-types (c :: <bind-exit>) => ()
//end;

//define method infer-computation-types (c :: <unwind-protect>) => ()
//end;

define method infer-computation-types
 (c :: type-union(<multiple-value-spill>, <multiple-value-unspill>,
                  <redefinition>,
                  <conditional-update!>, <type-redefinition>, 
                  <engine-node-call>)) => ()
  error("didn't expect %=", c);
end;
define generic get-function-object (o :: <object>)
 => (res :: false-or(type-union(<&limited-function-type>, <&function>)));

define method get-function-object (o :: <object>) => (res == #f)
  format-out("can't get function object of an <object>\n");
  #f;
end;

define method get-function-object (t :: <temporary>)
 => (f :: false-or(type-union(<&limited-function-type>, <&function>)))
  let gen = t.generator;
  if (instance?(gen, <make-closure>))
    gen.computation-closure-method;
  elseif (instance?(gen, <extract-single-value>))
    block()
      let res = gen.computation-value.temporary-type[gen.index];
      instance?(res, <&limited-function-type>) & res
    exception (e :: <condition>)
      #f
    end
  elseif (instance?(gen, <call>))
    let res = gen.temporary.temporary-type;
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
  debug-types(#"highlight", c);
  debug-types(#"beginning", list("inferring", c));
  debug-types(#"relayouted");
  let fun = c.function.get-function-object;
  infer-function-type(c, fun);
end;

define method infer-computation-types (c :: <primitive-call>) => ()
  next-method();
  let s = c.primitive.primitive-signature;
  create-arrow-and-constraint(c, s);
end;

define method infer-function-type (c :: <function-call>, fun == #f) => ()
  //well, need to generate an arrow-type, args and vals, constraint...
  create-arrow-and-constraint(c, make(<&signature>, rest-value?: #t, rest?: #t, values: #[], number-values: 0, required: #[], number-required: 0));
end;

define method infer-function-type (c :: <function-call>, fun :: <&limited-function-type>) => ()
  create-arrow-and-constraint(c, fun.^function-signature);
end;

define function gen-tuple (types :: <collection>, #key rest?) => (res :: <node>)
  make(if (rest?) <tuple-with-rest> else <tuple> end, tuples: types).lookup-type-node
end;

define function create-arrow-and-constraint
 (c :: <call>, sig :: <&signature>) => ()
  let specializers = map(lookup-type-node, sig.^signature-required-arguments);
  let vals = map(lookup-type-node, sig.^signature-required-values);
  let left = make(<node>,
                  graph: *graph*,
                  value: make(<arrow>,
                              arguments: gen-tuple(specializers, rest?: sig.^signature-optionals?),
                              values: gen-tuple(vals, rest?: sig.^signature-rest-value)));

  let args = map(abstract-and-lookup, c.arguments);

  let vals
    = if (c.temporary)
        let res = c.temporary.abstract-and-lookup;
        if (~instance?(c.temporary, <multiple-value-temporary>))
          vector(res).gen-tuple;
        else
          res
        end;
      else
        make(<&top-type>).lookup-type-node;
      end;

  let right = make(<node>, graph: *graph*,
                   value: make(<arrow>,
                               arguments: args.gen-tuple,
                               values: vals));
  debug-types(#"type-relation", right, c);
  add-constraint(make(<equality-constraint>, origin: c, left: left, right: right));
end;

define method upgrade-types (l)
end;

define method upgrade-types (l :: <&lambda>)
  dynamic-bind(*type-environment* = head(element($lambda-type-caches, l,
                                                 default: pair(make(<table>), #f))))
    for (t in l.^function-signature.^signature-required-arguments, p in l.parameters)
      unless (^type-equivalent?(t, p.specializer))
        p.specializer := t;
        //actually, add a constraint for given specializer,
        //but not yet in correct context (<graph>-wise)
        // actually, *constraint* get bound to #[] in type-infer, thus modifying constraints
        // does not make sense yet
        remove-key!(*type-environment*, p);
      end;
    end;
  end;
  l.type-infer
end;

define function initial-type-constraints (sig :: <&signature>) //XXX: that's actually a really bad name for what it is doing
  do(curry(remove-key!, *type-environment*), sig.^signature-required-arguments);
end;

define function constrain-type-variables
    (c :: <function-call>, sig-args :: <collection>, real-args :: <collection>)
  for (s in sig-args, r in real-args)
    add-constraint(make(<equality-constraint>,
                        origin: c, left: s, right: r));
  end;
end;

define method infer-function-type (c :: <function-call>, fun :: <&function>) => ()
  //keyword-arguments!
  let sig = ^function-signature(fun);
  unless (c.environment.lambda == fun) //updating self-calls is not wise (or, is it?)
    if (instance?(sig, <&polymorphic-signature>))
      //XXX: copy me if not single user!
      solve(*graph*, *constraints*, *type-environment*);
      //initial-type-constraints(sig);
      constrain-type-variables(c, map(lookup-type-node, sig.^signature-required-arguments),
                               map(abstract-and-lookup, c.arguments));
      let progress? = #t;
      while (progress?)
        let old-types = map(temporary-type, sig.^signature-type-variables);
        let old-argument-types = map(temporary-type-abstraction, c.arguments);
        solve(*graph*, *constraints*, *type-environment*);
        let new-types = map(temporary-type, sig.^signature-type-variables);
        let new-argument-types = map(temporary-type-abstraction, c.arguments);
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
        solve(*graph*, *constraints*, *type-environment*);
        let newer-types = map(temporary-type, sig.^signature-type-variables);
        progress? := #f;
        for (n in new-types, n2 in newer-types)
          if (~ ^type-equivalent?(n, n2))
            progress? := #t;
          end;
        end;
      end;
      //upgrade signature (using type variable types)
      let arg = map(temporary-type, sig.^signature-required-arguments);
      let val = map(temporary-type, sig.^signature-required-values);
      fun.^function-signature := make(<&signature>, required:  arg, number-required: arg.size, values: val, number-values: val.size);
      let env = element($lambda-type-caches, fun, default: #f);
      let tv-types = map(temporary-type, sig.^signature-type-variables);
      dynamic-bind(*type-environment* = env.head, *graph* = env.tail.head, *constraints* = env.tail.tail)
        for (tv in sig.^signature-type-variables, tv-type in tv-types)
          add-constraint(make(<equality-constraint>,
                              left: lookup-type-node(tv),
                              right: tv-type.lookup-type-node,
                              origin: c));
          solve(*graph*, *constraints*, *type-environment*);
        end
      end;
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
    solve(*graph*, *constraints*, *type-environment*);

    // then, try to upgrade the GF call to a simple call (narrowing result type)
    let arguments = map(temporary-type-abstraction, c.arguments);
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
