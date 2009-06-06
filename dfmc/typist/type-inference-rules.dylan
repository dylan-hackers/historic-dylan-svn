module: dfmc-typist

define constant $lambda-type-caches :: <table> = make(<table>);

define constant <type-environment> = <table>;

define thread variable *type-environment* :: false-or(<type-environment>) = #f;

define generic lookup-type (o) => (res :: <node>);

define method lookup-type (o :: <temporary>) => (res :: <node>)
  element(*type-environment*, o, default: #f) |
    begin
      let te = o.type-estimate-object;
      let tv = make(<type-variable>, contents: te);
      debug-types(#"new-type-variable", tv, o, te);
      let n = make(<node>, graph: *graph*, value: tv);
      *type-environment*[o] := n;
      unless (instance?(te, <&top-type>))
        add-constraint(make(<equality-constraint>, origin: o,
                            left: te.lookup-type, right: n));
      end;
      n;
    end;
end;

define method lookup-type (o :: <multiple-value-temporary>) => (res :: <node>)
  element(*type-environment*, o, default: #f) |
    begin
      let tes = make(<simple-object-vector>);
      for (x from 0 below o.required-values)
        tes := add(tes, make(<&top-type>));
      end;
      let top = make(<&top-type>);
      let tv = make(<type-variable>, contents: top);
      debug-types(#"new-type-variable", tv, o, top);
      let n = make(<node>, graph: *graph*, value: tv);
      *type-environment*[o] := n;
      add-constraint(make(<equality-constraint>, origin: o,
                          left: gen-tuple(map(lookup-type, tes), rest?: o.rest-values?),
                          right: n));
      n;
    end;
end;

define method lookup-type (t :: <lexical-required-type-variable>)
 => (res :: <node>)
  t.specializer.lookup-type
end;

define method lookup-type (tv :: <&polymorphic-type-variable>)
 => (res :: <node>)
  element(*type-environment*, tv, default: #f) |
    begin
      let node = make(<node>, graph: *graph*, value: tv);
      //add constraint between ^type-variable-kind and node!
      *type-environment*[tv] := node;
    end;
end;

define method lookup-type (o :: <object-reference>) => (res :: <node>)
  element(*type-environment*, o, default: #f) |
    begin
      let n = next-method();
      debug-types(#"type-relation", n, o);
      *type-environment*[o] := n;
    end;
end;

define method lookup-type (o :: <object>) => (res :: <node>)
  make(<node>, graph: *graph*, value: o.type-estimate-object);
end;

define generic type-estimate-object (o) => (res :: type-union(<typist-type>, <&type>));

define method type-estimate-object (o :: <object>) => (res :: <&type>)
  make(<&top-type>)
end;

define method type-estimate-object (o :: <object-reference>)
 => (res :: type-union(<typist-type>, <&type>))
  type-estimate-object(o.reference-value)
end;

define method type-estimate-object (o :: <lexical-specialized-variable>)
 => (res :: type-union(<typist-type>, <&type>));
  type-estimate-object(o.specializer)
end;

define method type-estimate-object (t :: <&type>)
 => (res :: <&type>)
  if (t == dylan-value(#"<object>"))
    make(<&top-type>)
  else
    t
  end
end;

define method type-estimate-object (t :: <typist-type>)
 => (res :: <typist-type>)
  t
end;

define method type-estimate-object (lft :: <&limited-function-type>)
 => (res :: type-union(<typist-type>, <&type>))
  let sig = lft.^function-signature;
  let args = map(lookup-type, sig.^signature-required-arguments);
  let vals = map(lookup-type, sig.^signature-required-values);
  make(<arrow>, arguments: gen-tuple(args, rest?: sig.^signature-rest?),
       values: gen-tuple(vals, rest?: sig.^signature-rest-value));
end;

define method type-estimate-object (lc :: <&limited-collection-type>)
 => (res :: type-union(<typist-type>, <&type>))
  let class = lc.^limited-collection-class.lookup-type;
  let etype = lc.^limited-collection-element-type.lookup-type;
  make(<limited-collection>, class: class, element-type: etype);
end;

define method type-estimate-object (o :: <integer>) => (res :: <&type>)
  dylan-value(#"<integer>")
end;

define method type-estimate-object (s :: <string>) => (res :: <&type>)
  dylan-value(#"<string>")
end;

define method type-estimate-object (b :: <boolean>) => (res :: <&type>)
  dylan-value(#"<boolean>")
end;

define method type-estimate-object (v :: <vector>)
 => (res :: type-union(<typist-type>, <&type>))
  make(<limited-collection>,
       class: dylan-value(#"<vector>").lookup-type,
       element-type:
         lookup-type(if (v.size == 0)
                       make(<&polymorphic-type-variable>,
                            name: #"v", kind: dylan-value(#"<top>"));
                     else
                       reduce1(^type-union, map(type-estimate-object, v));
                     end))
end;

//list, floats, symbols!

define generic model-type (t :: type-union(<node>, <typist-type>, <&type>)) => (t :: type-union(<collection>, <&type>));

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
  debug-types(#"constraint", c.left-hand-side, c.right-hand-side);
  add!(*constraints*, c);
  c;
end;

define constant temporary-type = compose(model-type, node-value, find, lookup-type);

define thread variable *inferring?* :: <boolean> = #f;

define method type-infer (l :: <&lambda>)
  dynamic-bind(*inferring?* = #t)
    let caches = element($lambda-type-caches, l, default: #f);
    unless (caches)
      caches := pair(make(<type-environment>), pair(make(<graph>), make(<stretchy-vector>)));
      $lambda-type-caches[l] := caches;
    end;
    dynamic-bind(*constraints* = caches.tail.tail,
                 *type-environment* = caches.head,
                 *graph* = caches.tail.head)
      do(lookup-type, l.parameters);
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
        let res-type = l.body.bind-return.computation-value.lookup-type.find.node-value;
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

define method convert-type-to-signature (old-sig == #f, parameters :: <collection>, values :: <&type>, rest? :: <boolean>)
 => (signature :: <&signature>)
  let param = map(specializer, parameters);
  ^make(<&signature>, required: param, number-required: param.size,
        values: vector(values), number-values: 1, rest?: rest?);
end;

define method convert-type-to-signature (old-sig == #f, parameters :: <collection>, values :: <collection>, rest? :: <boolean>)
 => (signature :: <&signature>)
  let param = map(specializer, parameters);
  ^make(<&signature>, required: param, number-required: param.size,
        values: values, number-values: values.size, rest?: rest?);
end;

define method convert-type-to-signature (old-sig :: <&signature>, parameters :: <collection>, values :: <&type>, rest? :: <boolean>)
 => (signature :: <&signature>)
  //we can also upgrade <&signature> -> <&polymorphic-signature>
  ^make(<&signature>, required: old-sig.^signature-required-arguments,
        number-required: old-sig.^signature-number-required,
        values: vector(values), number-values: 1, rest?: rest?);
end;

define method convert-type-to-signature (old-sig :: <&signature>, parameters :: <collection>, values :: <collection>, rest? :: <boolean>)
 => (signature :: <&signature>)
  //we can also upgrade <&signature> -> <&polymorphic-signature>
  ^make(<&signature>, required: old-sig.^signature-required-arguments,
        number-required: old-sig.^signature-number-required,
        values: values, number-values: values.size, rest?: rest?);
end;

define method convert-type-to-signature (old-sig :: <&polymorphic-signature>, parameters :: <collection>, values :: <&type>, rest? :: <boolean>)
 => (signature :: <&signature>)
  //or add/remove type variables...
  ^make(<&signature>, required: old-sig.^signature-required-arguments,
        number-required: old-sig.^signature-number-required,
        values: vector(values), number-values: 1, rest?: rest?,
        type-variables: old-sig.^signature-type-variables);
end;

define method convert-type-to-signature (old-sig :: <&polymorphic-signature>, parameters :: <collection>, values :: <collection>, rest? :: <boolean>)
 => (signature :: <&signature>)
  //or add/remove type variables...
  ^make(<&signature>, required: old-sig.^signature-required-arguments,
        number-required: old-sig.^signature-number-required,
        values: values, number-values: values.size, rest?: rest?,
        type-variables: old-sig.^signature-type-variables);
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
                          left: spec.lookup-type,
                          right: ass.temporary.lookup-type));
    end;
  end;
end;

define generic infer-computation-types (c :: <computation>) => ();

define method infer-computation-types (c :: <computation>) => ()
  debug-types(#"highlight", c);
  debug-types(#"beginning", list("inferring", c));
  debug-types(#"relayouted");
  c.temporary & lookup-type(c.temporary);
end;

define method infer-computation-types (c :: <temporary-transfer-computation>) => ()
  //this is our let!
  next-method();
  add-constraint(make(<equality-constraint>,
                      origin: c,
                      left: c.computation-value.lookup-type,
                      right: c.temporary.lookup-type));
end;

define method infer-computation-types (c :: <check-type>) => ()
  next-method();
  add-constraint(make(<equality-constraint>,
                      origin: c,
                      left: c.type.lookup-type,
                      right: c.temporary.lookup-type));
end;

define method infer-computation-types (c :: <values>) => ()
  next-method();
  let l = begin
            let types = map(lookup-type, c.fixed-values);
            gen-tuple(types, rest?: c.rest-value)
          end;
  add-constraint(make(<equality-constraint>,
                      origin: c,
                      left: l,
                      right: c.temporary.lookup-type));
end;

define generic typist-union
 (t1 :: type-union(<collection>, <&type>),
  t2 :: type-union(<collection>, <&type>)) => (union :: <node>);

define method typist-union (t1 :: <&type>, t2 :: <&type>) => (union :: <node>)
  ^type-union(t1, t2).lookup-type;
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
                      right: c.temporary.lookup-type));
end;

define method infer-computation-types (c :: <binary-merge>) => ()
  next-method();
  if (c.merge-left-value & c.merge-right-value)
    solve(*graph*, *constraints*, *type-environment*);
    add-constraint(make(<equality-constraint>,
                        origin: c,
                        left: typist-union(c.merge-left-value.temporary-type,
                                           c.merge-right-value.temporary-type).lookup-type,
                        right: c.temporary.lookup-type));
  elseif (c.merge-left-value | c.merge-right-value)
    let v = c.merge-left-value | c.merge-right-value;
    add-constraint(make(<equality-constraint>,
                        origin: c,
                        left: v.temporary-type.lookup-type,
                        right: c.temporary.lookup-type));
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

    dynamic-bind(*graph* = make(<graph>),
                 *constraints* = make(<stretchy-vector>),
                 *type-environment* = make(<type-environment>))
      //insert types of temporaries into environment
      do(method(x)
           let tv = x.head.lookup-type;
           add-constraint(make(<equality-constraint>,
                               origin: tv,
                               left: x.tail.lookup-type,
                               right: tv));
         end, temps);

      let phis = make(<stretchy-vector>);
      //collect phi and loop-merge nodes
      for (type in types, node = cfg-first then node.next-computation)
        //assign "outer" types to temporaries
        let tv = lookup-type(node.temporary);
        add-constraint(make(<equality-constraint>,
                            origin: tv,
                            left: type.lookup-type,
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
                fast-exit(#f);
              end;
            end;
            #t;
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
                          left: type.lookup-type,
                          right: node.extract-argument-type.lookup-type));
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
                      left: make(<tuple>, tuples: map(lookup-type, c.arguments)).lookup-type,
                      right: t.lookup-type));
end;

define method infer-computation-types (c :: <make-closure>) => ()
  next-method();
  add-constraint(make(<equality-constraint>,
                      origin: c,
                      left: make(<&limited-function-type>,  //or computation-signature-value? - is #f when sig is statically known
                                 signature: c.computation-closure-method.^function-signature).lookup-type,
                      right: c.temporary.lookup-type));
end;

define method infer-computation-types (c :: type-union(<slot-value>, <repeated-slot-value>)) => ()
  next-method();
  add-constraint(make(<equality-constraint>,
                      origin: c,
                      left: c.computation-slot-descriptor.^slot-type.lookup-type,
                      right: c.temporary.lookup-type));
end;

define method infer-computation-types (c :: type-union(<slot-value-setter>, <repeated-slot-value-setter>)) => ()
  next-method();
  add-constraint(make(<equality-constraint>,
                      origin: c,
                      left: c.computation-new-value.lookup-type,
                      right: c.temporary.lookup-type));
  add-constraint(make(<equality-constraint>,
                      origin: c,
                      left: c.computation-new-value.lookup-type,
                      right: c.computation-slot-descriptor.^slot-type.lookup-type));
end;

define method infer-computation-types (c :: <multiple-value-check-type>) => ()
  next-method();
  add-constraint(make(<equality-constraint>,
                      origin: c,
                      left: map(lookup-type, c.types).gen-tuple,
                      right: c.temporary.lookup-type));
end;

define method infer-computation-types (c :: <multiple-value-check-type-rest>) => ()
  next-method();
  let ts = map(lookup-type, c.types);
  add-constraint(make(<equality-constraint>,
                      origin: c,
                      left: gen-tuple(ts, rest?: c.rest-type), //pass real rest type!
                      right: c.temporary.lookup-type));
end;

define method infer-computation-types (c :: <extract-single-value>) => ()
  next-method();
  let tt = c.computation-value.temporary-type;
  add-constraint(make(<equality-constraint>,
                      origin: c,
                      left: tt[index],
                      right: c.temporary.lookup-type));
end;

define method infer-computation-types (c :: <extract-rest-value>) => ()
  next-method();
  //let tt = c.computation-value.lookup-type;
  add-constraint(make(<equality-constraint>,
                      origin: c,
                      left: make(<&top-type>).lookup-type, //tt.^tuple-types[index],
                      right: c.temporary.lookup-type));
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
                  <make-cell>, <get-cell-value>, <set-cell-value!>,
                  <assignment>, <any-definition>, <definition>, <redefinition>,
                  <set!>, <conditional-update!>, <type-redefinition>, 
                  <apply>, <method-apply>, <engine-node-apply>, <engine-node-call>)) => ()
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
  else
    get-function-object(gen.computation-value); //for check-type!
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
end;

define method infer-function-type (c :: <function-call>, fun :: <&limited-function-type>) => ()
  create-arrow-and-constraint(c, fun.^function-signature);
end;

define function gen-tuple (types :: <collection>, #key rest?) => (res :: <node>)
  make(if (rest?) <tuple-with-rest> else <tuple> end, tuples: types).lookup-type
end;

define function create-arrow-and-constraint
 (c :: <call>, sig :: <&signature>) => ()
  let specializers = map(lookup-type, sig.^signature-required-arguments);
  let vals = map(lookup-type, sig.^signature-required-values);
  let left = make(<node>,
                  graph: *graph*,
                  value: make(<arrow>,
                              arguments: gen-tuple(specializers, rest?: sig.^signature-optionals?),
                              values: gen-tuple(vals, rest?: sig.^signature-rest-value)));

  let args = map(lookup-type, c.arguments);

  let vals
    = if (c.temporary)
        let res = c.temporary.lookup-type;
        if (~instance?(c.temporary, <multiple-value-temporary>))
          vector(res).gen-tuple;
        else
          res
        end;
      else
        make(<&top-type>).lookup-type;
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

define function initial-type-constraints (sig :: <&signature>)
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
      solve(*graph*, *constraints*, *type-environment*);
      initial-type-constraints(sig);
      constrain-type-variables(c, map(lookup-type, sig.^signature-required-arguments),
                               map(lookup-type, c.arguments));
      let progress? = #t;
      while (progress?)
        let old-types = map(temporary-type, sig.^signature-type-variables);
        let old-argument-types = map(temporary-type, c.arguments);
        solve(*graph*, *constraints*, *type-environment*);
        let new-types = map(temporary-type, sig.^signature-type-variables);
        let new-argument-types = map(temporary-type, c.arguments);
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
    end;
  end;
  create-arrow-and-constraint(c, sig);
end;

define method infer-function-type (c :: <simple-call>, gf :: <&generic-function>) => ()
  //simple strategy here:
  // first, solve the type graph (at least the partial graph we have so far)
  //this might conflict with the general idea of the type graph (error narrowing),
  //but recording the types of the GF in the constraint is too generic
  solve(*graph*, *constraints*, *type-environment*);

  // then, try to upgrade the GF call to a simple call (narrowing result type)
  let arguments = map(temporary-type, c.arguments);
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
