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
                          left: te.lookup-type, right: n));
    end;
  end;
  res | tenv[o];
end;

define function congruent? (u :: <node>, v :: <node>) => (congruent? :: <boolean>)
  let edge-equal? = method(e1, e2) find(e1.edge-target) = find(e2.edge-target) end;
  let edge-equal-in? = method(e1, e2) find(e1.edge-source) = find(e2.edge-source) end;
  let u = find(u);
  let v = find(v);
  if (u == v)
    #t
  elseif (u.node-value = v.node-value)
    every?(method(x) any?(curry(edge-equal?, x), v.out-edges) end, u.out-edges)
     & every?(method(x) any?(curry(edge-equal?, x), u.out-edges) end, v.out-edges)
     & every?(method(x) any?(curry(edge-equal-in?, x), v.in-edges) end, u.in-edges)
     & every?(method(x) any?(curry(edge-equal-in?, x), u.in-edges) end, v.in-edges)
  end;
end;

define function create-sharing (u :: <node>) => (v :: <node>)
  block(found)
    for (node in *graph*.nodes)
      if (find(u) ~= find(node) & congruent?(u, node))
        found(graph-union(node, u, #f));
      end;
    end;
    u;
  end;
end;

define function lookup-type (o :: <object>) => (res :: <node>)
  let tenv = *type-environment*;
//  let res = element(tenv, o, default: #f);
//  if (res)
//    format-out("found cached type node\n");
//  else
//    format-out("getting new type node\n");
    let te = type-estimate-object(o);
    if (te == dylan-value(#"<object>"))
      te := make(<&top-type>);
    end;
    if (instance?(o, <temporary>))
      lookup-type-variable(o);
    else
      let n = make(<node>, graph: *graph*, value: te);
      //let u = create-sharing(n);
      //if (u ~= n)
      //  format-out("found cached type node\n");
      //else
      format-out("new type node\n");
      //end;
      //u;
      n;
    end;
//    tenv[o] := n;
//  end;
//  res | tenv[o];
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

define method type-estimate-value (t :: <&type>) => (res :: <&type>)
  t;
end;

define method type-estimate-value (o :: <integer>) => (res :: <&type>)
  dylan-value(#"<integer>");
end;

define method type-estimate-value (s :: <string>) => (res :: <&type>)
  dylan-value(#"<string>");
end;

//list, floats!

define thread variable *constraints* :: false-or(<stretchy-vector>) = #f;
define thread variable *graph* :: false-or(<graph>) = #f;

define function add-constraint (c :: <constraint>) => (c :: <constraint>)
  debug-types(#"constraint", c.left-hand-side, c.right-hand-side);
  add!(*constraints*, c);
  c;
end;

define method type-infer (l :: <&lambda>)
  *type-environment* := make(<type-environment>);
  dynamic-bind(*constraints* = make(<stretchy-vector>),
               *graph* = make(<graph>))
    do(lookup-type-variable, l.parameters);
    for (t in l.environment.temporaries)
      if (~empty?(t.assignments))
        maybe-add-variable-constraints(t);
      end;
    end;
    walk-computations(infer-computation-types, l.body, #f);
    debug-types(#"highlight", 0);
    solve(*graph*, *constraints*, *type-environment*);
  end;
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
                          left: spec.lookup-type,
                          right: ass.temporary.lookup-type-variable));
    end;
  end;
end;

define generic infer-computation-types (c :: <computation>) => ();

define method infer-computation-types (c :: <computation>) => ()
  debug-types(#"relayouted");
  debug-types(#"highlight", c);
  c.temporary & lookup-type-variable(c.temporary);
end;

define method infer-computation-types (c :: <temporary-transfer-computation>) => ()
  //this is our let!
  next-method();
  add-constraint(make(<equality-constraint>,
                      left: c.computation-value.lookup-type,
                      right: c.temporary.lookup-type-variable));
end;

define method infer-computation-types (c :: <check-type>) => ()
  next-method();
  add-constraint(make(<equality-constraint>,
                      left: c.type.lookup-type,
                      right: c.temporary.lookup-type-variable));
end;

define method infer-computation-types (c :: <values>) => ()
  next-method();
  add-constraint(make(<equality-constraint>,
                      left: lookup-type(if (c.fixed-values.size == 1)
                                          c.fixed-values.first
                                        else
                                          make(<&tuple-type>,
                                               tuples: map(lookup-type, c.fixed-values))
                                        end),
                      right: c.temporary.lookup-type-variable));
end;

define constant temporary-type = compose(^type-variable-contents, node-value, lookup-type-variable);

define method infer-computation-types (c :: <phi-node>) => ()
  next-method();
  solve(*graph*, *constraints*, *type-environment*);
  add-constraint(make(<equality-constraint>,
                      left: ^type-union(c.phi-left-value.temporary-type,
                                        c.phi-right-value.temporary-type).lookup-type,
                      right: c.temporary.lookup-type-variable));
end;

define method infer-computation-types (c :: <loop-merge>) => ()
  next-method();
  solve(*graph*, *constraints*, *type-environment*);
  add-constraint(make(<equality-constraint>,
                      left: ^type-union(c.merge-left-value.temporary-type,
                                        c.merge-right-value.temporary-type).lookup-type,
                      right: c.temporary.lookup-type-variable));  
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

  let types = make(<stretchy-vector>);
  for (node = c.loop-body then node.next-computation,
       while: instance?(node, type-union(<loop-merge>, <phi-node>)))
    add!(types, node.extract-parameter-type.temporary-type);
  end;

  dynamic-bind(*graph* = make(<graph>),
               *constraints* = make(<stretchy-vector>),
               *type-environment* = make(<type-environment>))
    //insert types of temporaries into environment
    do(method(x)
         let tv = x.head.lookup-type-variable;
         add-constraint(make(<equality-constraint>,
                             left: x.tail.lookup-type,
                             right: tv));
       end, temps);

    let phis = make(<stretchy-vector>);
    //collect phi and loop-merge nodes
    for (type in types, node = cfg-first then node.next-computation)
      //assign "outer" types to temporaries
      let tv = lookup-type-variable(node.temporary);
      add-constraint(make(<equality-constraint>,
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
                        left: type.lookup-type,
                        right: node.extract-argument-type.lookup-type-variable));
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
  let t = c.temporary;
  add-constraint(make(<equality-constraint>,
                      left: make(<&tuple-type>, tuples: map(lookup-type, c.arguments)).lookup-type,
                      right: t.lookup-type-variable));
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
  //next-method(); -- otherwise, we end up with more type variables than needed in
  //the type environment
  debug-types(#"highlight", c);
  let fun = c.function.get-function-object;
  infer-function-type(c, fun);
end;

define method infer-function-type (c :: <function-call>, fun :: <&function>) => ()
  format-out("got %=\n", fun);
  //keyword-arguments, #rest!
  let sig = ^function-signature(fun);
  let values
    = copy-sequence(sig.^signature-values, end: sig.^signature-number-values);
  let specializers
    = begin
        let args = ^function-specializers(fun);
        let rest? = ^signature-rest?(sig);
        //#rest can be annotated with a type, but this information is
        //lost in translation
        let nodes = map(lookup-type, args);
        if (nodes.size == 1 & ~rest?)
          nodes.first;
        else
          if (rest?)
            nodes := add!(nodes, make(<&rest-type>).lookup-type);
          end;
          lookup-type(make(<&tuple-type>, tuples: nodes));
        end;
      end;
  
  let vals
    = begin
        let nodes = map(lookup-type, values);
        let rest-values? = ^signature-rest-value(sig);
        if (nodes.size == 1 & ~rest-values?)
          nodes.first;
        else
          if (rest-values?)
            nodes := add!(nodes, make(<&rest-type>).lookup-type);
          end;
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
        let nodes = map(lookup-type-variable, c.arguments);
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
                               values: lookup-type-variable(c.temporary)));
  add-constraint(make(<equality-constraint>, left: left, right: right));
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
