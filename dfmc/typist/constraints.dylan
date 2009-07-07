module: dfmc-typist

define abstract class <constraint> (<object>)
  constant slot left-hand-side :: <node>,
    required-init-keyword: left:;
  constant slot right-hand-side :: <node>,
    required-init-keyword: right:;
  constant slot origin, //:: type-union(<computation>, <&type>, <constraint>),
    required-init-keyword: origin:;
end;

define method type-environment (c :: <constraint>) => (result :: <type-environment>)
  c.origin.type-environment
end;

define class <equality-constraint> (<constraint>)
end;

define method make (class :: subclass(<constraint>), #rest init-args, #key, #all-keys) => (res :: <constraint>)
  let c = next-method();
  connect(c.left-hand-side, c.right-hand-side, edge-type: <constraint-edge>);
  c;
end;

define function disconnect (c :: <constraint>) => ()
  disconnect-constraint(c.left-hand-side, c.right-hand-side);
end;

define function deep-origin (c :: <constraint>) => (o)
  if (instance?(c.origin, <constraint>))
    c.origin.deep-origin;
  else
    c.origin;
  end;
end;

define program-warning <type-unification-failed>
  slot condition-type-estimate1,
    required-init-keyword: type-estimate1:;
  slot condition-type-estimate2,
    required-init-keyword: type-estimate2:;
  format-string
    "Could not unify %s and %s.";
  format-arguments type-estimate1, type-estimate2;
end;

define function solve (type-env :: <type-environment>)
 => ()
  let graph = type-env.type-graph;
  let constraints = type-env.type-constraints;
  let cs = as(<deque>, constraints);
  constraints.size := 0;
  for (node in graph.graph-nodes)
    node.contains-variables? := #t;
  end;
  unless (cs.empty?)
    debug-types(#"beginning", type-env, list("solve"));
  end;
  while (~cs.empty?)
    let constraint = cs.pop;
    local method push-cs (l :: <node>, r :: <node>)
            unless (l == r)
              let new-constraint = make(<equality-constraint>, left: l, right: r, origin: constraint);
              push-last(cs, new-constraint);
            end;
          end;
    //debug-types(#"beginning", list("solve", constraint.deep-origin));
    debug-types(#"highlight-constraint", type-env, constraint.left-hand-side, constraint.right-hand-side);
    debug-types(#"relayouted", type-env);
    let u = find(constraint.left-hand-side);
    let v = find(constraint.right-hand-side);
    if (u ~= v)
      let (u, v, flag) = order(u, v);
      let ute = u.node-value;
      let vte = v.node-value;
      graph-union(u, v, flag);
      block()
        solve-constraint(ute, vte, u, v, push-cs);
      exception (e :: <error>)
        unless (exceptions?(ute, vte, u, v, push-cs))
          let orig = deep-origin(constraint);
          note(<type-unification-failed>,
               source-location: dfm-source-location(orig),
               context-id:      dfm-context-id(orig),
               type-estimate1:  ute,
               type-estimate2:  vte);
          //error("constraint %= cannot be satisfied", constraint);
        end
      end;
    end;
    debug-types(#"unhighlight-constraint", type-env, constraint.left-hand-side, constraint.right-hand-side);
    disconnect(constraint);
    local method may-remove (n :: <node>) => ()
            if (member?(n, graph.graph-nodes))
              if (n.in-edges.size == 0)
                unless (any?(rcurry(instance?, <constraint-edge>), n.out-edges))
                  unless (member?(n, type-env))
                    n.remove-node
                  end;
                end;
              end;
            end;
          end;
    u.may-remove;
    v.may-remove;
    u.may-remove;
  end;
  let quotient-graph = create-quotient-graph(graph);
  if (acyclic?(quotient-graph))
    let changed-vars = make(<stretchy-vector>);
    do(method(x)
         if (instance?(x.node-value, <type-variable>))
           let rep-type = x.find.node-value;
           if (instance?(rep-type, type-union(<&type>, <typist-type>)) & ~instance?(rep-type, <type-variable>))
             //format-out("changed TV %= to contain type %= now\n", x.node-value.get-id, rep-type);
             add!(changed-vars, x.node-value);
             x.node-value.type-variable-contents := rep-type;
           end
         end
       end, graph.graph-nodes);
    for (ele in type-env.key-sequence)
      let val = element(type-env, ele, default: #f);
      if (val & member?(val.node-value, changed-vars))
        debug-types(#"change-type", type-env, ele, format-to-string("%=", val.node-value.type-variable-contents.model-type));
      end;
    end;
  else
    error("type graph %= contains cycles!", quotient-graph)
  end;
end;

define method exceptions? (t1 :: type-union(<typist-type>, <&type>), t2 :: type-union(<typist-type>, <&type>), u :: <node>, v :: <node>, push-constraint :: <function>)
 => (res :: <boolean>)
  //dirty stuff goes here
  //dispatch-prologue needs to cast from a <mm-wrapper> to a <integer>;
  //this is done by pointer-as-integer, called from interpret-mm-wrapper-as-integer
  //which uses cast-raw-as-integer(unwrap-m-w(force-int-tag(wrap-m-w(cast-pointer-as-raw(x)))))
  //where x happens to be a <mm-wrapper> (obviously, cast-pointer-as-raw doesn't work, because
  //its signature is <raw-pointer> => <raw-address>
  let ex1 = pair(dylan-value(#"<mm-wrapper>"), dylan-value(#"<raw-pointer>"));
  if ((t1 == head(ex1) & t2 == tail(ex1)) | (t1 == tail(ex1) & t2 == head(ex1)))
    #t
  end;
end;

define generic solve-constraint
 (t1 :: type-union(<typist-type>, <&type>), t2 :: type-union(<typist-type>, <&type>), u :: <node>, v :: <node>, push-constraint :: <function>)
 => ();

define method solve-constraint
 (t1 :: <arrow>, t2 :: <arrow>, u :: <node>, v :: <node>, push-constraint :: <function>)
 => ()
  for (u1 in u.successors, v1 in v.successors)
    push-constraint(u1, v1);
  end;
end;

define method solve-constraint
 (t1 :: <arrow>, t2 :: <&top-type>, u :: <node>, v :: <node>, push-constraint :: <function>)
 => ()
  if (u.contains-variables?)
    u.contains-variables? := #f;
    for (u1 in u.successors)
      let w1 = make(<node>, graph: u.graph, value: make(<&top-type>));
      push-constraint(w1, u1);
    end;
  end;
end;

define method solve-constraint
 (t1 :: <tuple>, t2 :: <tuple>, u :: <node>, v :: <node>, push-constraint :: <function>)
 => ()
  map(push-constraint, u.successors, v.successors);
end;

define method solve-constraint
    (t1 :: <tuple>, t2 :: <tuple-with-rest>,
     u :: <node>, v :: <node>, push-constraint :: <function>) => ()
  next-method();
  let tts = t2.tuple-types;
  let orig-size = tts.size;
  for (i from v.successors.size below u.successors.size)
    let n = make(<node>, graph: u.graph, value: make(<&top-type>));
    tts := add(tts, n);
    push-constraint(u.successors[i], n)
  end;
  t2.tuple-types := tts;
  update-connections(v, orig-size);
end;

define method solve-constraint
    (t1 :: <tuple-with-rest>, t2 :: <tuple>,
     u :: <node>, v :: <node>, push-constraint :: <function>) => ()
  next-method();
  let tts = t1.tuple-types;
  let orig-size = tts.size;
  for (i from u.successors.size below v.successors.size)
    let top = make(<node>, graph: u.graph, value: make(<&top-type>));
    tts := add(tts, top);
    push-constraint(v.successors[i], top)
  end;
  t1.tuple-types := tts;
  update-connections(u, orig-size);
end;

define method solve-constraint
    (t1 :: <tuple-with-rest>, t2 :: <tuple-with-rest>,
     u :: <node>, v :: <node>, push-constraint :: <function>) => ()
  //next-method(); <- ambiguous!
  for (x in u.successors, y in v.successors)
    push-constraint(x, y);
  end;
  let (larger, smaller) = if (u.successors.size > v.successors.size) values(u, v) else values(v, u) end;
  let tts = smaller.node-value.tuple-types;
  let old-size = tts.size;
  for (i from smaller.successors.size below larger.successors.size)
    let top = make(<node>, graph: u.graph, value: make(<&top-type>));
    tts := add(tts, top);
    push-constraint(larger.successors[i], top)
  end;
  smaller.node-value.tuple-types := tts;
  update-connections(smaller, old-size);
  //push-constraint for &rest == &rest?
end;


define method solve-constraint
 (t1 :: <tuple>, t2 :: <&top-type>, u :: <node>, v :: <node>, push-constraint :: <function>)
 => ()
  if (u.contains-variables?)
    format-out("solving tuple == top\n");
    u.contains-variables? := #f;
    for (u1 in u.successors)
      let w1 = make(<node>, graph: u.graph, value: make(<&top-type>));
      push-constraint(u1, w1);
    end;
  end;
end;

define method solve-constraint
  (t1 :: <limited-collection>, t2 :: <limited-collection>,
   u :: <node>, v :: <node>, push-constraint :: <function>) => ()
  push-constraint(t1.collection-class, t2.collection-class);
  push-constraint(t1.element-type, t2.element-type);
end;

define method solve-constraint
  (t1 :: <limited-collection>, t2 :: <&top-type>,
   u :: <node>, v :: <node>, push-constraint :: <function>) => ()
  if (u.contains-variables?)
    u.contains-variables? := #f;
    push-constraint(t1.collection-class, make(<node>, graph: u.graph, value: make(<&top-type>)));
    push-constraint(t1.element-type, make(<node>, graph: u.graph, value: make(<&top-type>)));
  end;
end;

define method solve-constraint
 (t1 :: type-union(<typist-type>, <&type>), t2 :: type-union(<type-variable>, <&top-type>),
  u :: <node>, v :: <node>, push-constraint :: <function>)
 => ()
  //move along
end;

define method solve-constraint
 (t1 :: <&top-type>, t2 :: <&top-type>, u :: <node>, v :: <node>, push-constraint :: <function>)
 => ()
  //move along
end;

define method solve-constraint
 (t1 :: type-union(<typist-type>, <&type>), t2 :: type-union(<typist-type>, <&type>),
  u :: <node>, v :: <node>, push-constraint :: <function>)
 => ()
  if (is-subtype?(t2, t1) | is-subtype?(t1, t2))
    //move along
  else
    error("constraint cannot be satisfied");
  end;
end;

define function order (u :: <node>, v :: <node>)
 => (first :: <node>, second :: <node>, order-matters? :: <boolean>)
  let tu = node-value(u);
  let tv = node-value(v);
  if (dynamic?(tu))
    if (instance?(tv, <type-variable>))
      values(u, v, #t);
    else
      values(v, u, #t);
    end;
  elseif (instance?(tu, <type-variable>))
    values(v, u, #t);
  elseif (instance?(tv, <type-variable>))
    values(u, v, #t);
  else
    values(u, v, #f);
  end;
end;


