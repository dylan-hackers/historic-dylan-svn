module: dfmc-typist

define program-warning <type-unification-failed>
  slot condition-type-estimate1,
    required-init-keyword: type-estimate1:;
  slot condition-type-estimate2,
    required-init-keyword: type-estimate2:;
  format-string
    "Could not unify %s and %s.";
  format-arguments type-estimate1, type-estimate2;
end;

define thread variable *origin* = #f;

define function solve (type-env :: <type-environment>)
 => ()
  let graph = type-env.type-graph;
  //do(copy-dynamic, graph.graph-nodes);
  let constraints = graph.type-constraints;
  let cs = as(<deque>, constraints);
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
              let new-constraint = make(<constraint-edge>, graph: graph, source: l, target: r, origin: constraint);
              push-last(cs, new-constraint);
            end;
          end;
    debug-types(#"highlight-constraint", type-env, constraint.edge-source, constraint.edge-target);
    let u = find(constraint.edge-source);
    let v = find(constraint.edge-target);
    if (u ~= v)
      let (u, v, flag) = order(u, v);
      let ute = u.node-value;
      let vte = v.node-value;
      dynamic-bind(*origin* = constraint)
        graph-union(u, v, flag);
      end;
      block()
        solve-constraint(ute, vte, u, v, push-cs);
      exception (e :: <error>)
        let orig = deep-origin(constraint);
        note(<type-unification-failed>,
             source-location: dfm-source-location(orig),
             context-id:      dfm-context-id(orig),
             type-estimate1:  ute,
             type-estimate2:  vte);
      end;
    end;
    remove-edge(constraint);
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
    debug-types(#"relayouted", type-env);
  end;
  let quotient-graph = create-quotient-graph(graph);
  if (acyclic?(quotient-graph))
    let changed-vars = make(<stretchy-vector>);
    do(method(x)
         if (instance?(x.node-value, <type-variable>))
           let rep-type = x.find.node-value;
           if (instance?(rep-type, type-union(<&type>, <typist-type>)) & ~instance?(rep-type, <type-variable>))
             if (x.node-value.type-variable-contents ~== rep-type)
               //format-out("changed TV %= to contain type %= now\n", x.node-value.get-id, rep-type);
               add!(changed-vars, x.node-value);
               x.node-value.type-variable-contents := rep-type;
             end
           end
         end
       end, graph.graph-nodes);
    for (ele in type-env.key-sequence)
      let val = element(type-env, ele, default: #f);
      if (val & member?(val.node-value, changed-vars))
        debug-types(#"change-type", type-env, ele, format-to-string("%=", val.node-value.type-variable-contents.model-type));
      end;
    end;
    debug-types(#"relayouted", type-env);
  else
    error("type graph %= contains cycles!", quotient-graph)
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
 (t1 :: <arrow>, t2 :: <dynamic>, u :: <node>, v :: <node>, push-constraint :: <function>)
 => ()
  if (u.contains-variables?)
    u.contains-variables? := #f;
    for (u1 in u.successors)
      let w1 = make(<node>, graph: u.graph, value: make(<dynamic>));
      w1.contains-variables? := #f;
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
    let n = make(<node>, graph: u.graph, value: make(<dynamic>));
    n.contains-variables? := #f;
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
    let top = make(<node>, graph: u.graph, value: make(<dynamic>));
    top.contains-variables? := #f;
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
    let top = make(<node>, graph: u.graph, value: make(<dynamic>));
    top.contains-variables? := #f;
    tts := add(tts, top);
    push-constraint(larger.successors[i], top)
  end;
  smaller.node-value.tuple-types := tts;
  update-connections(smaller, old-size);
  //push-constraint for &rest == &rest?
end;


define method solve-constraint
 (t1 :: <tuple>, t2 :: <dynamic>, u :: <node>, v :: <node>, push-constraint :: <function>)
 => ()
  if (u.contains-variables?)
    //format-out("solving tuple == top\n");
    u.contains-variables? := #f;
    for (u1 in u.successors)
      let w1 = make(<node>, graph: u.graph, value: make(<dynamic>));
      w1.contains-variables? := #f;
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
  (t1 :: <limited-collection>, t2 :: <dynamic>,
   u :: <node>, v :: <node>, push-constraint :: <function>) => ()
  if (u.contains-variables?)
    u.contains-variables? := #f;
    let c = make(<node>, graph: u.graph, value: make(<dynamic>));
    c.contains-variables? := #f;
    push-constraint(t1.collection-class, c);
    let e = make(<node>, graph: u.graph, value: make(<dynamic>));
    e.contains-variables? := #f;
    push-constraint(t1.element-type, e);
  end;
end;

define method solve-constraint
 (t1 :: type-union(<typist-type>, <&type>), t2 :: type-union(<type-variable>, <dynamic>),
  u :: <node>, v :: <node>, push-constraint :: <function>)
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


