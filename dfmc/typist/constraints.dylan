module: dfmc-typist

define abstract class <constraint> (<object>)
  constant slot left-hand-side :: <node>,
    required-init-keyword: left:;
  constant slot right-hand-side :: <node>,
    required-init-keyword: right:;
end;

define class <equality-constraint> (<constraint>)
end;

define method make (class :: subclass(<constraint>), #rest init-args, #key, #all-keys) => (res :: <constraint>)
  let c = next-method();
  constraint-connect(c.left-hand-side, c.right-hand-side);
  c;
end;

define function solve (graph :: <graph>, constraints :: <collection>, type-env :: <type-environment>)
 => ()
  let cs = copy-dynamic(constraints);
  for (node in graph.nodes)
    node.contains-variables? := #t;
  end;
  while (~cs.empty?)
    debug-types(#"relayouted");
    let constraint = cs.pop;
    let u = find(constraint.left-hand-side);
    let v = find(constraint.right-hand-side);
    if (u ~= v)
      let (u, v, flag) = order(u, v);
      let ute = u.node-value;
      let vte = v.node-value;
      graph-union(u, v, flag);
      if (ute.arrow?)
        if (vte.arrow?)
          remove-edge(u, v);
          for (u1 in u.out-edges, v1 in v.out-edges)
            push-last(cs, make(<equality-constraint>,
                               left: u1.edge-target,
                               right: v1.edge-target));
          end;
        elseif (vte.dynamic?)
          if (u.contains-variables?)
            remove-edge(u, v);
            u.contains-variables? := #f;
            for (u1 in u.out-edges)
              let w1 = make(<node>, graph: graph, value: dylan-value(#"<object>"));
              push-last(cs, make(<equality-constraint>,
                                 left: w1,
                                 right: u1.edge-target));
            end;
          end;
        end;
      else
        if (instance?(vte, <type-variable>) |
            vte.dynamic? |
            vte = ute)
          //pass;
        else
          error("constraint %= cannot be satisfied", constraint);
        end;
      end;
    end;
  end;
  let quotient-graph = create-quotient-graph(graph);
  if (acyclic?(quotient-graph))
    do(method(x)
         if (instance?(x.node-value, <type-variable>))
           let rep-type = x.find.node-value;
           if (instance?(rep-type, <&type>))
             format-out("changed TV %= to contain type %= now\n", x.node-value.get-id, rep-type);
             x.node-value.type-variable-contents := rep-type;
           end
         end
       end, graph.nodes);
  else
    error("type graph %= contains cycles!", quotient-graph)
  end;
end;

define function copy-dynamic (constraints :: <collection>)
 => (res :: <deque>)
  as(<deque>,
     map(method(x)
           let left = copy-dyn(x.left-hand-side);
           let right = copy-dyn(x.right-hand-side);
           if (left ~= x.left-hand-side | right ~= x.right-hand-side)
             //retract old constraint?
             make(<equality-constraint>, left: left, right: right)
           else
             x
           end;
         end, constraints));
end;

define method copy-dyn (n :: <node>)
  if (dynamic?(n.node-value))
    make(<node>, graph: n.graph, value: n.node-value);
  elseif (arrow?(n.node-value))
    let args = map(copy-dyn, n.node-value.argument-types);
    let values = map(copy-dyn, n.node-value.value-types);
    if (args ~= n.node-value.argument-types | values ~= n.node-value.value-types)
      //retract old arrow node?
      make(<node>, graph: n.graph,
           value: make(<&limited-function-type>,
                       arguments: args,
                       values: values));
    else
      n
    end;
  else
    n
  end;
end;

define function order (u :: <node>, v :: <node>)
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


