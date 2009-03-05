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
  let cs = copy-dynamic(graph, constraints);
  for (node in graph.nodes)
    node.contains-variables? := #t;
  end;
  while (~cs.empty?)
    debug-types(#"relayouted");
    let constraint = cs.pop;
    let u = find(graph, constraint.left-hand-side);
    let v = find(graph, constraint.right-hand-side);
    if (u ~= v)
      let (u, v, flag) = order(u, v);
      let ute = u.node-value;
      let vte = v.node-value;
      graph-union(u, v, flag);
      if (instance?(ute, <&limited-function-type>))
        if (instance?(vte, <&limited-function-type>))
          remove-edge(u, v);
          //this is easier now!
          for (u1 in u.out-edges, v1 in v.out-edges)
            push-last(cs, make(<equality-constraint>,
                               left: u1.edge-target,
                               right: v1.edge-target));
          end;
        elseif (dynamic?(vte))
          if (u.contains-variables?)
            u.contains-variables? := #f;
            //w1 = vertex(stype = ?);
            //w2 = vertex(stype = ?);
            //cs.push(w1, u1); cs.push(w2, u2);
          end;
        end;
      else
        //tau = var, tau = dynamic -> pass
        //tau = tau -> pass
        //else
        //error("constraints cannot be satisfied");
      end;
    end;
  end;
  //let quotient-graph = create-quotient-graph(graph);
  //if (acyclic?(quotient-graph))
    //u -> stype(find(u));
  //else
  //  error("type graph contains cycles!")
  //end;
end;

define function copy-dynamic (graph :: <graph>, constraints :: <collection>)
 => (res :: <deque>)
  as(<deque>, constraints);
  //XXX: FIXME!
end;

define function dynamic? (type :: <&type>) => (res :: <boolean>)
  type == dylan-value(#"<object>"); //or type-=? ?
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


