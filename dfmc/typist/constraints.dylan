module: dfmc-typist

define abstract class <constraint> (<object>)
  constant slot left-hand-side :: type-union(<&type>, <type-variable>),
    required-init-keyword: left:;
  constant slot right-hand-side :: type-union(<&type>, <type-variable>),
    required-init-keyword: right:;
end;

define class <equality-constraint> (<constraint>)
end;

define function insert-constraint-into-type-graph (graph :: <graph>, c :: <constraint>)
  let left = insert-type-estimate(graph, c.left-hand-side);
  let right = insert-type-estimate(graph, c.right-hand-side);
  constraint-connect(left, right);
end;

define function solve (constraints :: <collection>, type-env :: <type-environment>)
  let graph = make(<graph>);
  do(curry(insert-constraint-into-type-graph, graph),
     constraints);
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
      let ute = u.type-variable;
      let vte = v.type-variable;
      graph-union(u, v, flag);
      if (instance?(ute, <&limited-function-type>))
        if (instance?(vte, <&limited-function-type>))
          remove-edge(u, v);
          let u1 = ute.^limited-function-argument-types[0];
          let u2 = ute.^limited-function-return-values[0];
          let v1 = vte.^limited-function-argument-types[0];
          let v2 = vte.^limited-function-return-values[0];
          push-last(cs, make(<equality-constraint>, left: u1, right: v1));
          constraint-connect(find(graph, u1), find(graph, v1));
          push-last(cs, make(<equality-constraint>, left: u2, right: v2));
          constraint-connect(find(graph, u2), find(graph, v2));
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
end;

define function dynamic? (type :: <&type>) => (res :: <boolean>)
  type == dylan-value(#"<object>"); //or type-=? ?
end;

define function order (u :: <node>, v :: <node>)
  let tu = type-variable(u);
  let tv = type-variable(v);
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


