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
/*  while (~cs.empty?)
    let constraint = cs.pop();
    let u = find(graph, constraint.left-hand-side);
    let v = find(graph, constraint.right-hand-side);
    if (u ~= v)
      let (u, v, flag) = order(u, v);
      let ute = u.type-variable.type-variable-contents;
      let vte = v.type-variable.type-variable-contents;
      if (instance?(ute, <type-estimate-limited-function>))
        if (instance?(vte, <type-estimate-limited-function>))
          cs.push(u1, v1); cs.push(u2, v2);
        elseif (vte == $type-estimate-dynamic)
          if (u.contains-variables?)
            u.contains-variables? := #f;
            w1 = vertex(stype = ?);
            w2 = vertex(stype = ?);
            cs.push(w1, u1); cs.push(w2, u2);
          end;
        end;
        tau = var, tau = dynamic -> pass
        tau = tau -> pass
        else -> error
    end;
  end; */
  let quotient-graph = create-quotient-graph(graph);
  if (acyclic?(quotient-graph))
    //u -> stype(find(u));
  else
    error("type graph not acyclic!")
  end;
end;

define function dynamic? (type :: <&type>) => (res :: <boolean>)
  type == dylan-value(#"<object>"); //or type-=? ?
end;

define function order (u :: <node>, v :: <node>)
  let tu = type-variable(u);
  let tv = type-variable(v);
  if (dynamic?(tu.type-variable-contents)) 
    if (instance?(tv.type-variable-contents, <type-variable>))
      values(u, v, #t);
    else
      values(v, u, #t);
    end;
  elseif (instance?(tu.type-variable-contents, <type-variable>))
    values(v, u, #t);
  elseif (instance?(tv.type-variable-contents, <type-variable>))
    values(u, v, #t);
  else
    values(u, v, #f);
  end;
end;


