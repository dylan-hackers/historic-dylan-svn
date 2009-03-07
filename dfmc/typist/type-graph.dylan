module: dfmc-typist

define class <graph> (<object>)
  constant slot nodes :: <stretchy-vector> = make(<stretchy-vector>);
  constant slot edges :: <stretchy-vector> = make(<stretchy-vector>);  
end;

define class <node> (<object>)
  constant slot graph :: <graph>,
    required-init-keyword: graph:;
  constant slot out-edges :: <stretchy-vector> = make(<stretchy-vector>);
  constant slot in-edges :: <stretchy-vector> = make(<stretchy-vector>);
  slot contains-variables? :: <boolean> = #f;
  constant slot node-value :: type-union(<type-variable>, <&type>),
    required-init-keyword: value:;
  slot node-rank :: <integer> = 0;
  slot representative :: <node>;
  constant slot node-id :: <integer>,
    init-function: next-computation-id;
end;

define method make (class == <node>, #rest init-args, #key, #all-keys)
 => (res :: <node>)
  let n = next-method();
  n.representative := n;
  add!(n.graph.nodes, n);
  debug-types(#"new-type-node", n, n.node-value);
  maybe-setup-connections(n, n.node-value);
  //find representative!
  n;
end;

define generic maybe-setup-connections (n :: <node>, t :: type-union(<type-variable>, <&type>));

define method maybe-setup-connections (n :: <node>, tv :: <type-variable>)
  maybe-setup-connections(n, tv.type-variable-contents);
end;

define method maybe-setup-connections (n :: <node>, t :: <&type>)
end;

define method maybe-setup-connections (n :: <node>, lft :: <&limited-function-type>)
  //actually, also function-types, but I don't expect any of those here
  if (lft.argument-types.size == 1)
    connect(n, lft.argument-types[0]);
  end;
  if (lft.value-types.size == 1)
    connect(n, lft.value-types[0]);
  end;
end;

define class <edge> (<object>)
  constant slot graph :: <graph>, required-init-keyword: graph:;
  constant slot edge-source :: <node>, required-init-keyword: source:;
  constant slot edge-target :: <node>, required-init-keyword: target:;
  constant slot constraint-edge? :: <boolean> = #f, init-keyword: constraint:;
end;

define method make (class == <edge>, #rest init-args, #key graph, source, target, #all-keys)
 => (res :: <edge>)
  let e = next-method();
  add!(graph.edges, e);
  add!(source.out-edges, e);
  add!(target.in-edges, e);
  debug-types(#"connect", e.edge-source, e.edge-target, (e.constraint-edge? & "constraint") | "");
  e;
end;

define function connect (source :: <node>, target :: <node>) => ()
  unless (source.graph == target.graph)
    error("source and target have to be in the same graph!");
  end;
  make(<edge>, graph: source.graph, source: source, target: target);
end;

define function constraint-connect (source :: <node>, target :: <node>) => ()
  unless (source.graph == target.graph)
    error("source and target have to be in the same graph!");
  end;
  make(<edge>, graph: source.graph, source: source, target: target, constraint: #t);
end;

define function remove-edge (source :: <node>, target :: <node>) => ()
  unless (source.graph == target.graph)
    error("source and target have to be in the same graph!");
  end;
  let e = choose(compose(curry(\=, target), edge-target),
                 source.out-edges);
  if (e.size == 1)
    let edge = e[0];
    remove!(source.out-edges, edge);
    remove!(target.in-edges, edge);
    remove!(source.graph.edges, edge);
    debug-types(#"disconnect", source, target);
  end;
end;

define function find (value :: <node>) => (res :: <node>)
  (value.representative == value & value)
    | (value.representative := find(value.representative));
end;

define method deep-copy-node
 (type :: type-union(<&type>, <type-variable>), graph :: <graph>)
 => (res :: <node>)
  make(<node>, graph: graph, value: type);
end;

define method deep-copy-node
 (type :: <&limited-function-type>, graph :: <graph>)
 => (res :: <node>)
  let t = make(<&limited-function-type>,
               arguments: deep-copy-node(type.argument-types, graph),
               values: deep-copy-node(type.value-types, graph));
  make(<node>, graph: graph, value: t);
end;

define method deep-copy-node
  (nodes :: <collection>, graph :: <graph>)
 => (res :: <collection>)
  map(compose(rcurry(deep-copy-node, graph), node-value), nodes);
end;

define function create-quotient-graph (g :: <graph>) => (res :: <graph>)
  dynamic-bind(*typist-visualize* = #f)
    let g* = make(<graph>);
    let rep-nodes = choose(method(v) find(v) == v end, g.nodes);
    let vs = deep-copy-node(rep-nodes, g*);
    for (edge in g.edges)
      let u = edge.edge-source;
      let v = edge.edge-target;
      if (find(u) == u)
        connect(choose-by(curry(\=, u), rep-nodes, vs)[0],
                choose-by(curry(\=, find(v)), rep-nodes, vs)[0]);
      end;
    end;
    g*;
  end;
end;

define function depth-first-search (graph :: <graph>, enter :: <function>, leave :: <function>)
  let colors = make(<table>);
  for (n in graph.nodes)
    colors[n] := #"white";
  end;

  local method visit (n :: <node>)
          colors[n] := #"grey";
          enter(n);
          for (next in map(edge-target, n.out-edges))
            if (colors[next] == #"white")
              visit(next);
            end;
          end;
          colors[n] := #"black";
          leave(n);
        end;

  for (n in graph.nodes)
    if (colors[n] == #"white")
      visit(n);
    end;
  end;
end;

define function acyclic? (graph :: <graph>) => (res :: <boolean>)
  //can be done in O(n + e), is currently O(n + e) + O(e)
  let top-sort = make(<table>);
  let i = graph.nodes.size;
  depth-first-search(graph,
                     method(x) end,
                     method(x)
                       top-sort[x] := i;
                       i := i - 1;
                     end);
  block(ret)
    for (edge in graph.edges)
      if (top-sort[edge.edge-source] > top-sort[edge.edge-target])
        ret(#f);
      end;
    end;
    #t;
  end;
end;

define function graph-union (u :: <node>, v :: <node>, order-matters? :: <boolean>) => ()
  if (order-matters?)
    if (u.node-rank == v.node-rank)
      u.node-rank := u.node-rank + 1;
    end;
    v.representative := u;
  elseif (u.node-rank > v.node-rank)
    v.representative := u;
  else
    u.representative := v;
    if (u.node-rank == v.node-rank)
      v.node-rank := v.node-rank + 1;
    end;
  end;
end;

/*
begin
  let g = make(<graph>);
  let tv = make(<type-variable>);
  let n1 = make(<node>, graph: g, tv: tv);
  let n2 = make(<node>, graph: g, tv: tv);
  let n3 = make(<node>, graph: g, tv: tv);
  connect(n1, n2);
  connect(n2, n3);
  format-out("g is %=\n", acyclic?(g));
  connect(n3, n1);
  format-out("g is %=\n", acyclic?(g));
end;
*/
