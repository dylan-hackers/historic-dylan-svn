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
  constant slot node-value :: <&type>,
    required-init-keyword: value:;
  slot node-rank :: <integer> = 0;
  constant slot node-id :: <integer>,
    init-function: next-computation-id;
end;

define method make (class == <node>, #rest init-args, #key, #all-keys)
 => (res :: <node>)
  let n = next-method();
  debug-types(#"new-type-node", n, n.node-value);
  add!(n.graph.nodes, n);
  maybe-setup-connections(n, n.node-value);
  n;
end;

define function representative (n :: <node>) => (res :: <node>)
  let rep = choose(rcurry(instance?, <representative-edge>), n.out-edges);
  if (rep.size == 1)
    rep.first.edge-target;
  elseif (rep.size == 0)
    n
  else
    error("multiple representative edges")
  end;
end;

define function representative-setter (new :: <node>, n :: <node>) => (res :: <node>)
  let old = n.representative;
  unless (old == new)
    unless (old == n)
      remove-edge(choose(rcurry(instance?, <representative-edge>), n.out-edges).first);
    end;
    connect(n, new, edge-type: <representative-edge>);
    let in-rep = choose(rcurry(instance?, <representative-edge>), n.in-edges);
    map(compose(curry(representative-setter, new), edge-source), in-rep);
  end;
  new;
end;

define function representative? (n :: <node>) => (result :: <boolean>)
  any?(rcurry(instance?, <representative-edge>), n.in-edges);
end;

define function successors (n :: <node>) => (res :: <collection>)
  map(edge-target, choose(rcurry(instance?, <graph-edge>), n.out-edges));
end;

define generic maybe-setup-connections (n :: <node>, t :: <&type>);

define method maybe-setup-connections (n :: <node>, tv :: <&type-variable>)
  maybe-setup-connections(n, tv.^type-variable-contents);
end;

define method maybe-setup-connections (n :: <node>, t :: <&type>)
end;

define method maybe-setup-connections (n :: <node>, arrow :: <&arrow-type>)
  connect(n, arrow.^arguments);
  connect(n, arrow.^values);
end;

define method maybe-setup-connections (n :: <node>, tt :: <&tuple-type>)
  do(curry(connect, n), tt.^tuple-types);
end;

define method maybe-setup-connections (n :: <node>, c :: <&limited-coll-type>)
  connect(n, c.^coll-class);
  connect(n, c.^coll-element-type);
end;

define abstract class <edge> (<object>)
  constant slot graph :: <graph>, required-init-keyword: graph:;
  constant slot edge-source :: <node>, required-init-keyword: source:;
  constant slot edge-target :: <node>, required-init-keyword: target:;
end;

define class <graph-edge> (<edge>)
end;

define class <constraint-edge> (<edge>)
end;

define class <representative-edge> (<edge>)
end;

define method initialize (edge :: <edge>, #rest init-args, #key, #all-keys)
  next-method();
  add!(edge.graph.edges, edge);
  add!(edge.edge-source.out-edges, edge);
  add!(edge.edge-target.in-edges, edge);
  debug-types(#"connect", edge.edge-source, edge.edge-target,
              as(<symbol>, edge.object-class.debug-name));
end;

define function connect (source :: <node>, target :: <node>, #key edge-type = <graph-edge>) => ()
  unless (source.graph == target.graph)
    error("source and target have to be in the same graph!");
  end;
  make(edge-type, graph: source.graph, source: source, target: target);
end;

define function disconnect (source :: <node>, target :: <node>, #key edge-type) => ()
  unless (source.graph == target.graph)
    error("source and target have to be in the same graph!");
  end;
  let e = choose(compose(curry(\=, target), edge-target),
                 source.out-edges);
  if (edge-type)
    e := choose(rcurry(instance?, edge-type), e);
  end;
  if (e.size == 1)
    remove-edge(e.first);
  elseif (e.size == 0)
    error("tried to disconnect %= from %=, which were not connected\n",
          source, target);
  else
    error("multi-edges from %= to %=\n", source, target);
  end;
end;

define function remove-edge (edge :: <edge>) => ()
  let source = edge.edge-source;
  let target = edge.edge-target;
  remove!(source.out-edges, edge);
  remove!(target.in-edges, edge);
  remove!(source.graph.edges, edge);
  debug-types(#"disconnect", source, target, as(<symbol>, edge.object-class.debug-name));
end;
define function degree (n :: <node>) => (int :: <integer>)
  n.out-edges.size + n.in-edges.size;
end;

define function remove-node (n :: <node>) => ()
  for (edge in n.out-edges.copy-sequence)
    remove-edge(edge);
    if (edge.edge-target.in-edges.size == 0)
      remove-node(edge.edge-target);
    end;
  end;
  if (n.degree == 0)
    remove!(n.graph.nodes, n);
    debug-types(#"remove-node", n);
  end;
end;

define function update-connections (n :: <node>, f :: <integer>) => ()
  for (succ from f below n.node-value.^tuple-types.size)
    let s = n.node-value.^tuple-types[succ];
    connect(n, s);
  end;
end;

define function find (value :: <node>) => (res :: <node>)
  (value.representative == value & value)
    | (value.representative := find(value.representative));
end;

define method deep-copy-node
 (type :: <&type>, graph :: <graph>)
 => (res :: <node>)
  make(<node>, graph: graph, value: type);
end;

define method deep-copy-node
 (type :: <&tuple-type>, graph :: <graph>)
 => (res :: <node>)
  make(<node>, graph: graph,
       value: make(<&tuple-type>,
                   tuples: map(compose(rcurry(deep-copy-node, graph), node-value),
                               type.^tuple-types)));
end;

define method deep-copy-node
 (type :: <&arrow-type>, graph :: <graph>)
 => (res :: <node>)
  let t = make(<&arrow-type>,
               arguments: deep-copy-node(type.^arguments.node-value, graph),
               values: deep-copy-node(type.^values.node-value, graph));
  make(<node>, graph: graph, value: t);
end;

define method deep-copy-node
 (type :: <&limited-coll-type>, graph :: <graph>)
 => (res :: <node>)
  let t = make(<&limited-coll-type>,
               class: deep-copy-node(type.^coll-class.node-value, graph),
               element-type: deep-copy-node(type.^coll-element-type.node-value, graph));
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

define function graph-union (u :: <node>, v :: <node>, order-matters? :: <boolean>) => (rep :: <node>)
  if (order-matters?)
    if (u.node-rank == v.node-rank)
      u.node-rank := u.node-rank + 1;
    end;
    v.representative := u;
  else
    //hah, we can decide on the order ourselves
    //done by least upper bound:
    //basically top loses always against a real type
    //and real types just use subtyping relationship
    if (u.node-value.dynamic?)
      v.node-rank := max(u.node-rank, v.node-rank) + 1;
      u.representative := v;
    elseif (v.node-value.dynamic?)
      u.node-rank := max(u.node-rank, v.node-rank) + 1;
      v.representative := u;
    elseif (is-subtype?(u, v))
      if (is-subtype?(v, u) & v.node-rank > u.node-rank)
        u.representative := v;
      else
        u.node-rank := max(u.node-rank, v.node-rank) + 1;
        v.representative := u;
      end;
    elseif (is-subtype?(v, u))
      v.node-rank := max(u.node-rank, v.node-rank) + 1;
      u.representative := v;
    elseif (v.rest-value?)
      u.node-rank := max(u.node-rank, v.node-rank) + 1;
      v.representative := u;
    elseif (u.rest-value?)
      v.node-rank := max(u.node-rank, v.node-rank) + 1;
      u.representative := v;
    elseif (u.node-rank > v.node-rank) //default case from gtubi-paper (needed?)
      v.representative := u;
    else
      if (u.node-rank == v.node-rank)
        v.node-rank := v.node-rank + 1;
      end;
      u.representative := v;
    end;
  end;
end;

define generic is-subtype? (t1, t2) => (res :: <boolean>);

define method is-subtype? (t1 :: <object>, t2 :: <object>) => (res == #f)
  #f;
end;

define method is-subtype? (t1 :: <&type>, t2 :: <&type>) => (res :: <boolean>)
  ^subtype?(t1.model-type, t2.model-type);
end;

define method is-subtype? (t1 :: <node>, t2 :: <&type>) => (res :: <boolean>)
  is-subtype?(t1.node-value, t2);
end;

define method is-subtype? (t1 :: <&type>, t2 :: <node>) => (res :: <boolean>)
  is-subtype?(t1, t2.node-value);
end;

define method is-subtype? (t1 :: <node>, t2 :: <node>) => (res :: <boolean>)
  is-subtype?(t1.node-value, t2.node-value);
end;

//don't care about those types, they'll be taken care by the constraint solver
//(by propagating constraints to the leaves)
define method is-subtype? (t1 :: type-union(<&arrow-type>, <&tuple-type>, <&limited-coll-type>),
                           t2 :: type-union(<&arrow-type>, <&tuple-type>, <&limited-coll-type>)) => (res :: <boolean>)
  #f;
end;
/*
begin
  let g = make(<graph>);
  let tv = make(<&type-variable>);
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
