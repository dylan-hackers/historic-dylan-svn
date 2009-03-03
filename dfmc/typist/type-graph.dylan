module: dfmc-typist

define generic insert-type-estimate (graph :: <graph>, te :: type-union(<&type>, <type-variable>))
 => (node :: <node>);

define method insert-type-estimate (graph :: <graph>, te :: type-union(<&type>, <type-variable>))
 => (res :: <node>)
  find(graph, te) |
    make(<node>, graph: graph, tv: te);
end;

define method insert-type-estimate (graph :: <graph>, te :: <&limited-function-type>)
 => (res :: <node>)
  let args = insert-type-estimate(graph, te.^limited-function-argument-types[0]);
  let vals = insert-type-estimate(graph, te.^limited-function-return-values[0]);
  let arrow = make(<node>, graph: graph, tv: te);
  connect(arrow, args);
  connect(arrow, vals);
  arrow;
end;

define function copy-dynamic (graph :: <graph>, constraints :: <collection>)
 => (res :: <collection>)

end;

define class <graph> (<object>)
  constant slot nodes :: <stretchy-vector> = make(<stretchy-vector>);
  constant slot edges :: <stretchy-vector> = make(<stretchy-vector>);  
end;

define class <node> (<object>)
  constant slot graph :: <graph>,
    required-init-keyword: graph:;
  constant slot type-variable :: type-union(<&type>, <type-variable>),
    required-init-keyword: tv:;
  constant slot out-edges :: <stretchy-vector> = make(<stretchy-vector>);
  constant slot in-edges :: <stretchy-vector> = make(<stretchy-vector>);
  slot contains-variables? :: <boolean> = #f;
end;

define method make (class == <node>, #rest init-args, #key graph, #all-keys)
 => (res :: <node>)
  let n = next-method();
  add!(graph.nodes, n);
  n;
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

define function find (g :: <graph>, value :: type-union(<&type>, <type-variable>)) => (res :: false-or(<node>))
  block(ret)
    for (node in g.nodes)
      if (node.type-variable == value)
        ret(node)
      end
    end
  end
end;

define function create-quotient-graph (graph :: <graph>) => (res :: <graph>)

end;

define function acyclic? (graph :: <graph>) => (res :: <boolean>)

end;
