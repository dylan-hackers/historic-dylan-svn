Module:    graphviz-renderer
Synopsis:  We want to see graphs
Author:    Andreas Bogk, Hannes Mehnert
Copyright: (C) 2005,  All rights reversed.

define sealed class <graph> (<object>)
  constant slot nodes :: <stretchy-vector> = make(<stretchy-vector>);
  constant slot edges :: <stretchy-vector> = make(<stretchy-vector>);
end;

define sealed class <node> (<object>)
  constant slot graph, required-init-keyword: graph:;
  constant slot label :: <string> = "", init-keyword: label:;
  constant slot outgoing-edges :: <stretchy-vector> = make(<stretchy-vector>);
  constant slot incoming-edges :: <stretchy-vector> = make(<stretchy-vector>);
  constant slot attributes :: <string-table> = make(<string-table>);
end;

define sealed class <edge> (<object>)
  constant slot graph, required-init-keyword: graph:;
  constant slot label :: <string> = "", init-keyword: label:;
  constant slot source :: <node>, required-init-keyword: source:;
  constant slot target :: <node>, required-init-keyword: target:;
end;

define function create-node (graph :: <graph>, #key label)
 => (node :: <node>)
  let node = make(<node>,
                  graph: graph,
                  label: label | integer-to-string(graph.nodes.size));
  add!(graph.nodes, node);
  node
end;

define function create-edge
 (graph :: <graph>, source :: <node>, target :: <node>, #key label)
 => (edge :: <edge>);
  let edge = make(<edge>,
                  graph: graph,
                  source: source,
                  target: target,
                  label: label | integer-to-string(graph.edges.size));
  add!(graph.edges, edge);
  add!(source.outgoing-edges, edge);
  add!(target.incoming-edges, edge);
  edge
end;

define function maybe-create-nodes (graph :: <graph>, pres :: <collection>)
 => (res :: <collection>)
  let all = graph.nodes;
  let nodes-to-connect = choose-by(rcurry(member?, pres, test: \=),
                                   map(label, all),
                                   all);
  let missing-nodes = choose(complement(curry(find-node, graph)), pres);
  let new-nodes = map(curry(create-node, graph, label:), missing-nodes);
  concatenate(new-nodes, nodes-to-connect);
end;

define function find-node (graph :: <graph>, name :: <string>)
 => (res :: false-or(<node>))
  let res = choose(compose(curry(\=, name), label), graph.nodes);
  if (res & res.size = 1)
    res[0];
  end;
end;

define function add-predecessors (node :: <node>, pres :: <collection>) => ()
  let nodes-to-connect = maybe-create-nodes(node.graph, pres);
  map(curry(create-edge, node.graph, node), nodes-to-connect);
end;

define function add-successors (node :: <node>, succs :: <collection>) => ()
  let nodes-to-connect = maybe-create-nodes(node.graph, succs);
  map(rcurry(curry(create-edge, node.graph), node), nodes-to-connect);
end;

define function adjacent-edges (node :: <node>)
 => (edges :: <collection>);
  concatenate(node.incoming-edges, node.outgoing-edges)
end;

define function remove-edge (graph :: <graph>,
                           edge :: <edge>)
  remove!(edge.source.outgoing-edges, edge);
  remove!(edge.target.incoming-edges, edge);
  remove!(graph.edges, edge);
end;

define function predecessors (node :: <node>)
 => (predecessors :: <collection>);
  map(source, node.incoming-edges)
end;

define function successors (node :: <node>)
 => (predecessors :: <collection>);
  map(target, node.outgoing-edges)
end;

define function neighbours (node :: <node>)
 => (predecessors :: <collection>);
  concatenate(node.predecessors, node.successors)
end;
