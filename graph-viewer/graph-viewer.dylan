Module:    graph-viewer
Synopsis:  We want to see graphs
Author:    Andreas Bogk, Hannes Mehnert
Copyright: (C) 2005,  All rights reserved.

define sealed class <graph> (<object>)
  slot nodes :: <stretchy-vector> = make(<stretchy-vector>);
  slot edges :: <stretchy-vector> = make(<stretchy-vector>);
end;

define sealed class <node> (<object>)
  slot graph, required-init-keyword: graph:;
  slot label = "", init-keyword: label:;
  slot outgoing-edges :: <stretchy-vector> = make(<stretchy-vector>);
  slot incoming-edges :: <stretchy-vector> = make(<stretchy-vector>);
end;

define sealed class <edge> (<object>)
  slot graph, required-init-keyword: graph:;
  slot label = "", init-keyword: label:;
  slot source :: <node>, required-init-keyword: source:;
  slot target :: <node>, required-init-keyword: target:;
end;

define method create-node (graph :: <graph>, #key label)
 => (node :: <node>)
  let node = make(<node>,
                  graph: graph,
                  label: label | graph.nodes.size);
  add!(graph.nodes, node);
  node
end;

define method create-edge (graph :: <graph>,
                           source :: <node>,
                           target :: <node>,
                           #key label)
 => (edge :: <edge>);
  let edge = make(<edge>,
                  graph: graph,
                  source: source,
                  target: target,
                  label: label | graph.edges.size);
  add!(graph.edges, edge);
  add!(source.outgoing-edges, edge);
  add!(target.incoming-edges, edge);
  edge
end;

define method adjacent-edges (node :: <node>)
 => (edges :: <collection>);
  concatenate(node.incoming-edges, node.outgoing-edges)
end;

define method remove-edge (graph :: <graph>,
                           edge :: <edge>)
  remove!(edge.source.outgoing-edges, edge);
  remove!(edge.target.incoming-edges, edge);
  remove!(graph.edges, edge);
end;

/*
define method remove-node (graph :: <graph>,
                           node :: <node>)
  do(curry(remove-edge, graph), node.adjacent-edges);
  remove!(graph.nodes, node)
end;
*/

define method predecessors (node :: <node>)
 => (predecessors :: <collection>);
  map(source, node.incoming-edges)
end;

define method successors (node :: <node>)
 => (predecessors :: <collection>);
  map(target, node.outgoing-edges)
end;

define method neighbours (node :: <node>)
 => (predecessors :: <collection>);
  concatenate(node.predecessors, node.successors)
end;

define method incoming-degree (node :: <node>)
 => (in-degree :: <integer>)
  node.incoming-edges.size
end;

define method outgoing-degree (node :: <node>)
 => (out-degree :: <integer>)
  node.outgoing-edges.size
end;

define method degree (node :: <node>)
 => (degree :: <integer>)
  node.incoming-degree + node.outgoing-degree
end;




 


