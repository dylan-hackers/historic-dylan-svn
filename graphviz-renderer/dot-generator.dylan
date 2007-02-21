module: graphviz-renderer
author: Hannes Mehnert <hannes@mehnert.org>
copyright: (C) 2007,  All rights reversed.

define function generate-dot
 (graph :: <graph>, output :: <stream>, #key top-node) => ()
  let top-node = top-node | graph.nodes[0];
  write(output, "digraph G {\n");
  process-nodes(top-node, output);
  write(output, "}\n");
end;

define function process-nodes
    (top-node :: <node>, output :: <stream>) => ()
  let nodes-queue = make(<deque>);
  push(nodes-queue, top-node);
  let visited = make(<stretchy-vector>);
  while (nodes-queue.size > 0)
    let node = nodes-queue.pop;
    process-node(node, output);
    add!(visited, node);
    do(curry(push-last, nodes-queue),
       choose(method(x) ~ member?(x, nodes-queue) & ~ member?(x, visited) end,
              successors(node)))
  end;
end;

define function process-node (node :: <node>, output :: <stream>) => ()
  local method print-edge (target :: <node>)
          write(output, concatenate("  \"", node.label, "\" -> \"",
                                    target.label, "\"\n"));
        end;
  do(print-edge, node.successors);
end;

