module: graphviz-renderer
author: Hannes Mehnert <hannes@mehnert.org>
copyright: (C) 2007,  All rights reversed.

define function generate-dot
 (graph :: <graph>, output :: <stream>, #key top-node) => ()
  write(output, "digraph G {\n");
  if (graph.attributes.size > 0)
    for (value keyed-by name in graph.attributes)
      write(output, concatenate("  ", name, " = \"", value ,"\";\n"));
    end for;
  end if;
  for (node in graph.nodes)
    process-node(node, output);
  end;
  write(output, "}\n");
end;

define function process-nodes
    (top-node :: <node>, output :: <stream>) => ()
  let nodes-queue = make(<deque>);
  push(nodes-queue, top-node);
  do(curry(push-last, nodes-queue), predecessors(top-node));
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
  local method print-edge (edge :: <edge>)
          write(output, concatenate("  \"", node.label, "\" -> \"",
                                    edge.target.label, "\""));
	  print-attributes(output, edge.attributes);
	  write(output, ";\n");
        end;
  write(output, concatenate("  \"", node.label, "\""));
  print-attributes(output, node.attributes);
  write(output, ";\n");
  do(print-edge, node.outgoing-edges);
end;

define function print-attributes (stream :: <stream>, attributes :: <string-table>)
  if (attributes.size > 0)
    let attrs = make(<stretchy-vector>);
    for (ele in key-sequence(attributes))
      add!(attrs, concatenate(ele, " = \"", attributes[ele], "\""));
    end;
    attrs := reduce1(method(x, y) concatenate(x, ",", y) end, attrs);
    write(stream, concatenate(" [", attrs, "]"))
  end;
end;
