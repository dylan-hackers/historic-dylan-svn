module: graphviz-renderer
author: Andreas Bogk and Hannes Mehnert
copyright: 2005-2011 Andreas Bogk and Hannes Mehnert. All rights reserved.
license: see license.txt in this directory

define function generate-gml
 (graph :: <graph>, output :: <stream>, #key top-node) => ()
  write(output, "graph [\n");
  if (graph.attributes.size > 0)
    for (value keyed-by name in graph.attributes)
      write(output, concatenate("  ", name, " \"", value ,"\";\n"));
    end for;
  end if;
  do(rcurry(gml-process-node, output), graph.nodes);
  do(rcurry(gml-process-edge, output), graph.edges);
  write(output, "]\n");
end;

define function gml-process-node (node :: <node>, output :: <stream>) => ()
  write(output, "  node [\n");
  write(output, concatenate("    id ", integer-to-string(node.id), "\n"));
  write(output, concatenate("    label \"", node.label, "\""));
  write(output, "  ]\n");
end;

define function gml-process-edge (edge :: <edge>, output :: <stream>) => ()
  write(output, "  edge [\n");
  write(output, concatenate("    source ", integer-to-string(edge.source.id), "\n"));
  write(output, concatenate("    target ", integer-to-string(edge.target.id), "\n"));
  write(output, "  ]\n");
end;

