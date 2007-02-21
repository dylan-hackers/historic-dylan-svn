module: graphviz-renderer
author: Hannes Mehnert <hannes@mehnert.org>
copyright: (C) 2007,  All rights reversed.


define function generate-graph (graph :: <graph>, top :: <node>)
 => (result-filename :: <string>)
  let file-prefix = concatenate("/tmp/foo", integer-to-string(random(10000)));
  let dot = concatenate(file-prefix, ".dot");
  let png = concatenate(file-prefix, ".png");
  with-open-file (stream = dot, direction: #"output")
    generate-dot(graph, stream, top-node: top);
  end;
  run-application(concatenate("dot -Tpng -o ", png, " ", dot));
  png;
end;

