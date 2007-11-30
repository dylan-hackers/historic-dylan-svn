module: graphviz-renderer
author: Hannes Mehnert <hannes@mehnert.org>
copyright: (C) 2007,  All rights reversed.


define function generate-graph (graph :: <graph>, top :: <node>, #key format = "svg", args = "", program = "dot")
 => (result-filename :: <string>)
  let file-prefix = concatenate("foo", integer-to-string(random(10000)));
  let dot = concatenate(file-prefix, ".dot");
  let output = concatenate(file-prefix, ".", format);
  with-open-file (stream = dot, direction: #"output")
    generate-dot(graph, stream, top-node: top);
  end;
  run-application(concatenate(program, " -T", format, " -o ", output, " ", args, " ", dot));
  output;
end;

