module: dylan-user
author: Hannes Mehnert <hannes@mehnert.org>
copyright: (C) 2007,  All rights reversed.

define library graphviz-renderer
  use dylan;
  use system;
  use io;
  use common-dylan;
  export graphviz-renderer;
end;

define module graphviz-renderer
  use dylan;
  use operating-system;
  use file-system;
  use format;
  use streams;
  use standard-io;
  use common-dylan;
  use simple-random;

  export <graph>, <node>, <edge>,
    create-node, create-edge,
    generate-dot, generate-graph,
    find-node, add-successors,
    add-predecessors, attributes,
    nodes, edges, neighbours,
    label;
end;
