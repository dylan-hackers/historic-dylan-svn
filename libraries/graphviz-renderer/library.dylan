module: dylan-user
author: Andreas Bogk and Hannes Mehnert
copyright: 2005-2011 Andreas Bogk and Hannes Mehnert. All rights reserved.
license: see license.txt in this directory

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
  use common-dylan,
    exclude: { format-to-string };
  use simple-random;

  export <graph>, <node>, <edge>,
    create-node, create-edge,
    generate-dot, generate-gml,
    generate-graph,
    find-node, find-node!,
    add-successors,
    add-predecessors, attributes,
    nodes, edges, neighbours,
    label;
end;
