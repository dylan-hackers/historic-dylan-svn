Module:    dylan-user
Synopsis:  helps to build class graphs
Author:    Hannes Mehnert
Copyright: (C) 2009.  All rights reversed.

define library class-graph-builder
  use dylan;
  use graphviz-renderer;
  use system, import: { file-system, operating-system };
  use io, import: { format-out };
  use environment-protocols;
  use dfmc-environment-projects;
  use common-dylan, import: { common-dylan };

  export class-graph-builder;
end library class-graph-builder;

define module class-graph-builder
  use dylan;
  use graphviz-renderer,
    import: { generate-dot, add-successors, <graph>, find-node, create-node };
  use file-system, import: { with-open-file, temp-directory };
  use format-out, import: { format-out };
  use operating-system, import: { application-arguments };
  use environment-protocols,
    import: { find-project, open-project-compiler-database,
              parse-project-source, project-library, library-modules,
              find-environment-object, environment-object-display-name,
              class-direct-subclasses };
  use common-dylan, import: { split };
  use dfmc-environment-projects, import: { };

end module class-graph-builder;
