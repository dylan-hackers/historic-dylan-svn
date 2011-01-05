Module:    class-graph-builder
author: Andreas Bogk and Hannes Mehnert
copyright: 2005-2011 Andreas Bogk and Hannes Mehnert. All rights reserved.
license: see license.txt in this directory

define function callback-handler (#rest args)
end function callback-handler;

define function generate-class-graph (class-name :: <string>, project :: <string>) => (res :: <graph>)
  let project = find-project(project);
  open-project-compiler-database(project, 
                                 warning-callback: callback-handler,
                                 error-handler: callback-handler);
  parse-project-source(project);

  let library-object = project-library(project);
  let module-object
    = first(library-modules(project, project-library(project)));
  let class
    = find-environment-object(project, class-name, library: library-object, module: module-object);
  let todo = make(<deque>);
  let visited = make(<stretchy-vector>);
  push(todo, class);
  let graph = make(<graph>);

  local method get-class-name (class)
          split(environment-object-display-name(project, class, #f), ':')[0]
        end;
  while (todo.size > 0)
    let class = pop(todo);
    let class-name = get-class-name(class);
    let class-node = find-node(graph, class-name);
    unless (class-node)
      format-out("class node for %s was not found, creating\n", class-name);
      class-node := create-node(graph, label: class-name);
    end;
    add!(visited, class);
    let subclasses = class-direct-subclasses(project, class);
    format-out("subclasses for %s %=\n",
               class-name, map(get-class-name, subclasses));
    add-successors(class-node, map(get-class-name, subclasses));
    do(curry(push-last, todo),
       choose(method(x) ~ member?(x, visited) & ~ member?(x, todo) end,
              subclasses))
  end;
  graph
end;

define method main () => ()
  let args = application-arguments();
  if (args.size > 2 | args.size < 1)
    error("please run with arguments: <class> project or <class> (then dylan library will be used)");
  end;
  let project = (args.size == 1 & "dylan") | args[1];
  let class = args[0];
  let graph = generate-class-graph(class, project);
  let file-name = concatenate(as(<string>, temp-directory()),
                              copy-sequence(class, start: 1, end: class.size - 1));
  with-open-file (file = concatenate(file-name, ".dot"), direction: #"output", if-exists: #"overwrite")
    generate-dot(graph, file);
  end;
  format-out("wrote dot: %s.dot\n", file-name);
  with-open-file (file = concatenate(file-name, ".gml"), direction: #"output", if-exists: #"overwrite")
    generate-gml(graph, file);
  end;
  format-out("wrote gml: %s.gml\n", file-name);
end method main;

begin
  main();
end;
