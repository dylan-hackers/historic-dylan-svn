Module:   code-browser
Synopsis: Brwose FD environment objects
Author:   Andreas Bogk

define thread variable *project* = #f; 

define taglib code-browser () end;

define page code-browser-page (<dylan-server-page>)
  (url: "/project",
   source: "code-browser/project.dsp")
end;

define method respond-to-get (page :: <code-browser-page>,
                              request :: <request>,
                              response :: <response>)
  let project-name = get-query-value("name");
  if(~project-name | project-name = "")
    project-name := "minimal-console-compiler";
  end if;
  dynamic-bind(*project* = find-project(project-name))
    if (*project*)
      open-project-compiler-database(*project*, 
                                     warning-callback: callback-handler,
                                     error-handler: callback-handler);
      parse-project-source(*project*);
      next-method();
    else
      application-error(format-string: "No such project %s", 
                        format-arguments: vector(project-name));
    end if;
  end;
end method respond-to-get;


define tag project-name in code-browser
  (page :: <code-browser-page>, response :: <response>)
  ()
  write(output-stream(response), *project*.project-name);
end;

define tag project in code-browser
  (page :: <code-browser-page>, response :: <response>)
  ()
  format(output-stream(response), "%=", *project*);
end;

define tag project-sources in code-browser
  (page :: <code-browser-page>, response :: <response>)
  ()
  dynamic-bind(*check-source-record-date?* = #f)
    format(response.output-stream, "<pre>\n");
    for(source in *project*.project-sources)
      block()
        format(response.output-stream,
               "<h3>%s</h3> module <strong>%s</strong>\n", 
               source-record-location(source),
               source-record-module-name(source));
        write(response.output-stream, 
              markup-dylan-source
                (as(<byte-string>, source-record-contents(source))));
      exception(e :: <condition>)
        format(response.output-stream,
               "Source for %= unavailable because of %=\n",
               source,
               e);
      end block;
    end for;
    format(response.output-stream, "</pre>\n");
  end
end;

define function markup-dylan-source(source :: <string>)
 => (processed-source :: <string>);
  regexp-replace(regexp-replace(regexp-replace(source, "&", "&amp;"), "<", "&lt;"), ">", "&gt;");
end function markup-dylan-source;

define tag project-direct-superclasses in code-browser
  (page :: <code-browser-page>, response :: <response>)
  ()
    format(response.output-stream, "<ul>\n");
    for (superclass in class-direct-superclasses(*project*, 
     find-environment-object(*project*, "<string>", 
      library: project-library(*project*), module: first(library-modules(*project*, project-library(*project*))))))
        format(response.output-stream, "<li>%s</li>\n", markup-dylan-source(environment-object-display-name(*project*, superclass, #f)));
    end for;
    format(response.output-stream, "</ul>\n");
end;

define tag project-direct-subclasses in code-browser
  (page :: <code-browser-page>, response :: <response>)
  ()
    format(response.output-stream, "<ul>\n");
    for (subclass in class-direct-subclasses(*project*,
     find-environment-object(*project*, "<string>",
      library: project-library(*project*), module: first(library-modules(*project*, project-library(*project*))))))
        format(response.output-stream, "<li>%s</li>\n", markup-dylan-source(environment-object-display-name(*project*, subclass, #f)));
    end for;
    format(response.output-stream, "</ul>\n");
end;

define tag project-used-libraries in code-browser
  (page :: <code-browser-page>, response :: <response>)
  ()
 
  format(response.output-stream, "<ul>\n");
  for (library in project-used-libraries(*project*, *project*))
    let name = environment-object-display-name(*project*, library, #f);
    format(response.output-stream, 
           "<li><a href=\"/project?name=%s\">%s</a></li>\n",
           name, name);
  end for;
  format(response.output-stream, "</ul>\n");
end;

define tag project-library in code-browser
  (page :: <code-browser-page>, response :: <response>)
  ()
    format(response.output-stream, "%s", environment-object-display-name(*project*, project-library(*project*), #f));
end;


define tag project-modules in code-browser
  (page :: <code-browser-page>, response :: <response>)
  ()
    format(response.output-stream, "<ul>\n");
    for (module in library-modules(*project*, project-library(*project*)))
      format(response.output-stream, "<li>%s</li>\n", environment-object-display-name(*project*, module, #f));
    end for;
    format(response.output-stream, "</ul>\n");
end;

define tag find-section-for-definition in code-browser
  (page :: <code-browser-page>, response :: <response>)
  ()
    format(response.output-stream, "%s", markup-dylan-source(source-location-string(environment-object-source-location(*project*, find-environment-object(*project*, "concatenate",
      library: project-library(*project*), module: first(library-modules(*project*, project-library(*project*))))))));
end;

define tag generic-function-object-methods in code-browser
  (page :: <code-browser-page>, response :: <response>)
  ()
    format(response.output-stream, "<ul>\n");
    for (m in generic-function-object-methods(*project*,
     find-environment-object(*project*, "concatenate",
      library: project-library(*project*), module: first(library-modules(*project*, project-library(*project*)))))) 
        format(response.output-stream, "<li>%s</li>\n", markup-dylan-source(environment-object-display-name(*project*, m, #f)));
    end;
    format(response.output-stream, "</ul>\n");
end;

define tag find-section-for-method in code-browser
  (page :: <code-browser-page>, response :: <response>)
  ()
    format(response.output-stream, "%s", markup-dylan-source(source-location-string(
      environment-object-source-location(*project*, first(generic-function-object-methods(*project*,
        find-environment-object(*project*, "concatenate",
          library: project-library(*project*), module: first(library-modules(*project*, project-library(*project*))))))))));
end;

//environment-object-source-location  source-location-string
//source-location-source-record

/// Main

define function callback-handler (#rest args)
  log-debug("%=\n", args);
end function callback-handler;

// Starts up the web server.
define function main () => ()
  *check-source-record-date?* := #f;
  let config-file =
    if(application-arguments().size > 0)
      application-arguments()[0]
    end;
  start-server(config-file: config-file);
end;

begin
  main()
end;

/*
begin
  let class-graph = generate-class-graph("<string>");
  let filename = generate-graph(class-graph, find-node(class-graph, "<object>"));
  format-out("filename %s\n", filename);
end;
*/

define function generate-class-graph (class-name :: <string>) => (res :: <graph>)
  let project = find-project("code-browser");
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
          split(environment-object-display-name(project, class, #f), ':')[0];
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
    let superclasses
      = class-direct-superclasses(project, class);
    format-out("superclasses for %s %=\n",
               class-name, map(get-class-name, superclasses));
    add-successors(class-node, map(get-class-name, superclasses));
    do(curry(push-last, todo),
       choose(method(x) ~ member?(x, visited) & ~ member?(x, todo) end,
              superclasses))
  end;
  graph;
end;

