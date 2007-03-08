Module:   code-browser
Synopsis: Brwose Open Dylan environment objects
Author:   Andreas Bogk, Bastian Mueller, Hannes Mehnert

define thread variable *project* = #f; 
define thread variable *environment-object* = #f;

define function callback-handler (#rest args)
  log-debug("%=\n", args);
end function callback-handler;
define taglib code-browser () end;

define class <code-browser-page> (<dylan-server-page>)
end;

define generic environment-object-page (obj :: <environment-object>) => (res :: <code-browser-page>);
define directory responder symbol-responder ("/symbol")
 (request, response)
  let suffix = split(request.request-url-tail, '/');
  //format-out("hit /: %= %d\n", suffix, suffix.size);
  if (suffix.size = 3)
    let library-name = suffix[0];
    let module-name = suffix[1];
    let symbol-name = suffix[2];
    let project = find-project(library-name);
    open-project-compiler-database(project, 
                                   warning-callback: callback-handler,
                                   error-handler: callback-handler);
    parse-project-source(project);
    let library = project.project-library;
    let module = find-module(project, module-name, library: library);
    let symbol = find-environment-object(project,
                                         symbol-name,
                                         library: library,
                                         module: module);
    dynamic-bind(*project* = project)
      dynamic-bind(*environment-object* = symbol)
        process-template(environment-object-page(*environment-object*), request, response);
      end;
    end;
  end;
end;

define page raw-source-page (<code-browser-page>)
  (source: "raw-source.dsp")
end;

define method environment-object-page (object :: <environment-object>)
 => (res :: <code-browser-page>)
  *raw-source-page*;
end;
define macro code-browser-pages-definer
 { define code-browser-pages ?pages:* end }
 => { ?pages }

  pages:
   { } => { }
   { ?page:name, ... }
   => { define page ?page ## "-page" (<code-browser-page>)
          (source: ?"page" ## ".dsp")
        end;
        define method environment-object-page
         (object :: "<" ## ?page ## "-object>") => (res :: "<" ## ?page ## "-page>")
           "*" ## ?page ## "-page*";
        end;
        ... }
end;

define code-browser-pages
  constant, domain, generic-function,
  \method, simple-function, \macro, module-variable,
  library, module, class //singleton missing? but it is not exported!
end;

define tag source in code-browser
 (page :: <code-browser-page>, response :: <response>)
 ()
  format(output-stream(response), "%s",
         markup-dylan-source(environment-object-source(*project*, *environment-object*)));
end;

define tag project-name in code-browser
  (page :: <code-browser-page>, response :: <response>)
  ()
  write(output-stream(response), *project*.project-name);
end;


define function markup-dylan-source(source :: <string>)
 => (processed-source :: <string>);
  regexp-replace(regexp-replace(regexp-replace(source, "&", "&amp;"), "<", "&lt;"), ">", "&gt;");
end function markup-dylan-source;

//XXX: refactor this into the specific tags - each tag which may be a reference
// knows for itself best where to link!
define tag canonical-link in code-browser
 (page :: <code-browser-page>, response :: <response>)
 ()
  format(output-stream(response), "%s", do-canonical-link(*environment-object*));
end;
define method do-canonical-link (symbol)
  let name-object = environment-object-home-name(*project*, symbol);
  if (name-object)
    let module-object = name-namespace(*project*, name-object);
    let module-name-object = environment-object-home-name(*project*, module-object);
    let library-object = name-namespace(*project*, module-name-object);
    concatenate("/symbol/", dylan-name(library-object),
                "/", dylan-name(module-object),
                "/", dylan-name(symbol));
  end;
end;

define method do-canonical-link (slot :: <slot-object>)
  do-canonical-link(slot-type(*project*, slot))
end;

define body tag slots in code-browser
 (page :: <code-browser-page>, response :: <response>, do-body :: <function>)
 ()
  do-all-slots(method(x) dynamic-bind(*environment-object* = x) do-body() end end, *project*, *environment-object*);
end;

define function dylan-name
    (definition :: <environment-object>)
 => (name :: <string>)
  let project = *project*;
  let name = environment-object-home-name(*project*, definition);
  if (name)
    environment-object-primitive-name(*project*, name)
  else
    environment-object-display-name(*project*, definition, #f, qualify-names?: #f)
  end
end;

define function html-name (symbol) // :: <definition-object>)
  (symbol & markup-dylan-source(dylan-name(symbol))) | "unknown symbol"
end;

define tag display-name in code-browser
  (page :: <code-browser-page>, response :: <response>)
  ()
    format(response.output-stream, "%s", html-name(*environment-object*));
end;
define body tag direct-superclasses in code-browser
  (page :: <code-browser-page>, response :: <response>, do-body :: <function>)
  ()
  for (superclass in class-direct-superclasses(*project*, *environment-object*))
    dynamic-bind(*environment-object* = superclass)
      do-body()
    end;
  end for;
end;


define body tag direct-subclasses in code-browser
 (page :: <code-browser-page>, response :: <response>, do-body :: <function>)
 ()
  for (subclass in class-direct-subclasses(*project*, *environment-object*))
    dynamic-bind(*environment-object* = subclass)
      do-body()
    end;
  end for;
end;

define tag slot-name in code-browser
 (page :: <code-browser-page>, response :: <response>)
 ()
   format(output-stream(response), "%s",
          html-name(slot-getter(*project*, *environment-object*)));
end;

define tag slot-type in code-browser
 (page :: <code-browser-page>, response :: <response>)
 ()
  format(output-stream(response), "%s",
         html-name(slot-type(*project*, *environment-object*)));
end;
define body tag used-definitions in code-browser
 (page :: <code-browser-page>, response :: <response>, do-body :: <function>)
 ()
  for (used-definition in source-form-used-definitions(*project*, *environment-object*))
    dynamic-bind (*environment-object* = used-definition)
      do-body()
    end;
  end;
end;


//These tags are not used currently!
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


define tag clients in code-browser
  (page :: <code-browser-page>, response :: <response>)
  ()
    format(response.output-stream, "<ul>\n");
    for (used-definition in source-form-clients(*project*, project-library(*project*)))
      format(response.output-stream, "<li>%s</li>", markup-dylan-source(environment-object-display-name(*project*, used-definition, #f)))
    end for;
    format(response.output-stream, "</ul>\n");
end;

define tag project-warnings in code-browser
  (page :: <code-browser-page>, response :: <response>)
  ()
    format(response.output-stream, "<ul>\n");
    for (warning in project-warnings(*project*))
      format(response.output-stream, "<li>%s</li>", markup-dylan-source(environment-object-display-name(*project*, warning, #f)))
    end for;
    format(response.output-stream, "</ul>\n");
end;

/// Main

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

