Module:   code-browser
Synopsis: Brwose FD environment objects
Author:   Andreas Bogk

// Responds to a single URL.
define responder responder1 ("/shutdown")
    (request :: <request>,
     response :: <response>)
  select (request-method(request))
    #"get", #"post"
      => begin 
           format(output-stream(response),
                  "<html><body>Shutting down the server"
                    "</body></html>");
           force-output(output-stream(response));
           stop-server(abort: #t);
         end;
    
  end;
end;

define class <code-browser-page> (<dylan-server-page>)
  slot project;
end;

define taglib code-browser ()
end;

define page project-page (<code-browser-page>)
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
  page.project := find-project(project-name);
  if(page.project)
    open-project-compiler-database(page.project, 
                                   warning-callback: callback-handler,
                                   error-handler: callback-handler);
    parse-project-source(page.project);
    next-method();
  else
    application-error(format-string: "No such project %s", 
                      format-arguments: vector(project-name));
  end if;
end method respond-to-get;


define tag project-name in code-browser
  (page :: <code-browser-page>, response :: <response>)
  ()
  write(output-stream(response), page.project.project-name);
end;

define tag project in code-browser
  (page :: <code-browser-page>, response :: <response>)
  ()
  format(output-stream(response), "%=", page.project);
end;

define tag project-sources in code-browser
  (page :: <code-browser-page>, response :: <response>)
  ()
  dynamic-bind(*check-source-record-date?* = #f)
    format(response.output-stream, "<pre>\n");
    for(source in page.project.project-sources)
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
  regexp-replace(regexp-replace(source, "&", "&amp;"), "<", "&lt;");
end function markup-dylan-source;



define tag project-used-libraries in code-browser
  (page :: <code-browser-page>, response :: <response>)
  ()
  format(response.output-stream, "<ul>\n");
  for(library in project-used-libraries(page.project, page.project))
    let name = environment-object-display-name(page.project, library, #f);
    format(response.output-stream, 
           "<li><a href=\"/project?name=%s\">%s</a></li>\n",
           name, name);
  end for;
  format(response.output-stream, "</ul>\n");
end; 



/// Main

define function callback-handler (#rest args)
  log-debug("%=\n", args);
end function callback-handler;

// Starts up the web server.
define function main () => ()
  *check-source-record-date?* := #f;
  start-server();
end;

begin
  main();
end;

