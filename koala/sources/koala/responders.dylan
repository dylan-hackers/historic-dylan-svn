Module:    httpi
Synopsis:  Built-in URI response functions
Author:    Carl Gay
Copyright: Copyright (c) 2001 Carl L. Gay.  All rights reserved.
           Original Code is Copyright (c) 2001 Functional Objects, Inc.  All rights reserved.
License:   Functional Objects Library Public License Version 1.0
Warranty:  Distributed WITHOUT WARRANTY OF ANY KIND


// General server statistics
//
define responder general-stats-responder ("/koala/stats")
    (request, response)
  let stream = output-stream(response);
  let server = request.request-server;
  format(stream, "<html><body>");
  format(stream, "%s<br>", $server-header-value);
  format(stream, "Up since %s<br>", as-iso8601-string(server.startup-date));
  format(stream, "Connections handled: %d<br>", server.connections-accepted);
  format(stream, "</body></html>");
end;


// Show some stats about what user-agents have connected to the server.
//
define responder user-agent-responder ("/koala/user-agents")
    (request, response)
  let stream = output-stream(response);
  format(stream, "<html><body>");
  for (count keyed-by agent in user-agent-stats(request-server(request)))
    format(stream, "%5d: %s<br>", count, agent);
  end;
  format(stream, "</body></html>");
end;

// Return an HTTP error code, for testing purposes.
// e.g., /koala/http-error?code=503
//
define responder http-error-responder ("/koala/http-error")
    (request, response)
  let code-string = get-query-value("code");
  let code = string-to-integer(code-string);
  signal(make(<http-error>,
              code: code,
              format-string: "Koala HTTP server received a request for error code %=",
              format-arguments: vector(code-string)));
end;

/*
// Shutdown the server.  You definately don't want this active in a 
// production setting.
//
define responder shutdown-responder ("/koala/shutdown")
    (request, response)
  let stream = output-stream(response);
  let server = request.request-server;
  format(stream, "<html><body>Shutting down...</body></html>");
  force-output(stream);
  stop-server(abort: #t);
end;
*/

// Load a module
//
define responder load-module-responder ("/koala/load-module")
    (request, response)
  load/unload-module(request, response, #"load");
end;

// Unload a module
//
define responder unload-module-responder ("/koala/unload-module")
    (request, response)
  load/unload-module(request, response, #"unload");
end;

define function load/unload-module
    (request, response, op :: one-of(#"load", #"unload"))
  let stream = output-stream(response);
  let server = request.request-server;
  let module-name = get-query-value("name");
  write(stream, "<html><body>\n");
  if (~module-name)
    write(stream, "You must specify the name of a module in the URL.\n");
  else
    if (op == #"load")
      load-module(module-name);
      format(stream, "Module %s loaded.");
    else
      unload-module(module-name);
      format(stream, "Module %s unloaded.");
    end;
  end;
  write(stream, "</body></html>");
end;

