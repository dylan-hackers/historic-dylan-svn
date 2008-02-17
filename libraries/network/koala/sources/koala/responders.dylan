Module:    httpi
Synopsis:  Built-in URI response functions
Author:    Carl Gay
Copyright: Copyright (c) 2001 Carl L. Gay.  All rights reserved.
           Original Code is Copyright (c) 2001 Functional Objects, Inc.  All rights reserved.
License:   Functional Objects Library Public License Version 1.0
Warranty:  Distributed WITHOUT WARRANTY OF ANY KIND


define class <responder> (<object>)
  slot responder-map :: <table> = make(<table>),
    init-keyword: map:;
end;

define open generic add-responder
    (url :: <object>, responder :: <responder>, #key replace?)
 => ();

define method add-responder
    (url :: <string>, responder :: <responder>, #key replace?)
 => ();
  add-responder(parse-url(url), responder, replace?: replace?);
end;

define method add-responder
    (url :: <url>, responder :: <responder>, #key replace?)
 => ();
  local method responder-registration ()
          if (empty?(url.uri-path))
            error(make(<koala-api-error>,
                       format-string: "You can't add a responder with an empty URL: %s",
                       format-arguments: list(url)));
          else
            add-object(*server*.url-map, url.uri-path, responder, replace?: replace?);
            log-info("responder on %s registered", url);
          end if;
        end;
  if (*server-running?*)
    responder-registration();
  else
    register-init-function(responder-registration);
  end;
end method add-responder;

define open generic find-responder
    (url :: <object>)
 => (responder :: false-or(<responder>),
	            rest-path :: false-or(<sequence>));

define method find-responder
    (url :: <string>)
 => (responder :: false-or(<responder>),
	            rest-path :: false-or(<sequence>));
  find-responder(parse-url(url));
end method find-responder;

define method find-responder
    (url :: <url>)
 => (responder :: false-or(<responder>),
	            rest-path :: false-or(<sequence>));
  find-object(*server*.url-map, url.uri-path);
end method find-responder;


define open generic remove-responder (object :: <object>);

define method remove-responder (url :: <string>)
  remove-responder(parse-url(url));
end;

define method remove-responder (url :: <url>)
  remove-object(*server*.url-map, url.uri-path);
end;


define macro url-map-definer
  { define url-map
      ?urls
    end }
   => { ?urls }

  urls:
    { } => { }
    { ?url ; ... } => { ?url ; ... }

  url:
    { url ?location:expression , ?definitions }
     => { begin
            let responder = make(<responder>);
            ?definitions ;
            ?location ;
          end }
    { url ( ?locations ) , ?definitions }
      => { begin
            let responder = make(<responder>);
            ?definitions ;
            ?locations ;
           end }

  locations:
    { } => { }
    { ?location , ...  } => { ?location ; ... }

  location: 
    { ?uri:expression } => { add-responder( ?uri , responder) }

  definitions:
    { } => { }
    { ?definition , ... } => { ?definition ; ... }

  definition:
    { action ( ?request-methods ) ( ?regex ) => ?action:name }
      => { begin
             let regex = compile-regex(?regex);
             let actions = list(?action);
             ?request-methods
           end }
    { action ?request-method:name ( ?regex ) => ?action:name }
      => { begin
             let regex = compile-regex(?regex);
             let actions = list(?action);
             ?request-method
           end }
    { action ( ?request-methods ) ( ?regex ) => ( ?action-sequence:* ) }
      => { begin
             let regex = compile-regex(?regex);
             let actions = list(?action-sequence);
             ?request-methods
           end }
    { action ?request-method:name ( ?regex ) => ( ?action-sequence:* ) }
      => { begin
             let regex = compile-regex(?regex);
             let actions = list(?action-sequence);
             ?request-method
           end }

  request-methods:
    { } => { }
    { ?request-method , ...  } => { ?request-method ; ... }

  request-method:
    { ?:name }
     => { begin
            let map = element(responder.responder-map,
                              ?#"name",
		              default: #f);
            unless (map)
              map := make(<table>);
              responder.responder-map[?#"name"] := map;
            end unless;
            map[regex] := actions
          end }

  regex:
    { } => { "^$" }
    { * } => { ".*" }
    { ?pattern:expression } => { ?pattern }

end macro url-map-definer;

// define responder test ("/test" /* , secure?: #t */ )
//   format(output-stream(response), "<html><body>test</body></html>");
// end;
define macro responder-definer
  { define responder ?:name (?url:expression)
      ?:body
    end
  }
  => { define method ?name () ?body end;
         register-url(?url, ?name)
     }

  { define directory responder ?:name (?url:expression)
      ?:body
    end
  }
  => { define method ?name () ?body end;
         register-url(?url, ?name, prefix?: #t)
     }
end;

/*
define (get, post) responder foo-responder ("/foo", "/bar")
  ("^(?P<name>\\w+)/?$")
  (#key name)
  ...
end;
*/

/*
// General server statistics
//
define responder general-stats-responder ("/koala/stats") 
  let stream = current-response().output-stream;
  let server = current-request().request-server;
  format(stream, "<html><body>");
  format(stream, "%s<br>", $server-header-value);
  format(stream, "Up since %s<br>", as-iso8601-string(server.startup-date));
  format(stream, "Connections handled: %d<br>", server.connections-accepted);
  format(stream, "</body></html>");
end;


// Show some stats about what user-agents have connected to the server.
//
define responder user-agent-responder ("/koala/user-agents")
  let stream = current-response().output-stream;
  format(stream, "<html><body>");
  for (count keyed-by agent in current-request().request-server.user-agent-stats)
    format(stream, "%5d: %s<br>", count, agent);
  end;
  format(stream, "</body></html>");
end;

// Return an HTTP error code, for testing purposes.
// e.g., /koala/http-error?code=503
//
define responder http-error-responder ("/koala/http-error")
  let code-string = get-query-value("code");
  let code = string-to-integer(code-string);
  signal(make(<http-error>,
              code: code,
              format-string: "Koala HTTP server received a request for error code %=",
              format-arguments: vector(code-string)));
end;
*/
