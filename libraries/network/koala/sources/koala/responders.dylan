Module:    httpi
Synopsis:  Built-in URI response functions
Author:    Carl Gay
Copyright: Copyright (c) 2001 Carl L. Gay.  All rights reserved.
           Original Code is Copyright (c) 2001 Functional Objects, Inc.  All rights reserved.
License:   Functional Objects Library Public License Version 1.0
Warranty:  Distributed WITHOUT WARRANTY OF ANY KIND


define class <responder> (<object>)
  // responder-map is a map from request method (e.g. #"POST") to another table
  // mapping regex -> list of functions.  When the regex matches the tail of the
  // url (i.e., the part following the base url on which this responder was
  // registered) the functions are called in order.  They should raise an exception
  // (of what type?) to abort the chain.
  constant slot responder-map :: <table> = make(<table>),
    init-keyword: map:;
end;

// API
define open generic add-responder
    (url :: <object>, responder :: <object>,
     #key replace?, request-methods, server);

// Convenience method to convert first arg to <url>.  All other methods
// should specialize the first arg on (a subclass of) <url>.
define method add-responder
    (url :: <string>, responder :: <object>,
     #key replace?,
          request-methods = #(#"GET", #"POST"),
          server :: false-or(<http-server>))
  add-responder(parse-url(url), responder,
                replace?: replace?,
                request-methods: request-methods,
                server: server)
end method add-responder;

define method add-responder
    (url :: <url>, responder :: <responder>,
     #key replace?,
          request-methods = #(#"GET", #"POST"),
          server :: false-or(<http-server>) = *server*)
  local method register-responder ()
          if (empty?(url.uri-path))
            error(make(<koala-api-error>,
                       format-string: "You can't add a responder with an empty URL: %s",
                       format-arguments: list(url)));
          else
            add-object(*server*.url-map, url.uri-path, responder, replace?: replace?);
            log-info("responder on %s registered", url);
          end if;
        end;
  if (server)
    dynamic-bind(*server* = server)
      register-responder();
    end;
  else
    register-init-function(register-responder);
  end;
end method add-responder;

// The simple case where you just want an exact URL to map to a function.
// This takes care of the messy details of building a <responder> object.
//
define method add-responder
    (url :: <url>, response-function :: <function>,
     #key replace?,
          request-methods = #(#"GET", #"POST"),
          server :: false-or(<http-server>))
  let table = make(<table>, size: 1);
  table[compile-regex("^$")] := list(response-function);
  add-responder(url, table,
                replace?: replace?,
                request-methods: request-methods,
                server: server);
end method add-responder;

// Use this if you want a prefix URL and different behaviour depending on
// which regex matches the URL tail.
//
define method add-responder
    (url :: <url>, regex-map :: <table>,
     #key replace?,
          request-methods = #(#"GET", #"POST"),
          server :: false-or(<http-server>))
  for (responses keyed-by regex in regex-map)
    assert(instance?(regex, <regex>)
             & instance?(responses, <sequence>)
             & every?(rcurry(instance?, <function>), responses),
           "The regex-map argument to add-responder must be a table "
           "mapping <regex> to a sequence of functions.  Found %= -> %=.",
           regex, responses);
  end;
  let responder = make(<responder>);
  for (request-method in request-methods)
    // todo -- validate-request-method(request-method)
    responder.responder-map[request-method] := regex-map;
  end;
  add-responder(url, responder,
                replace?: replace?,
                request-methods: request-methods,
                server: server);
end method add-responder;

define open generic find-responder
    (url :: <object>)
 => (responder :: false-or(<responder>),
     rest-path :: false-or(<sequence>));

define method find-responder
    (url :: <string>)
 => (responder :: false-or(<responder>),
     rest-path :: false-or(<sequence>))
  find-responder(parse-url(url))
end method find-responder;

define method find-responder
    (url :: <url>)
 => (responder :: false-or(<responder>),
     rest-path :: false-or(<sequence>))
  find-object(*server*.url-map, url.uri-path);
end method find-responder;


define open generic remove-responder (object :: <object>);

define method remove-responder (url :: <string>)
  remove-responder(parse-url(url));
end;

define method remove-responder (url :: <url>)
  remove-object(*server*.url-map, url.uri-path);
end;


/* Example usage
define url-map
  url "/wiki",
    action get () => show-page,
    action get () => show-page;
  url "/wiki/login"
    action post ("/(?<name>:\\w+") => login;
  url
end;
*/
// It might be nice to add a prefix clause to this.  e.g.,
//    prefix: "/demo"
// so that all urls are prefixed with that string.  But that might
// be better handled by "define web-application", which perhaps this
// macro can be expanded to at some point.
//
// This should have a way of specifying the server for which the urls
// will be defined.  Maybe "define url-map for server ... end".
// Predictably, trying to add that syntax results in a mysterious and
// unhelpful error message.
//
define macro url-map-definer
  { define url-map
      ?urls
    end }
   => { ?urls }

  urls:
    { } => { }
    { ?url ; ... } => { ?url ; ... }

  url:
    { url ?location:expression ?definitions }
     => { let _responder = make(<responder>);
          ?definitions ;
          ?location }
    { url ( ?locations ) ?definitions }
      => { let _responder = make(<responder>);
           ?definitions ;
           ?locations }

  locations:
    { } => { }
    { ?location , ...  } => { ?location ; ... }

  location: 
    { ?uri:expression } => { add-responder( ?uri , _responder) }

  definitions:
    { } => { }
    { ?definition , ... } => { ?definition ; ... }

  // I'd like to get rid of the parens around ?request-methods.
  // Not quite sure how yet though.  --cgay
  definition:
    { action ( ?request-methods ) ( ?regex ) => ?action:expression }
      => { let regex = compile-regex(?regex, use-cache: #t);
           let actions = list(?action);
           ?request-methods }
    { action ?request-method:name ( ?regex ) => ?action:expression }
      => { let regex = compile-regex(?regex);
           let actions = list(?action);
           ?request-method }
    { action ( ?request-methods ) ( ?regex ) => ( ?action-sequence:* ) }
      => { let regex = compile-regex(?regex, use-cache: #t);
           let actions = list(?action-sequence);
           ?request-methods }
    { action ?request-method:name ( ?regex ) => ( ?action-sequence:* ) }
      => { let regex = compile-regex(?regex);
           let actions = list(?action-sequence);
           ?request-method }
  request-methods:
    { } => { }
    { ?request-method , ...  } => { ?request-method ; ... }

  request-method:
    { ?:name }
     => { add-responder-map-entry(_responder, ?#"name", regex, actions) }

  regex:
    { } => { "^$" }
    { * } => { ".*" }
    { ?pattern:expression } => { ?pattern }

end macro url-map-definer;

define inline function add-responder-map-entry
    (responder :: <responder>,
     request-method :: <symbol>,
     regex :: <regex>,
     actions :: <sequence>)
  let table = element(responder.responder-map, request-method, default: #f);
  if (~table)
    table := make(<table>);
    responder.responder-map[request-method] := table;
  end;
  // The following depends on regex caching working, so they're ==.
  // todo -- Add the url to the error message.  It's not accessible
  //         here at the moment.
  if (element(table, regex, default: #f))
    signal(make(<koala-api-error>,
                format-string: "Duplicate regular expression (%s) "
                               "in url map for %s",
                format-arguments: list(regex, request-method)));
  end;
  table[regex] := actions
end function add-responder-map-entry;


// define responder test ("/test" /* , secure?: #t */ )
//   format(output-stream(response), "<html><body>test</body></html>");
// end;
// This is just minimally working after the switch to "define url-map" 
define macro responder-definer
  { define responder ?:name (?url:expression)
      ?:body
    end
  }
  => { define function ?name (#key) ?body end;
       add-responder(?url, ?name)
     }
end macro responder-definer;

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
