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

  // This doesn't make much sense to me.  Since the regular expressions are in a
  // table there's no guaranteed order in which they'll be searched so you could
  // get an arbitrary result.  --cgay Sep 2008

  constant slot responder-map :: <table>,
    init-function: curry(make, <table>),
    init-keyword: map:;
end;

// API
define open generic add-responder
    (server :: <http-server>, url :: <object>, responder :: <object>,
     #key replace?, request-methods);

// Convenience method to convert first arg to <url>.  All other methods
// should specialize the first arg on (a subclass of) <url>.
define method add-responder
    (server :: <http-server>, url :: <string>, responder :: <object>,
     #key replace?,
          request-methods = #(#"GET", #"POST"))
  add-responder(server, parse-url(url), responder,
                replace?: replace?,
                request-methods: request-methods)
end method add-responder;

define method add-responder
    (server :: <http-server>, url :: <uri>, responder :: <responder>,
     #key replace?,
          request-methods = #(#"GET", #"POST"))
  if (empty?(url.uri-path))
    error(make(<koala-api-error>,
               format-string: "You can't add a responder for a URL with no path: %s",
               format-arguments: list(url)));
  else
    add-object(server.url-map, url.uri-path, responder, replace?: replace?);
    log-info("Responder: %s ", build-path(url));
  end if;
end method add-responder;

// The simple case where you just want an exact URL to map to a function.
// This takes care of the messy details of building a <responder> object.
//
define method add-responder
    (server :: <http-server>, url :: <uri>, response-function :: <function>,
     #key replace?,
          request-methods = #(#"GET", #"POST"))
  let table = make(<table>, size: 1);
  table[compile-regex("^$")] := list(response-function);
  add-responder(server, url, table,
                replace?: replace?,
                request-methods: request-methods);
end method add-responder;

// Use this if you want a prefix URL and different behaviour depending on
// which regex matches the URL tail.
//
define method add-responder
    (server :: <http-server>, url :: <uri>, regex-map :: <table>,
     #key replace?,
          request-methods = #(#"GET", #"POST"))
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
  add-responder(server, url, responder,
                replace?: replace?,
                request-methods: request-methods);
end method add-responder;

define open generic find-responder
    (server :: <http-server>, url :: <object>)
 => (responder :: false-or(<responder>),
     rest-path :: false-or(<sequence>));

define method find-responder
    (server :: <http-server>, url :: <string>)
 => (responder :: false-or(<responder>),
     rest-path :: false-or(<sequence>))
  find-responder(server, parse-url(url))
end method find-responder;

define method find-responder
    (server :: <http-server>, url :: <url>)
 => (responder :: false-or(<responder>),
     rest-path :: false-or(<sequence>))
  find-object(server.url-map, url.uri-path)
end method find-responder;


define open generic remove-responder
    (server :: <http-server>, object :: <object>);

define method remove-responder
    (server :: <http-server>, url :: <string>)
  remove-responder(server, parse-url(url));
end;

define method remove-responder
    (server :: <http-server>, url :: <url>)
  remove-object(server.url-map, url.uri-path);
end;


/* Example usage
define url-map on my-http-server
  url "/wiki",
    action GET () => show-page,
    action POST () => edit-page;
  url "/wiki/login"
    action POST ("/(?<name>:\\w+") => login;
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
  { define url-map on ?http-server:expression
      ?urls
    end }
   => { let _http-server :: <http-server> = ?http-server;
        ?urls }

  urls:
    { } => { }
    { ?url ; ... } => { ?url ; ... }

  url:
    { url ?location:expression ?actions }
     => { let _responder = make(<responder>);
          ?actions ;
          ?location }
    { url ( ?locations ) ?actions }
      => { let _responder = make(<responder>);
           ?actions ;
           ?locations }

  locations:
    { } => { }
    { ?location , ...  } => { ?location ; ... }

  location: 
    { ?uri:expression } => { add-responder( _http-server, ?uri , _responder) }

  actions:
    { } => { }
    { ?action-definition , ... } => { ?action-definition ; ... }

  // I'd like to get rid of the parens around ?request-methods.
  // Not quite sure how yet though.  --cgay
  action-definition:
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
