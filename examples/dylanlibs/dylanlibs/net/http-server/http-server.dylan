Module:    http-server
Synopsis:  Simple HTTP Server
Author:    Chris Double
Copyright: (C) 2001, Chris Double.  All rights reserved.

// A <request> holds the details contained within an http request
// so that handlers can act upon them.
define class <request> (<object>)
  //  #"get", #"post", etc.
  constant slot request-type :: <symbol>, required-init-keyword: request-type:;
  constant slot request-path :: <string>, required-init-keyword: path:;
  constant slot request-headers :: <string-table>, required-init-keyword: headers:;
  constant slot request-query :: false-or(<form-query>) = #f, init-keyword: query:;  
  constant slot request-host :: false-or(<string>) = #f, init-keyword: host:;

  // Contains the contents of the body from POST requests.
  constant slot request-body :: false-or(<string>) = #f, init-keyword: body:;
end class <request>;

// A <handler> handles an incoming request in some manner.
define open class <handler> (<object>)
  // The path that the request must match to cause this
  // handler to activate.
  slot handler-path :: <string>, required-init-keyword: path:;  

  // The host that the request must match to cause this
  // handler to activate.
  slot handler-host :: false-or(<string>) = #f, init-keyword: host:;

  // The request type that must match to cause this handler
  // to activate.
  slot handler-request-type :: false-or(<symbol>) = #f, init-keyword: handler-type:;

  // Whether the given path must match exactly for this handler
  // to activate.  
  slot handler-exact-match? :: <boolean> = #t, init-keyword: exact-match?:;
end class <handler>;

define method \=(lhs :: <handler>, rhs :: <handler>) 
 => (r :: <boolean>)
  lhs.object-class = rhs.object-class &
    lhs.handler-path = rhs.handler-path &
    lhs.handler-host = rhs.handler-host &
    lhs.handler-request-type == rhs.handler-request-type &
    lhs.handler-exact-match? = rhs.handler-exact-match?;  
end method \=;

define open generic process-handler(handler :: <handler>,
                                    server :: <simple-http-server>,
                                    request :: <request>,
                                    stream :: <stream>)
 => ();

define method process-handler(handler :: <handler>, 
                              server :: <simple-http-server>,
                              request :: <request>,
                              stream :: <stream>)
 => ()
end method process-handler;

define method can-handle-request?(handler :: <handler>,
                                  request :: <request>)
 => (r :: <boolean>)
  local method path-matches?() => (r :: <boolean>)
    (handler.handler-exact-match? 
       & handler.handler-path = request.request-path) 
              |
    (~handler.handler-exact-match? &
        request.request-path.size > handler.handler-path.size &
        handler.handler-path = copy-sequence(request.request-path, end: handler.handler-path.size));
  end;
 
  local method request-type-matches?() => (r :: <boolean>)
    (handler.handler-request-type &
      handler.handler-request-type == request.request-type)
                   |
    ~handler.handler-request-type;
  end;

  local method host-matches?() => (r :: <boolean>)
    ~request.request-host 
              |
    (handler.handler-host &
      handler.handler-host = request.request-host)
                   |
    ~handler.handler-host;                   
  end;

  request-type-matches?() & host-matches?() & path-matches?();
end method can-handle-request?;
                                  
define open class <dynamic-handler> (<handler>)
  // Function has the signature:
  //   function(server :: <simple-http-server>, request :: <request>, stream :: <stream>) => ();
  //
  slot handler-function :: <function>, required-init-keyword: function:;
end class;

define method process-handler(handler :: <dynamic-handler>, 
                              server :: <simple-http-server>,
                              request :: <request>,
                              stream :: <stream>)
 => ()
  handler.handler-function(server, request, stream);
end method process-handler;

define method publish-dynamic-handler(server :: <simple-http-server>, 
                                      path :: <string>,
                                      function :: <function>,
                                      #key 
                                      host, request-type, exact-match? = #t)
 => (h :: <handler>)
  let handler = make(<dynamic-handler>,
                     path: path,
                     function: function,
                     host: host,
                     request-type: request-type,
                     exact-match?: exact-match?);
  publish-handler(server, handler);
  handler;
end method publish-dynamic-handler;

define class <file-handler> (<handler>)
  slot handler-file-name :: <string>, required-init-keyword: file-name:;
  slot handler-cache? :: <boolean> = #f, init-keyword: cache?:;
end class <file-handler>;

define class <simple-http-server> (<object>) 
  // Server socket for this HTTP server.
  slot http-server-socket :: false-or(<tcp-server-socket>) = #f;

  // Shut down server on next request if #t
  slot http-server-perform-shutdown? :: <boolean> = #f;

  // A sequence of <handler> objects that handle
  // requests for this server.
  slot http-server-handlers :: <stretchy-vector> = make(<stretchy-vector>);
end class <simple-http-server>;

define method process-client-connection(server :: <simple-http-server>, remote-socket :: <stream>)
 => ()
  block()
    handle-request(server, remote-socket);
  cleanup
    force-output(remote-socket);
    close(remote-socket);
  end;
end method process-client-connection;

define method publish-handler(server :: <simple-http-server>, handler :: <handler>)
  // First look to see if the given handler already exists in the server.
  // If so, replace it. Otherwise add it.
  let index =
    block(return)
      for(h keyed-by index in server.http-server-handlers)
        when(h = handler)
          return(index);
        end when;
      end for;
    end block;

  if(index)
    server.http-server-handlers := handler;
  else
    server.http-server-handlers := add!(server.http-server-handlers, handler);
  end if;
end method;

define method unpublish-handler(server :: <simple-http-server>, handler :: <handler>)
 => ()
  server.http-server-handlers := remove!(server.http-server-handlers, handler, test: \=);
end method unpublish-handler;

define method unpublish-all-handlers(server :: <simple-http-server>)
 => ()
  server.http-server-handlers := make(<stretchy-vector>);
end method unpublish-all-handlers;

define class <threaded-http-server> (<simple-http-server>)
end class <threaded-http-server>;

define method process-client-connection(server :: <threaded-http-server>, remote-socket :: <stream>)
 => ()
  make(<thread>, function: 
	method()
	  block()
	    handle-request(server, remote-socket);
          cleanup
            force-output(remote-socket);
            close(remote-socket);
//          exception(e :: <error>)
//            format-out("Condition: %=\n", e);
	  end;
	end);
end method process-client-connection;

define method parse-path-and-query(path-and-query :: <string>)
 => (path :: <string>, query :: false-or(<form-query>))
  let split = split-string(path-and-query, "?");
  values(uri-decode(split.first), 
         when(split.size > 1) 
           form-query-decode(split.second) 
         end);  
end method parse-path-and-query;

define method decode-request(request-line :: <string>)
 => (request :: <symbol>, path :: <string>, query :: false-or(<form-query>), protocol :: <symbol>)
  let split = split-string(request-line, " ");
  let (path, query) = parse-path-and-query(split.second);
  values(as(<symbol>, split.first),
         path,
         query,
         as(<symbol>, split.third));
end method decode-request;

define method extract-headers-and-body(stream :: <stream>)
 => (r :: false-or(<string-table>), body :: false-or(<string>))
  let result = make(<string-table>);
  let body = #f;
  for(line = read-line(stream, on-end-of-stream: #f)
      then read-line(stream, on-end-of-stream: #f),
      until: ~line | empty?(line) )
	let p = subsequence-position(line, ":");
	let key = copy-sequence(line, end: p);
	let value = copy-sequence(line, start: p + 2);
	result[key] := value;
  finally   
    when(empty?(line))
      let content-length = element(result, "Content-Length", default: #f);
      when(content-length)
        // read(...) and read-into!(...) are returning strange errors when
        // used on sockets. As a result, I'm using read-element content-length times.
        // Email sent to Functional Objects requesting advice on the issue.
        let content-length = string-to-integer(content-length);
        let temp-body = make(<byte-string>, size: content-length);
        body := 
          block(return)
            for(n from 0 below content-length)
              let ch = read-element(stream, on-end-of-stream: #f);
              if(ch)
                temp-body[n] := ch;
              else
                return(temp-body)
              end if;
            end for;
            temp-body;
          end block;
      end when;
    end when;
  end for;
  values(result, body);
end method extract-headers-and-body;

define method find-matching-handler(server :: <simple-http-server>, 
                                    request :: <request>)
 => (r :: false-or(<handler>))
  // Look for exact matches first.
  let found = 
    block(return)
      for(handler in server.http-server-handlers)
        when(handler.handler-exact-match? & can-handle-request?(handler, request))
          return(handler);
        end when;
      end for;
    end block;

  unless(found)
    // Now look for in-exact matches
    for(handler in server.http-server-handlers)
      unless(handler.handler-exact-match?)
        when(can-handle-request?(handler, request))
          if(found)
            when(handler.handler-path.size > found.handler-path.size)
              found := handler;
            end when;
          else
            found := handler;
          end if;
        end when;
      end unless;
    end for;
  end unless;

  found;             
end method find-matching-handler;

define method handler-not-found(server :: <simple-http-server>,
                                request :: <request>,
                                stream :: <stream>)
 => ()
  let dom = 
    with-dom-builder()
      (html
        (head (title ["File not found"])),
        (body 
          (p ["I could not find the file "],
             [request.request-path],
             [" on this server."])))          
    end with-dom-builder;
  print-html(dom, stream);
end method handler-not-found;

define method handle-request(server :: <simple-http-server>, remote-socket :: <stream>)
  let (request-type, path, query, protocol) = decode-request(read-line(remote-socket));
  let (headers :: <string-table>, body) = extract-headers-and-body(remote-socket);

  // Look for 'Host' header.
  let host = element(headers, "host", default: #f);
  let request = make(<request>,
                     request-type: request-type,
                     path: path,
                     headers: headers,
                     query: query,
                     host: host,
                     body: body);
                     
  let handler = find-matching-handler(server, request);

  if(handler)
    process-handler(handler, server, request, remote-socket);
    format-out("Handling: %=\n", request.request-path);
  else
    write(remote-socket, "HTTP/1.0 404 Not Found\r\n");
    write(remote-socket, "Content-type: text/html\r\n");
    write(remote-socket, "\r\n");
    handler-not-found(server, request, remote-socket);
    write(remote-socket, "\r\n\r\n");
    force-output(remote-socket);
    format-out("Not Found...\n");
  end if;
end method handle-request;

define method start-http-server(server :: <simple-http-server>, #key port = 80)
 => ()
  block(quit-server)    
    with-server-socket(server-socket, port: port)
      server.http-server-socket := server-socket;

      start-server(server-socket, remote-socket)
        process-client-connection(server, remote-socket);
        when(server.http-server-perform-shutdown?)
          quit-server();
        end when;
      end;
    end with-server-socket;
  cleanup
    server.http-server-socket := #f;
  end block;
end method start-http-server;

define method start-http-server-on-thread(server :: <simple-http-server>, #key port = 80)
 => ()
  make(<thread>, function: method() start-http-server(server, port: port) end);
end method start-http-server-on-thread;

define method stop-http-server(server :: <simple-http-server>) => ()
  server.http-server-perform-shutdown? := #t;
end method stop-http-server;

define method write-standard-headers(stream :: <stream>, #key content-type = "text/html")
  write(stream, "HTTP/1.0 200 OK\r\n");
  format(stream, "Content-type: %s\r\n\r\n", content-type);
end method;

define method write-standard-ending(stream :: <stream>)
  write(stream, "\r\n");
  force-output(stream);
end method;

define macro with-standard-http-result
{ with-standard-http-result(?stream:expression)
    ?:body
  end }
 => { block()
        write-standard-headers(?stream);
        ?body
      cleanup
        write-standard-ending(?stream);
      end }

{ with-standard-http-result(?stream:expression, ?content:expression)
    ?:body
  end }
 => { block()
        write-standard-headers(?stream, content-type: ?content);
        ?body
      cleanup
        write-standard-ending(?stream);
      end }
end macro with-standard-http-result;

define method quit-handler(server :: <simple-http-server>,
                           request :: <request>,
                           stream :: <stream>)
 => ()
  with-standard-http-result(stream)
    let dom =
      with-dom-builder()
        (html
          (head (title ["Shutting down server"])),
          (body ["Shutting down server..."]))
      end with-dom-builder;
    print-html(dom, stream); 
    write(stream, "\r\n");
    stop-http-server(server);
  end;  
end method quit-handler;

define method display-header-handler(server :: <simple-http-server>,
                                     request :: <request>,
                                     stream :: <stream>)
 => ()
  with-standard-http-result(stream)
    print-html(with-dom-builder()
                 (html
                   (head (title ["Headers"])),
                    (body
                      ((table border: 1)
                        (tr (td ["Header"]), (td ["Value"])),
                        [for(v keyed-by k in request.request-headers)
                           with-dom-builder(*current-dom-element*)
                             (tr (td [k]), (td [v]))
                           end with-dom-builder;
                         end for])))
               end, stream);
    write(stream, "\r\n");
  end;
end method;

define method initialize-http-server() => ()
  start-sockets();
end method initialize-http-server;

