Module:    http-server
Synopsis:  Simple HTTP Server
Author:    Chris Double
Copyright: (C) 2001, Chris Double.  All rights reserved.

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
                                    request-type :: <symbol>,
                                    requested-path :: <string>,
                                    headers :: <string-table>,
                                    stream :: <stream>)
 => ();

define method process-handler(handler :: <handler>, 
                              server :: <simple-http-server>,
                              request-type :: <symbol>,
                              requested-path :: <string>,
                              headers :: <string-table>,
                              stream :: <stream>)
 => ()
end method process-handler;

define method can-handle-request?(handler :: <handler>,
                                  request-type :: <symbol>,
                                  path :: <string>,
                                  host :: false-or(<string>))
 => (r :: <boolean>)
  local method path-matches?() => (r :: <boolean>)
    (handler.handler-exact-match? 
       & handler.handler-path = path) 
              |
    (~handler.handler-exact-match? &
        path.size > handler.handler-path.size &
        handler.handler-path = copy-sequence(path, end: handler.handler-path.size));
  end;
 
  local method request-type-matches?() => (r :: <boolean>)
    (handler.handler-request-type &
      handler.handler-request-type == request-type)
                   |
    ~handler.handler-request-type;
  end;

  local method host-matches?() => (r :: <boolean>)
    ~host 
              |
    (handler.handler-host &
      handler.handler-host = host)
                   |
    ~handler.handler-host;                   
  end;

  path-matches?() & request-type-matches?() & host-matches?();
end method can-handle-request?;
                                  
define open class <dynamic-handler> (<handler>)
  slot handler-function :: <function>, required-init-keyword: function:;
end class;

define method process-handler(handler :: <dynamic-handler>, 
                              server :: <simple-http-server>,
                              request-type :: <symbol>,
                              requested-path :: <string>,
                              headers :: <string-table>,
                              stream :: <stream>)
 => ()
  handler.handler-function(server, request-type, requested-path, headers, stream);
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


define method decode-request(request-line :: <string>)
 => (request :: <symbol>, path :: <string>, protocol :: <symbol>)
  let split = split-string(request-line, " ");
  values(as(<symbol>, split.first),
         split.second, 
         as(<symbol>, split.third));
end method decode-request;

define method extract-headers(stream :: <stream>)
 => (r :: false-or(<string-table>))
  let result = make(<string-table>);
  for(line = read-line(stream, on-end-of-stream: #f)
      then read-line(stream, on-end-of-stream: #f),
      until: ~line | empty?(line) )
	let p = subsequence-position(line, ":");
	let key = copy-sequence(line, end: p);
	let value = copy-sequence(line, start: p + 2);
	result[key] := value;
  end for;
  result;
end method extract-headers;

define method find-matching-handler(server :: <simple-http-server>, 
                                    request-type :: <symbol>,
                                    path :: <string>,
                                    host :: false-or(<string>))
 => (r :: false-or(<handler>))
  // Look for exact matches first.
  let found = 
    block(return)
      for(handler in server.http-server-handlers)
        when(handler.handler-exact-match? & 
             can-handle-request?(handler, request-type, path, host))
          return(handler);
        end when;
      end for;
    end block;

  unless(found)
    // Now look for in-exact matches
    for(handler in server.http-server-handlers)
      unless(handler.handler-exact-match?)
        when(can-handle-request?(handler, request-type, path, host))
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
                                request-type :: <symbol>,
                                requested-path :: <string>,
                                headers :: <string-table>,
                                stream :: <stream>)
 => ()
  let dom = 
    with-dom-builder()
      (html
        (head (title ["File not found"])),
        (body 
          (p ["I could not find the file "],
             [requested-path],
             [" on this server."])))          
    end with-dom-builder;
  print-html(dom, stream);
end method handler-not-found;

define method handle-request(server :: <simple-http-server>, remote-socket :: <stream>)
  let request :: <string> = read-line(remote-socket);
  let (request-type, path, protocol) = decode-request(request);
  let headers :: <string-table> = extract-headers(remote-socket);

  // Look for 'Host' header.
  let host = element(headers, "host", default: #f);
  let handler = find-matching-handler(server, request-type, path, host);

  if(handler)
    process-handler(handler, server, request-type, path, headers, remote-socket);
    format-out("Handling: %=\n", path);
  else
    write(remote-socket, "HTTP/1.0 404 Not Found\r\n");
    write(remote-socket, "Content-type: text/html\r\n");
    write(remote-socket, "\r\n");
    handler-not-found(server, request-type, path, headers, remote-socket);
    write(remote-socket, "\r\n\r\n");
    force-output(remote-socket);
    format-out("Not Found...\n");
  end if;
end method handle-request;

define method start-http-server(server :: <simple-http-server>, #key port = 80)
 => ()
  with-server-socket(server-socket, port: port)
    server.http-server-socket := server-socket;
    start-server(server-socket, remote-socket)
      process-client-connection(server, remote-socket);
    end;
  end with-server-socket;
end method start-http-server;

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
                           request-type :: <symbol>,
                           requested-path :: <string>,
                           headers :: <string-table>,
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
    make(<thread>, function: method() 
                               sleep(1); 
                               close(server.http-server-socket);
                               server.http-server-socket := #f;
                             end);
  end;  
end method quit-handler;

define method display-header-handler(server :: <simple-http-server>,
                           request-type :: <symbol>,
                           requested-path :: <string>,
                           headers :: <string-table>,
                           stream :: <stream>)
 => ()
  with-standard-http-result(stream)
    print-html(with-dom-builder()
                 (html
                   (head (title ["Headers"])),
                    (body
                      ((table border: 1)
                        (tr (td ["Header"]), (td ["Value"])),
                        [for(v keyed-by k in headers)
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

