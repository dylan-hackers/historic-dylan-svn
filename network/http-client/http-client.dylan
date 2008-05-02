Module: http-client

/// Parameters.

define variable *debug-http* :: <boolean> = #t;

define constant $default-http-port :: <integer> = 80;


/// Conditions

define class <http-error> (<error>)
  constant slot http-error-message :: <string>, 
    required-init-keyword: #"message";
  constant slot http-error-code :: <string>,
    init-value: #f,
    init-keyword: #"code";
end class <http-error>;

// FIXME: This doesn't change the way the error message is displayed in the IDE.
//        Why not?
define method condition-to-string
    (cond :: <http-error>) => (string :: <string>)
  if (cond.http-error-code)
    format-to-string("%s - %s", cond.http-error-code, cond.http-error-message)
  else
    cond.http-error-message
  end
end method condition-to-string;

// RFC 2616, 6.6.1
define function read-http-status-line
    (stream :: <stream>)
 => (http-version :: <symbol>, status-code :: <byte-string>, reason :: <byte-string>)
  let status-line = read-line(stream);
  when (*debug-http*)
    format-out("%s\n", status-line);
  end;
  let parts = split(status-line, ' ', count: 3);
  assert(parts.size == 3, "Invalid HTTP status line received: %=", status-line);
  let (http-version, status-code, reason-phrase) = apply(values, parts);
  values(as(<symbol>, http-version),
         status-code,
         reason-phrase)
end function read-http-status-line;

/// Session-level interface.

// API
define macro with-http-stream
  { with-http-stream (?:variable to ?host:expression, #rest ?args:*) ?:body end }
    => { let http-stream = #f;
         block ()
           http-stream := open-http-stream(?host, ?args);
           let ?variable = http-stream;
           ?body
         cleanup
           if (http-stream)
             close-http-stream(http-stream);
           end;
         end; }
end macro with-http-stream;

// API
define method open-http-stream 
    (host, #key port = $default-http-port) => (stream :: <stream>)
  let stream = make(<tcp-socket>, host: host, port: port);
  stream
end;

// API
define method close-http-stream
    (stream :: <stream>) => ()
  close(stream);
end;

// API
define method write-http-get
    (stream :: <stream>, host :: <byte-string>, path :: <byte-string>, #rest headers)
 => ()
  format-http-line(stream, "GET %s HTTP/1.1", path);
  // RFC 2616, 19.6.1.1 -- HTTP/1.1 clients MUST include the Host header.
  format-http-line(stream, "Host: %s", host);
  for (i from 0 below size(headers) by 2)
    let key = headers[i];
    let val = headers[i + 1];
    format-http-line(stream, "%s: %s", key, val);
  end;
  format-http-line(stream, "");
end method write-http-get;

// API
// Return the content of the given URL as a string.
//
define method simple-http-get
    (url :: <byte-string>) => (content :: <string>)
  let url :: <url> = as(<url>, url);
/*
  if (~instance?(url, <server-url>))
    error("You must specify the remote host in the URL.");
  end;
  let request-uri = locator-path(url);
  if (instance?(url, <cgi-url>))
    request-uri := concatenate(request-uri, "?", locator-cgi-string(url));
  end;
  if (instance?(url, <file-index-url>))
    request-uri := concatenate(request-uri, "#", locator-index);
  end;
*/
  let directory = locator-directory(url);
  let server = locator-server(directory);
  let host = locator-host(server);
  with-http-stream(stream to host, port: locator-port(server))
    write-http-get(stream, host, locator-as-string(<string>, url));
    let (http-version, status, reason-phrase) = read-http-status-line(stream);
    if (status[0] == '2')
      read-http-response-header(stream);
      read-to-end(stream)
    elseif (member?(status[0], "45"))
      error(make(<http-error>, message: reason-phrase, code: status));
    else
      error(make(<http-error>,
                 message: format-to-string("HTTP response code %s not yet implemented",
                                           status),
                 code: status));
    end
  end
end method simple-http-get;

// API
define method read-http-response-header
    (stream :: <stream>) => ()
  read-http-response-header-as(<string>, stream);
end;

// API
define method read-http-response-header-as
    (type :: subclass(<string>), stream :: <stream>) => ()
  with-output-to-string (string-stream)
    let line = #f;
    while ((line := read-line(stream)) ~= "")
      if (*debug-http*)
        format-out("%s\n", line);
      end;
      write-line(string-stream, line);
    end;
  end;
end method;

define method format-http-line 
    (stream :: <stream>, template :: <string>, #rest args) => ()
  when (*debug-http*)
    apply(format-out, template, args);
    format-out("\n");
  end;
  apply(format, stream, template, args);
  write(stream, "\r\n");
end method;

// eof

