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
    (raw-url :: <byte-string>) => (content :: <string>)
  let url :: <url> = parse-uri(raw-url);
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
  let host = uri-host(url);
  with-http-stream(stream to host, port: uri-port(url))
    write-http-get(stream, host, uri-path(url));
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

define method format-http-line 
    (stream :: <stream>, template :: <string>, #rest args) => ()
  when (*debug-http*)
    apply(format-out, template, args);
    format-out("\n");
  end;
  apply(format, stream, template, args);
  write(stream, "\r\n");
end method;


/*
// Represents an HTTP request.  Make one of these and pass it to
// send-http-request.
//
define class <http-request> (<object>)
  constant slot request-url :: <url>,
    required-init-keyword: url:;
  constant slot request-http-version :: <symbol>,
    init-value: #"http/1.1",
    init-keyword: http-version:;
  constant slot request-method :: <symbol>,
    init-value: #"GET",
    init-keyword: method:;
  constant slot request-headers :: <table>,
    init-value: make(<table>),
    init-keyword: headers:;
  constant slot request-data :: false-or(<byte-string>),
    init-value: #f,
    init-keyword: data:;
end class <http-request>;

define method initialize
    (request :: <http-request>, #key url)
  // todo -- remove url fragment part
end;

define method send-http-request
    (request :: <http-request>,
     #key background :: <boolean>,
          follow-redirects :: <boolean>)
 => (response :: <http-response>)
  let host = uri-host(url);
  // todo -- is the port always set in the <uri> class?
  let port :: <integer> = uri-port(url) | $default-http-port;
  with-http-stream (http-stream to host, port: port)
    format(http-stream, "%s %s %s\r\n",
           as-uppercase(as(<byte-string>, request.request-method)),
           url,
           as-uppercase(as(<byte-string>, request.request-http-version)));

    // Send headers...
    // Host: header is required for HTTP 1.1 requests.
    // todo -- Verify that it is accepted/ignored for older protocol versions.
    // todo -- RFC 2616, 4.2: it is "good practice" to send
    //         general-header fields first, followed by request-header or response-
    //         header fields, and ending with the entity-header fields.
    add-header(request, "Host", host, replace: #f);
    add-header(request,
               "Content-Length",
               if (data) "0" else integer-to-string(data.size) end,
               replace: #t);
    for (header-value keyed-by header in request.request-headers)
      format(http-stream, "%s: %s\r\n", header, header-value)
    end;

    // Blank line separates request headers from message body.
    write(http-stream, "\r\n");

    // Send request body, if any.
    if (data)
      write(http-stream, data)
    end;

    force-output(http-stream);
    let response :: <http-response> = read-http-response(http-stream);
    response

    // todo -- manage connections...keep-alive.
    //         for now one connection per request.
  end with-http-stream;
end method send-http-request;

define method read-http-response
    (http-stream :: <stream>) => (response :: <http-response>)
  let 
*/
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

/*
// okay, i'm an idiot...i should probably move Koala's header parsing stuff
// into http-common instead, but this will do for now.
//
define method read-http-response-headers
    (stream :: <stream>) => (headers :: <string-table>)
  let headers :: <string-table> = make(<string-table>);

  local method consume-header (header-lines)
          // Construct a header/value pair out of multiple header lines (i.e., if
          // there were continuation lines) and store it in the headers table.
          // Note that this prefers later headers if there are duplicates.
          if (~empty?(header-lines))
            let header-lines = map(trim-whitespace, header-lines);
            let parts = split(join(header-lines, " "), ':', count: 2);
            if (parts.size < 2)
              if (*debug-http*)
                format-out("Ignoring bad response header: %s\n", header-lines);
              end;
            else
              let header-name = parts[0];
              let header-value = parts[1];
              headers[header-name] = header-value;
            end;
          end;
        end method consume-header;

  let current-header-lines = make(<stretchy-vector>);
  block (exit-block)
    while (#t)
      let line = read-line(stream);
      if (line = "")
        consume-header(current-header-lines);
        exit-block();
      end;
      if (~member?(line[0], " \t"))
        // new header starting, not a continuation line
        consume-header(current-header-lines);
        current-header-lines := make(<stretchy-vector>);
      end;
      add!(current-header-lines, line);
    end while;
  end block;
  headers
end method read-http-response-headers;
*/

// RFC 2616, 6.6.1
define function read-http-status-line
    (stream :: <stream>)
 => (http-version :: <symbol>, status-code :: <byte-string>, reason :: <byte-string>)
  let status-line = read-line(stream);
  when (*debug-http*)
    format-out("%s\n", status-line);
  end;
  let parts = split(status-line, ' ', count: 3);
  if (parts.size ~== 3)
    signal(make(<http-error>,
                format-string: "Invalid HTTP status line received: %=",
                format-argument: list(status-line)));
  else
    let (http-version, status-code, reason-phrase) = apply(values, parts);
    values(as(<symbol>, http-version),
           status-code,
           reason-phrase)
  end
end function read-http-status-line;


