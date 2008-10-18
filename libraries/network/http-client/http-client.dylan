Module: http-client

/// Parameters.

define variable *debug-http* :: <boolean> = #t;

define constant $default-http-port :: <integer> = 80;

// temporary
define constant $log
  = make(<logger>,
         name: "http-client",
         targets: list($stdout-log-target));

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
  let host = uri-host(url);
  with-http-stream(stream to host, port: uri-port(url))
    write-http-get(stream, host, uri-path(url));
    let (http-version, status, reason-phrase) = read-http-status-line(stream);
    if (status == 200)
      read-http-response-header(stream);
      read-to-end(stream)
    elseif (status >= 400 & status <= 599)
      error(make(<http-error>,
                 format-string: reason-phrase,
                 code: status));
    else
      error(make(<http-error>,
                 format-string: "HTTP response code %s not yet implemented",
                 format-arguments: list(status),
                 code: status));
    end if
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
end method format-http-line;


// Represents an HTTP request.  Make one of these and pass it to
// send-http-request.
//
define open primary class <http-request> (<base-http-request>)
end class <http-request>;

define open primary class <http-response> (<base-http-response>)
  slot response-content :: <byte-string>,
    init-value: "",
    init-keyword: content:;
end class <http-response>;

define method send-http-request
    (request :: <http-request>,
     #key background :: <boolean>,
          follow-redirects :: <boolean>)
 => (response :: <http-response>)
  let url :: <url> = request.request-url;
  let host = uri-host(url) | "localhost";
  let port :: <integer> = uri-port(url) | $default-http-port;
  with-http-stream (http-stream to host, port: port)
    format(http-stream, "%s %s %s\r\n",
           as-uppercase(as(<byte-string>, request.request-method)),
           url,
           as-uppercase(as(<byte-string>, request.request-version)));

    let content :: <string> = request.request-content;
    // Send headers...
    // Host: header is required for HTTP 1.1 requests.
    // todo -- Verify that it is accepted/ignored for older protocol versions.
    // todo -- RFC 2616, 4.2: it is "good practice" to send
    //         general-header fields first, followed by request-header or response-
    //         header fields, and ending with the entity-header fields.
    add-header(request, "Host", host);
    add-header(request, "Content-Length", integer-to-string(content.size));
    for (header-value keyed-by header in request.raw-headers)
      format(http-stream, "%s: %s\r\n", header, header-value);
    end;

    // Blank line separates request headers from message body.
    write(http-stream, "\r\n");

    // todo -- preprocess content based on content-type.  e.g., form data
    write(http-stream, content);

    force-output(http-stream);
    let response :: <http-response> = read-http-response(request, http-stream);
    response

    // todo -- manage connections...keep-alive.
    //         for now one connection per request.
  end with-http-stream;
end method send-http-request;

define method read-http-response
    (request :: <http-request>, stream :: <stream>)
 => (response :: <http-response>)
  let (version, code, reason-phrase) = read-http-status-line(stream);
  let response = make(<http-response>,
                      request: request,
                      code: code,
                      version: version,
                      reason-phrase: reason-phrase);
  read-message-headers(stream, headers: response.raw-headers);
  read-response-content(response, stream);
  response
end method read-http-response;

// Read the status line from the response.  Signal <internal-server-error>
// (code 500) if that status line is not valid.
//
// Status-Line = HTTP-Version SP Status-Code SP Reason-Phrase CRLF
//
define method read-http-status-line
    (stream :: <stream>)
 => (version :: <symbol>,
     status-code :: <integer>,
     reason-phrase :: <string>)
  let (buffer, eol) = read-http-line(stream);
  let epos1 = whitespace-position(buffer, 0, eol);
  let bpos2 = epos1 & skip-whitespace(buffer, epos1, eol);
  let epos2 = bpos2 & whitespace-position(buffer, bpos2, eol);
  let bpos3 = epos2 & skip-whitespace(buffer, epos2, eol);

  let version-string = epos1 & copy-sequence(buffer, end: epos1);
  let status-string = epos2 & copy-sequence(buffer, start: bpos2, end: epos2);
  let reason-phrase = bpos3 & copy-sequence(buffer, start: bpos3, end: eol);

  log-debug($log, "Status-Line: version = %s, status = %s, reason = %s",
            version-string, status-string, reason-phrase);
  if (version-string & status-string & reason-phrase)
    let version :: <symbol> = validate-http-version(version-string);
    let status-code :: <integer> = validate-http-status-code(status-string);
    values(version, status-code, reason-phrase)
  else
    // The rationale for 500 here is that if the server sent us an incomplete
    // status line it is completely hosed.
    signal(make(<internal-server-error>,
                format-string: "Invalid status line in HTTP response: %=",
                format-arguments: list(copy-sequence(buffer, end: eol)),
                code: 500));
  end
end method read-http-status-line;

define method read-response-content
    (response :: <http-response>, stream :: <stream>)
 => (content :: <byte-string>)
  // ---TODO: Should probably try to continue here even if Content-Length
  //          not supplied.  Or have a "strict" option.
  let content-length = get-header(response, "Content-Length", parsed: #t)
                       | content-length-required-error();
  let buffer :: <byte-string> = make(<byte-string>, size: content-length);
  // todo -- What if we're not at end-of-stream after the read?
  //         Should we signal an error?
  let bytes-read = read-into!(stream, content-length, buffer);
  if (bytes-read == content-length)
    response-content(response) := buffer
  else
    // RFC 2616, 4.4
    bad-request(message: format-to-string("Request content size (%d) does not "
                                          "match Content-Length header (%d)",
                                          bytes-read, content-length));
  end
end method read-response-content;

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

