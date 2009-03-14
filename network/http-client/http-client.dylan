Module: http-client-internals
Author: Carl Gay

/*

to-do list:

* See todos in code below.
* (optional?) strict mode in which reads/writes signal an error if the
  chunk size is wrong or content length is wrong.  give the user a way
  to recover from the error.

Examples:

let conn = make(<http-connection>, host: host, port: port, ...);

// The simplest GET possible at this level.
// (GET is the default request method.  HTTP/1.1 is the default version.)
//
send-request(conn, "/status");
let response :: <http-response> = read-response(conn);
...content is in response.response-content...


// Read streaming data (e.g., if it's too big to buffer it all).
//
send-request(conn, "/status");
let response :: <http-response> = read-response(conn, read-content: #f);
...read(response, n)...


// POST form data.
// Content will be automatically encoded if it is a table.
// A Content-Type header will be added if not otherwise provided.
send-request(conn, "/form",
             method: "POST",
             content: encode-form-data(form-data));
...


// Send streaming data.
start-request(conn,  "PUT", "/huge-file.gz");
...write(conn, "foo")...
finish-request(conn);
let response = read-response(conn);


// Error handling
block ()
  send-request(conn, ...);
  let response = read-response(conn, ...);
exception (ex :: <resource-not-found-error>)
  ...
exception (ex :: <http-error>)
  ...last resort handler...
end;

close(conn);

// The send-request signature...
define method send-request
    (conn :: <http-connection>,
     url :: <uri>
     #key headers :: <object>,
          method :: type-union(<byte-string>, <symbol>),
          send-standard-headers = #t,
          http-version :: <symbol> = #"http/1.1",
          content :: <object>)
 => ()

// The plan is to allow content to be a stream, a string, a function,
// a table (for POST), etc.


*/

// Add a target to this if you want to see logging for the client.
//
define constant $log :: <logger>
  = make(<logger>, name: "http.client");

// By the spec request methods are case-sensitive, but for convenience
// we let them be specified as symbols as well.  If a symbol is used it
// is uppercased before sending to the server.  Similarly for HTTP version.
//
define constant <request-method> = type-union(<symbol>, <byte-string>);
define constant <http-version> = type-union(<symbol>, <byte-string>);


// For sending requests, an <http-connection> acts as the output stream so
// that it can do chunking etc.  But note that the request line and the headers
// are written directly to the socket so as to avoid chunking etc.
//
// For reading responses, the <http-connection> is used to create a response
// object and initialize it with the message headers and Status-Line data,
// after which one reads from the response object itself.
//
define open class <http-connection> (<basic-stream>)
  slot connection-socket :: <tcp-socket>;
  slot connection-host :: <string>;

  slot outgoing-chunk-size :: <integer>,
    init-value: 8192,
    init-keyword: outgoing-chunk-size:;

  slot connection-sent-headers :: false-or(<table>),
    init-value: #f;
  slot write-buffer :: <byte-string>;
  slot write-buffer-index :: <integer>,
    init-value: 0;
  // Number of bytes written so far for the current request message body only.
  slot message-bytes-written :: <integer>,
    init-value: 0;

end class <http-connection>;

define method initialize
    (conn :: <http-connection>, #rest socket-args, #key host)
  next-method();
  conn.connection-socket := apply(make, <tcp-socket>,
                                  remove-keys(socket-args, outgoing-chunk-size:));
  conn.write-buffer := make(<byte-string>,
                             size: conn.outgoing-chunk-size, fill: ' ');

  // We store the GIVEN host name locally so we're not subject to the vaguaries
  // of the <tcp-socket> implementation.  The doc implies that it may be converted
  // to the canonical host name and we generally want to send Host headers with
  // the host name we were given by the user.  (The port number, on the other
  // hand, we can get from the socket.)
  conn.connection-host := host;
end method initialize;

define method connection-port
    (conn :: <http-connection>)
 => (port :: <integer>)
  conn.connection-socket.local-port
end method connection-port;

define method chunked?
    (conn :: <http-connection>)
 => (chunked? :: <boolean>)
  let sent-headers = conn.connection-sent-headers;
  sent-headers & ~get-header(sent-headers, "Content-Length")
end method chunked?;

// Override this to create a progress meter for sending request data.
// (Byte count is only for message body data, not headers, chunk wrappers, etc.)
// 
define open generic note-bytes-sent
    (conn :: <http-connection>, byte-count :: <integer>);

define method note-bytes-sent
    (conn :: <http-connection>, byte-count :: <integer>)
  // default method does nothing
end;


//////////////////////////////////////////
// Writing requests
//////////////////////////////////////////

define generic start-request
    (conn :: <http-connection>,
     request-method :: <request-method>,
     url :: type-union(<uri>, <string>),
     #key headers,
          standard-headers = #t,
          http-version :: <http-version>)
 => ();

// todo -- http-version ~= 1.1 not supported yet
//
define method start-request
    (conn :: <http-connection>,
     request-method :: <request-method>,
     url :: type-union(<uri>, <string>),
     #key headers,
          standard-headers = #t,
          http-version :: <http-version> = #"HTTP/1.1")
 => ()
  if (instance?(url, <string>))
    url := parse-uri(url);
  end;
  if (instance?(http-version, <string>))
    http-version := as(<symbol>, http-version);
  end;
  let headers = convert-headers(headers);
  let proxy? = #f;  // todo -- probably in the connection

  // Determine the URL string to send in the request line.  If using a proxy an
  // absolute URI is required, otherwise HTTP/1.1 clients MUST send an abs_path
  // (a.k.a. path-absolute) and send a Host header.
  let url-string = iff(proxy?,
                       build-uri(url),
                       build-uri(url, include-scheme: #f, include-authority: #f));

  send-request-line(conn, request-method, url-string, http-version);

  if (standard-headers)
    // Add standard headers unless user has already set them.
    if (http-version = #"HTTP/1.1")
      // Host
      // uri-host should return #f, not "". :-(
      if (~get-header(headers, "Host"))
        add-header(headers, "Host", iff(empty?(uri-host(url)),
                                        conn.connection-host,
                                        uri-host(url)));
      end;
    end;
  end;

  // If the user set the content length explicitly, we trust them.
  // Otherwise the transfer is chunked.
  unless (get-header(headers, "Content-Length"))
    add-header(headers, "Transfer-Encoding", "chunked", if-exists?: #"ignore");
  end;

  send-headers(conn, headers);
end method start-request;


define generic send-request
    (conn :: <http-connection>, request-method :: <request-method>,
     url :: type-union(<uri>, <string>),
     #rest start-request-args,
     #key content :: <byte-string>,
     #all-keys)
 => ();

define method send-request
    (conn :: <http-connection>, request-method :: <request-method>,
     url :: type-union(<uri>, <string>),
     #rest start-request-args,
     #key content :: <byte-string> = "",
          headers)
 => ()
  let headers = convert-headers(headers);
  if (~get-header(headers, "Content-Length")
        & ~chunked-transfer-encoding?(headers))
    add-header(headers, "Content-Length", integer-to-string(content.size));
  end;
  apply(start-request, conn, request-method, url,
        headers: headers, content: content, start-request-args);
  write(conn, content);
  finish-request(conn);
end method send-request;

define generic finish-request
    (conn :: <http-connection>)
 => ();

define method finish-request
    (conn :: <http-connection>)
 => ()
  if (conn.write-buffer-index > 0)
    send-write-buffer(conn);
  end;
  if (chunked?(conn))
    // zero-length chunk terminates message body
    send-chunk(conn);
  end;
end method finish-request;

// Send Request-Line = Method SP Request-URI SP HTTP-Version CRLF
// 
define method send-request-line
    (conn :: <http-connection>,
     request-method :: <request-method>,
     url :: type-union(<uri>, <byte-string>),
     http-version :: type-union(<byte-string>, <symbol>))
  format(conn.connection-socket, "%s %s %s\r\n",
         iff(instance?(request-method, <symbol>),
             as-uppercase(as(<byte-string>, request-method)),
             request-method),
         // The client MUST omit the URI host unless sending to a proxy.
         // (Since we don't support proxies yet, the user can do this manually
         // by passing the url as a string.)
         iff(instance?(url, <uri>),
             build-uri(url, include-scheme: #f, include-authority: #f),
             url),
         iff(instance?(http-version, <symbol>),
             as-uppercase(as(<byte-string>, http-version)),
             http-version));
end method send-request-line;

define method send-headers
    (conn :: <http-connection>, headers :: <table>)
  let stream :: <tcp-socket> = conn.connection-socket;
  for (header-value keyed-by header-name in headers)
    format(stream, "%s: %s\r\n", header-name, header-value);
  end;
  write(stream, "\r\n");
  conn.connection-sent-headers := headers;
end method send-headers;

define method write-element
    (conn :: <http-connection>, char :: <byte-character>)
 => ()
  if (conn.write-buffer-index = conn.write-buffer.size)
    send-write-buffer(conn);
  end;
  conn.write-buffer[conn.write-buffer-index] := char;
  inc!(conn.write-buffer-index);
end method write-element;

define method write
    (conn :: <http-connection>, string :: <byte-string>,
     #key start: bpos = 0, end: epos)
 => ()
  let epos :: <integer> = epos | string.size;
  let wbuff :: <byte-string> = conn.write-buffer;
  while (bpos < epos)
    let wpos :: <integer> = conn.write-buffer-index;
    if (wpos = wbuff.size)
      send-write-buffer(conn);
      wpos := conn.write-buffer-index;
    end;
    wbuff[wpos] := string[bpos];
    inc!(bpos);
    inc!(conn.write-buffer-index);
  end;
end method write;

define inline function send-write-buffer
    (conn :: <http-connection>)
  if (chunked?(conn))
    send-chunk(conn);
  else
    write(conn.connection-socket, conn.write-buffer,
          end: conn.write-buffer-index);
  end;
  conn.write-buffer-index := 0;
  note-bytes-sent(conn, conn.message-bytes-written);
end function send-write-buffer;

// Note that if the chunk is zero bytes long that signals the end of the
// HTTP message.
//
define function send-chunk
    (conn :: <http-connection>)
  let socket :: <tcp-socket> = conn.connection-socket;
  let count :: <integer> = conn.write-buffer-index;
  write(socket, integer-to-string(count, base: 16));
  write(socket, "\r\n");
  write(socket, conn.write-buffer, end: count);
  write(socket, "\r\n");
  inc!(conn.message-bytes-written, count);
  // todo -- If strict mode (?) check the bytes written against the
  //         content-length header.
end function send-chunk;


// Headers may be supplied to send-request in various forms for convenience.
// These methods on convert-headers all convert them to a <header-table>.

define method convert-headers
    (headers == #f)
  make(<header-table>)
end method convert-headers;

define method convert-headers
    (headers :: <header-table>)
  headers
end method convert-headers;

define method convert-headers
    (headers :: <sequence>)
  let new-headers = make(<header-table>);
  for (item in headers)
    let header-name :: <byte-string> = item[0];
    let header-value :: <byte-string> = item[1];
    new-headers[header-name] := header-value;
  end;
  new-headers
end method convert-headers;

define method convert-headers
    (headers :: <table>)
  let new-headers = make(<header-table>);
  for (header-value keyed-by header-name in headers)
    new-headers[header-name] := header-value;
  end;
  new-headers
end method convert-headers;


//////////////////////////////////////////
// Response
//////////////////////////////////////////

define open primary class <http-response>
    (<chunking-input-stream>, <base-http-response>)
  // Stores the content of the response, unless the user chose to read
  // streaming content from the response instead.
  slot response-content :: false-or(<byte-string>),
    init-value: #f,
    init-keyword: content:;
end class <http-response>;

define method make
    (class :: subclass(<http-response>), #rest args, #key connection, #all-keys)
 => (response :: <http-response>)
  apply(next-method, class, inner-stream: connection.connection-socket, args)
end;

// Read the status line and headers from the given connection and return an
// <http-response> object.  If "read-content" is true (the default) then the
// entire message body is read and stored in the response object.  Otherwise
// the stream is positioned to read the body of the response, which is the
// responsibility of the caller.
//
define open generic read-response
    (conn :: <http-connection>,
     #key read-content :: <boolean>,
          response-class :: subclass(<http-response>))
 => (response :: <http-response>);

define method read-response
    (conn :: <http-connection>,
     #key read-content :: <boolean> = #t,
          response-class :: subclass(<http-response>) = <http-response>)
 => (response :: <http-response>)
  let socket :: <tcp-socket> = conn.connection-socket;
  let (http-version, status-code, reason-phrase) = read-status-line(socket);
  let headers :: <header-table> = read-message-headers(socket);
  let response = make(response-class,
                      connection: conn,
                      // todo -- add version to <http-response> class
                      http-version: http-version,
                      code: status-code,
                      reason-phrase: reason-phrase,
                      headers: headers);
  if (read-content
        & (status-code ~= 204 /* $status-code-no-content */))
    response.response-content := read-to-end(response);
  end;
  if (status-code >= 400)
    signal(make(condition-class-for-status-code(status-code),
                format-string: "%s",
                format-arguments: list(reason-phrase),
                code: status-code));
  elseif (status-code >= 300 & follow-redirects)
    follow-redirects(conn, response)
  else
    response
  end
end method read-response;

define method follow-redirects
    (conn :: <http-connection>, response :: <http-response>)
 => (response :: <http-response>)
  // Reminder: 302 and 307 require subtly different treatment
  not-implemented-error(what: "follow-redirects");
end method follow-redirects;

// Read the status line from the response.  Signal <internal-server-error>
// (code 500) if that status line is not valid.
//
// Status-Line = HTTP-Version SP Status-Code SP Reason-Phrase CRLF
//
define method read-status-line
    (stream :: <tcp-socket>)
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

  if (version-string & status-string & reason-phrase)
    let version :: <symbol> = validate-http-version(version-string);
    let status-code :: <integer> = validate-http-status-code(status-string);
    values(version, status-code, reason-phrase)
  else
    // The rationale for 500 here is that if the server sent us an incomplete
    // status line it is probably completely hosed.
    signal(make(<internal-server-error>,
                format-string: "Invalid status line in HTTP response: %=",
                format-arguments: list(copy-sequence(buffer, end: eol)),
                code: 500));
  end
end method read-status-line;


///////////////////////////////////////////
// Convenience APIs
///////////////////////////////////////////

define function make-http-connection
    (host-or-url, #rest initargs, #key port, #all-keys)
  let host = host-or-url;
  let port = port | $default-http-port;
  if (instance?(host, <uri>))
    let uri :: <uri> = host;
    host := uri-host(uri);
    if (empty?(host))
      error(make(<simple-error>,
                 format-string: "The URI provided to with-http-connection "
                   "must have a host component: %s",
                 format-arguments: list(build-uri(host))));
    end if;
    port := uri-port(uri) | port;
  end if;
  apply(make, <http-connection>, host: host, port: port, initargs)
end function make-http-connection;

// with-http-connection(conn = url) blah end;
// with-http-connection(conn = host, ...<http-connection> initargs...) blah end;
//
// Note that you'll need to add-header("Connection", "Keep-alive") if you intend
// to use the connection for multiple requests and the server doesn't assume
// keep-alive.
//
define macro with-http-connection
  { with-http-connection (?conn:name = ?host-or-url:expression, #rest ?initargs:*)
      ?:body
    end }
    => { let _conn = #f;
         block ()
           _conn := make-http-connection(?host-or-url, ?initargs);
           let ?conn = _conn;
           ?body
         cleanup
           if (_conn)
             close(_conn, abort?: #t)
           end;
         end }
end macro with-http-connection;

// Return the content of the given URL as a string, or, if a stream is
// supplied write the content to that stream and return #f.
//
define open generic http-get
    (url :: <object>, #key stream)
 => (content :: false-or(<string>));

define method http-get
    (url :: <byte-string>, #key stream)
 => (content :: false-or(<string>))
  http-get(parse-uri(url), stream: stream)
end method http-get;

define method http-get
    (url :: <uri>, #key stream)
 => (content :: false-or(<string>))
  let host = uri-host(url);
  with-http-connection(conn = host, port: uri-port(url))
    send-request(conn, "GET", url,
                 headers: #[#["Connection", "close"]]);
    let response :: <http-response> = read-response(conn, read-content: #f);
    if (stream)
      block ()
        while (#t)
          write(stream, read(response, 8192));
        end;
      exception (ex :: <incomplete-read-error>)
        write(stream, ex.stream-error-sequence);
      exception (ex :: <end-of-stream-error>)
        // pass
      end;
      #f
    else
      read-to-end(response)
    end
  end
end method http-get;

