Module: http-client-test-suite

/*
tests to write:
* send chunks of size 1, chunk-size, chunk-size - 1, and chunk-size + 1
  and verify that the correct content is received.  need an echo server.
* chunked and non-chunked requests and responses
* verify that adding a method on stream-sequence-class has the intended
  effect.  (i.e., read-to-end gives me a <byte-string> not a <vector>)
* verify error semantics for requests/responses with incorrect size or
  bad chunk size.
*/

///////////////////////////
// Utilities and responders
///////////////////////////

define constant $log :: <logger>
  = make(<logger>, name: "http.client.test-suite");

define variable *test-server* :: false-or(<http-server>) = #f;

define variable *test-port* :: <integer> = 7000;

define variable *test-host* :: <string> = "localhost";

define variable *url-prefix* :: <byte-string> = "/http-test";

// Make a full URL for HTTP requests.
define function full-url
    (url :: <string>, #key secure = #f) => (url :: <string>)
  format-to-string("http://%s:%d%s",
                   *test-host*, *test-port*, short-url(url))
end;

// Make URLs for registering with the server (i.e., just a path)
define function short-url
    (url :: <string>) => (url :: <string>)
  format-to-string("%s%s", *url-prefix*, url)
end;


define function x-responder ()
  let n = get-query-value("n", as: <integer>);
  output(make(<byte-string>, size: n, fill: 'x'))
end;

define function make-x-url
    (n :: <integer>)
 => (url)
  full-url(format-to-string("/x?n=%d", n))
end function make-x-url;

// Responder that echos the message body back verbatim to the response.
//
define function echo-responder ()
  // should eventually be output(read-to-end(current-request()))
  output(request-content(current-request()))
end;

define function register-test-responders
    (server :: <http-server>)
  add-responder(server, short-url("/x"),
                table(compile-regex("^.*$") => list(x-responder)));
  add-responder(server, short-url("/echo"),
                table(compile-regex("^.*$") => list(echo-responder)));
end function register-test-responders;

define variable *test-suite-initialized?* = #f;

define function setup-http-client-test-suite
    ()
  if (~*test-suite-initialized?*)
    add-target(get-logger("http.common"), $stdout-log-target);
    add-target(get-logger("http.client"), $stdout-log-target);
    //logger-enabled?(get-logger("http.common.headers")) := #f;
    start-test-server();
    *test-suite-initialized?* := #t;
  end;
end function setup-http-client-test-suite;

define function cleanup-http-client-test-suite
    ()
  if (*test-server*)
    stop-server(*test-server*)
  end
end function cleanup-http-client-test-suite;

define function start-test-server
    (#key host = *test-host*, port = *test-port*)
  *test-server* := make(<http-server>,
                        listeners: list(list(host, port)));
  register-test-responders(*test-server*);
  start-server(*test-server*, background: #t, wait: #t);
end function start-test-server;


/////////////////////////////
// Tests
/////////////////////////////


// Test GETs with responses of various sizes.  For Koala, the largest
// one causes a chunked response.
//
define test test-http-get ()
  for (n in list(0, 1, 2, 8192, 100000))
    check-equal(format-to-string("GET %d-byte string of 'x's", n),
                http-get(make-x-url(n)),
                make(<byte-string>, size: n, fill: 'x'));
  end;
end test test-http-get;

// Test http-get with output done to a stream.
//
define test test-http-get-to-stream ()
  check-equal("http-get to a stream",
              "xxxx",
              with-output-to-string(stream)
                http-get(make-x-url(4), stream: stream)
              end);
end test test-http-get-to-stream;

define test test-encode-form-data ()
end test test-encode-form-data;

define test test-http-connections ()
end test test-http-connections;

define test test-with-http-connection ()
end test test-with-http-connection;

define test test-reuse-http-connection ()
  // The explicit headers here should be temporary.  I want to make
  // with-http-connection and send-request coordinate better to do
  // the keep-alive.
  with-http-connection (conn = *test-host*, port: *test-port*)
    send-request(conn, "GET", make-x-url(2),
                 headers: #[#["Connection", "Keep-alive"]]);
    let response :: <http-response> = read-response(conn);
    check-equal("first response is xx", response.response-content, "xx");

    send-request(conn, "GET", make-x-url(5),
                 headers: #[#["Connection", "Keep-alive"]]);
    let response :: <http-response> = read-response(conn);
    check-equal("second response is xxxxx", response.response-content, "xxxxx");
  end;
    // todo -- 
    // be sure to check what happens if we write more data to the request
    // stream than specified by Content-Length, and if the server sends
    // more data than specified by its Content-Length header.  i.e., do
    // we need to flush/discard the extra data to make the connection 
    // usable again...presumably.
end test test-reuse-http-connection;

define test test-streaming-request ()
  with-http-connection(conn = *test-host*, port: *test-port*)
    // This uses a content-length header because currently Koala doesn't
    // support requests with chunked encoding.
    start-request(conn, #"post", short-url("/echo"),
                  headers: #[#["Content-Length", "7"],
                             #["Content-Type", "text/plain"]]);
    write(conn, "abcdefg");
    finish-request(conn);
    check-equal("Streamed request data sent correctly",
                "abcdefg",
                response-content(read-response(conn)));
  end;
end test test-streaming-request;

define test test-streaming-response ()
  with-http-connection(conn = *test-host*, port: *test-port*)
    let data = make(<byte-string>, size: 10000, fill: 'x');
    send-request(conn, "POST", short-url("/echo"), content: data);
    let response :: <http-response> = read-response(conn, read-content: #f);
    check-equal("streamed response data same as sent data",
                read-to-end(response),
                data);
  end;
end test test-streaming-response;

define test test-chunked-request ()
  // See chunked-request-test in koala-test-suite
end test test-chunked-request;

define test test-read-chunked-response ()
  with-http-connection(conn = *test-host*, port: *test-port*)
    // currently no way to set response chunk size so make data bigger
    // than koala's $chunk-size.  koala adds Content-Length header if
    // entire response < $chunk-size.
    let data = make(<byte-string>, size: 100000, fill: 'x');
    send-request(conn, "POST", short-url("/echo"), content: data);
    let response :: <http-response> = read-response(conn);
    check-equal("response data same as sent data",
                response-content(response),
                data);
    check-false("ensure no Content-Length header",
                get-header(response, "Content-Length"));
    check-true("ensure Transfer-Encoding: chunked header",
               chunked-transfer-encoding?(response));
    // we don't currently have a way to verify that the response was
    // actually chunked.  
  end;
end test test-read-chunked-response;

define test test-non-chunked-request ()
end test test-non-chunked-request;

define test test-non-chunked-response ()
end test test-non-chunked-response;

define test test-resource-not-found-error ()
  check-condition("<resource-not-found-error> (404) signaled",
                  <resource-not-found-error>,
                  http-get(full-url("/no-such-url")));
end test test-resource-not-found-error;

define test test-invalid-response-chunk-sizes ()
end test test-invalid-response-chunk-sizes;

define test test-invalid-response-content-lengths ()
end test test-invalid-response-content-lengths;

define test test-invalid-request-content-lengths ()
end test test-invalid-request-content-lengths;

define test test-read-from-response-after-done ()
  with-http-connection(conn = *test-host*, port: *test-port*)
    send-request(conn, #"get", make-x-url(3));
    let response = read-response(conn, read-content: #t);
    check-condition("Reading past end of response raises <end-of-stream-error>",
                    <end-of-stream-error>,
                    read-element(response));
  end;
end test test-read-from-response-after-done;

define suite http-client-test-suite
    (setup-function: setup-http-client-test-suite,
     cleanup-function: cleanup-http-client-test-suite)
  test test-http-get;
  test test-http-get-to-stream;
  test test-encode-form-data;
  test test-with-http-connection;
  test test-http-connections;
  test test-reuse-http-connection;
  test test-streaming-request;
  test test-streaming-response;

  test test-chunked-request;
  test test-read-chunked-response;
  test test-non-chunked-request;
  test test-non-chunked-response;

  test test-resource-not-found-error;
  test test-invalid-response-chunk-sizes;
  test test-invalid-response-content-lengths;
  test test-invalid-request-content-lengths;

  test test-read-from-response-after-done;
  // todo -- test the reaction to server errors

end suite http-client-test-suite;

