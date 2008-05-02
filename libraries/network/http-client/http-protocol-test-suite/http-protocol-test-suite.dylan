Module: http-protocol-test-suite
Synopsis: Test suite to validate conformance to HTTP 1.1 protocol spec (RFC 2616)
Author: Carl Gay

define suite http-protocol-test-suite ()
  suite http-method-test-suite;
end suite http-protocol-test-suite;

define suite http-method-test-suite ()
  test test-get-method;
  test test-post-method;
  test test-head-method;
  test test-put-method;
  test test-delete-method;
  test test-trace-method;
  test test-connect-method;
end;

define test test-get-method ()
  check-equal("GET /hello yields \"hello\"",
              simple-http-get(test-url("hello")),
              "hello");
end test test-get-method;

define test test-post-method ()
end test test-post-method;

define test test-head-method ()
end test test-head-method;

define test test-put-method ()
end test test-put-method;

define test test-delete-method ()
end test test-delete-method;

define test test-trace-method ()
end test test-trace-method;

define test test-connect-method ()
end test test-connect-method;

//---------------------------------------------------------------------
// utilities

define variable *test-host* :: <string> = "localhost";

define variable *test-port* :: <integer> = 80;

define variable *test-url-base-directory* :: <string> = "/http-test/";

define function test-url
    (url :: <string>) => (full-url :: <url>)
  parse-url(concatenate("http://", *test-host*, ":", *test-port*,
                        *test-url-prefix*, url))
end function test-url;

begin
  run-test-application(http-protocol-test-suite);
end;

