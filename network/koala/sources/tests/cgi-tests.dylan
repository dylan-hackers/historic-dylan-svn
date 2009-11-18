Module: koala-test-suite
Synopsis: Tests for CGI functionality

// CGI 1.2 specification draft: http://ken.coar.org/cgi/cgi-120-00a.html
// CGI 1.1 "spec": http://hoohoo.ncsa.illinois.edu/cgi/interface.html

//add-target(get-logger("http.common"), $stdout-log-target);

define suite cgi-test-suite ()
  test cgi-location-header-test;
  test cgi-required-environment-variables-test;
  test cgi-status-header-test;
  test cgi-command-line-test;
  test cgi-authentication-test;
end suite cgi-test-suite;

define function make-cgi-server
    (#key server-root :: <string>) => (server :: <http-server>)
  let cfg = "<?xml version=\"1.0\"?>\n"
            "<koala>\n"
            "<server-root   location=\"%s\" />\n"
            "<document-root location=\".\" />\n"
            "<debug-server value=\"on\"/>\n"
            "<debug-log level=\"trace\"/>\n"
            "<error-log level=\"trace\"/>\n"
            "<request-log level=\"trace\"/>\n"
            "<directory pattern=\"/*\" allow-cgi=\"yes\" cgi-extensions=\"cgi,bat,exe\"/>\n"
            "</koala>\n";
  let server = make-server();
  configure-from-string(server, fmt(cfg, server-root), "<no-file>");
  server
end function make-cgi-server;

// Environment variables that MUST be set according to RFC 3875.
// Table values are default variable values, if any.
//
define table $required-cgi-variables :: <string-table>
  = { "GATEWAY_INTERFACE" => "CGI/1.1",
      "QUERY_STRING" => #f,
      "REMOTE_ADDR" => #f,
      "REMOTE_HOST" => #f,
      "REQUEST_METHOD" => #f,
      "SCRIPT_NAME" => #f,
      "SERVER_NAME" => #f,
      "SERVER_PORT" => #f,
      "SERVER_PROTOCOL" => "HTTP/1.1",
      "SERVER_SOFTWARE" => #f };

define table $all-cgi-variables :: <string-table>
  = { "AUTH_TYPE" => #f,
      "CONTENT_LENGTH" => #f,
      "CONTENT_TYPE" => #f,
      "PATH_INFO" => #f,
      "PATH_TRANSLATED" => #f,
      "REMOTE_IDENT" => #f,
      "REMOTE_USER" => #f };

begin
  for (val keyed-by key in $required-cgi-variables)
    $all-cgi-variables[key] := val;
  end;
end;

define function cgi-directory ()
  // We invoke koala-test-suite as a CGI script, hence using application-filename.
  as(<string>, locator-directory(as(<file-locator>, application-filename())))
end;

// Verify that the expected environment variables are set when a CGI script
// is invoked.
// todo -- for now this just verifies that they're set but doesn't really
//         check the values.
//
define test cgi-required-environment-variables-test ()
  with-http-server (server = make-cgi-server(server-root: cgi-directory()))
    let url = make-test-url("/koala-test-suite.exe?cgi=env");

    with-http-connection (conn = url)
      send-request(conn, "GET", url);
      let response :: <http-response> = read-response(conn);

      // Build the environment from the response content.  On Windows the
      // lines are terminated with \r\n so we need to trim the trailing \r
      // after splitting.
      let env = make(<string-table>);
      do(method (line)
           let (var, val) = apply(values, split(line, "=", count: 2));
           env[var] := trim(val);
         end,
         split(response.response-content, "\n"));

      log-debug("env.size = %d", env.size);
      for (val keyed-by key in env)
        log-debug("%= = %=", key, val);
      end;

      for (expected keyed-by var in $required-cgi-variables)
        check-true(fmt("%s environment variable is set", var),
                   key-exists?(env, var));
        if (expected)
          check-equal(fmt("%s environment variable = %s", var, expected),
                      expected,
                      element(env, var, default: #f));
        end;
      end for;
    end with-http-connection;
  end with-http-server;
end test cgi-required-environment-variables-test;

// CGI "script" for cgi=env.  See main().
// Output the values of the CGI environment variables on stdout.
//
define method cgi-show-environment ()
  format-out("Content-type: text/plain\n");
  format-out("X-my-test-header: foo\n");
  format-out("\n");
  for (var in key-sequence($all-cgi-variables))
    format-out("%s=%s\n", var, environment-variable(var) | "");
  end;
end method cgi-show-environment;


// Verify that the Location header is respected when emitted by a CGI script
//
define test cgi-location-header-test ()
  let server = make-cgi-server(server-root: cgi-directory());
  let redirected? = #f;
  add-responder(server,
                "/cgi-location-header-test",
                method ()
                  log-debug("Executing responder for /cgi-location-header-test");
                  redirected? := #t;
                end);
  with-http-server (server = server)
    let url = make-test-url("/koala-test-suite.exe?cgi=location");

    with-http-connection (conn = url)
      send-request(conn, "GET", url);
      let response :: <http-response> = read-response(conn);
      check-true("Location header redirected to /cgi-location-header-test",
                 redirected?);
    end;
  end;
end test cgi-location-header-test;

// CGI "script" for "cgi=location".  See main().
// Output a Location header, which should cause that URL to be processed
// by the local server if it's a relative URL.
//
define function cgi-emit-location-header ()
  format-out("Location: /cgi-location-header-test\n");
  format-out("\n");
end;


// Verify that the Status header is passed through to the client as the
// status code in the response line.
//
define test cgi-status-header-test ()
  let server = make-cgi-server(server-root: cgi-directory());
  let expected-content = "Status header worked";
  add-responder(server,
                "/cgi-status-header-test",
                curry(output, expected-content));
  with-http-server (server = server)
    let url = make-test-url("/koala-test-suite.exe?cgi=status");

    with-http-connection (conn = url)
      send-request(conn, "GET", url);
      let response :: <http-response> = read-response(conn);
      check-equal("Received 201 from /cgi-status-header-test",
                  201,
                  response.response-code);
      check-equal("Received expected content for /cgi-status-header-test",
                  expected-content,
                  response.response-content);
    end;
  end;
end test cgi-status-header-test;

// CGI "script" for "cgi=status".  See main().
// Output a Status header, which should be passed back to the client
// as the status code in the response line.
//
define function cgi-emit-status-header ()
  format-out("Status: 201\n");
  format-out("\n");
  format-out("Status header worked")
end;


// Verify that a raw HTTP response, including the response line
// (e.g., HTTP/1.1 200 OK) is passed back to the client verbatim.
// The CGI 1.1 "spec" says that this is done by having the script
// name begin with the prefix "nph-".
//
define test cgi-raw-response-test ()
end test cgi-raw-response-test;

define test cgi-command-line-test ()
end test cgi-command-line-test;

define test cgi-authentication-test ()
end test cgi-authentication-test;

