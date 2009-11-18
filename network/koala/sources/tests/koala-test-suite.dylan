Module: koala-test-suite

define function connect-and-close
    (addr, #key port = *test-port*)
  block ()
    with-http-connection(conn = addr, port: port)
      log-debug("Connected to %s:%s", addr, port);
      #t
    end;
  exception (ex :: <connection-failed>)
    #f
  end;
end function connect-and-close;


// Test creating <http-server>s with various settings.
//
/*
define test server-creation-test ()
  let server = make(<http-server>,
                    // how do i get the (or a) root directory
                    // in a platform independent way?
                    dsp-root: as(<directory-locator>, 
end test server-creation-test;
*/

define test start-stop-basic-test ()
  let server = make-server();
  block ()
    check-equal("start-stop-basic-test check #1",
                start-server(server, background: #t, wait: #t),
                #t);
  cleanup
    stop-server(server);
  end;
end test start-stop-basic-test;

// Make sure there are no timing problems related to threads and
// starting and stopping the server.
define test repeated-start-stop-test ()
  for (i from 1 to 5)
    let server = make-server();
    block ()
      check-equal(fmt("repeated-start-stop-test check #%d", i),
                  start-server(server, background: #t, wait: #t),
                  #t);
    cleanup
      stop-server(server);
    end;
  end;
end test repeated-start-stop-test;

define test conflicting-listener-ips-test ()
  let server = make-server(listeners: list($listener-127, $listener-127));
  block ()
    check-condition("start-server with conflicting listener-ips",
                    <address-in-use>,
                    start-server(server, background: #t, wait: #t));
  cleanup
    // Note that listener threads are still running.
    stop-server(server);
  end;
end test conflicting-listener-ips-test;

// Make sure we can bind specific IP addresses.
define test bind-interface-test ()
  let host-addresses = map(host-address, all-addresses($local-host));
  for (addrs in list(#["127.0.0.1"],
                     concatenate(host-addresses, #["127.0.0.1"]),
                     #["0.0.0.0"]))

    log-debug("Starting server with addrs = %s", addrs);
    with-http-server(server = make-server(listeners: map(make-listener, addrs)))
      for (addr in concatenate(host-addresses, #("127.0.0.1")))
        if (member?(addr, addrs, test: \=) | addrs = #["0.0.0.0"])
          check-true(fmt("address %s is listening for bound = %s", addr, addrs),
                     connect-and-close(addr));
        else
          check-false(fmt("address %s is NOT listening for bound = %s", addr, addrs),
                      connect-and-close(addr));
        end;
      end for;
    end with-http-server;
  end for;
end test bind-interface-test;

define suite start-stop-test-suite ()
  test start-stop-basic-test;
  test repeated-start-stop-test;
  test bind-interface-test;
  test conflicting-listener-ips-test;
end suite start-stop-test-suite;

// Test that the server can handle the "Transfer-encoding: chunked" header
// by setting the outgoing-chunk-size of the client's connection.
//
define test chunked-request-test ()
  // Client requests are chunked if we don't add a Content-Length header.
  with-http-server (server = make-server())
    add-responder(server, "/echo", echo-responder);
    with-http-connection(conn = make-test-url("/echo"),
                         outgoing-chunk-size: 8)
      for (data-size in #(0, 1, 7, 8, 9, 200))
        let data = make(<byte-string>, size: data-size, fill: 'x');
        send-request(conn, "POST", "/echo", content: data);
        let response = read-response(conn);
        check-equal(format-to-string("chunked request of size %d received correctly",
                                     data-size),
                    data, response-content(response));
      end for;
    end;
  end;
end test chunked-request-test;

define suite chunking-test-suite ()
  test chunked-request-test;
end;

// exported
define suite koala-test-suite
    (setup-function: start-sockets)
  suite start-stop-test-suite;
  suite chunking-test-suite;
  suite configuration-test-suite;
  suite xml-rpc-test-suite;
  suite vhost-test-suite;
  suite cgi-test-suite;
end suite koala-test-suite;

// This library may be used as a dll or as an executable.  If an executable,
// it does different things depending on the environment so that it can be
// invoked as a CGI script.
//
define method main () => ()
  let filename = locator-name(as(<file-locator>, application-name()));
  if (split(filename, ".")[0] = "koala-test-suite")
    let query = environment-variable("QUERY_STRING");
    if (~query)
      run-test-application(koala-test-suite);
    else
      // We're being invoked as a CGI script.
      // Note: don't log anything in this branch since it will become
      // part of the response.

      // Expecting "cgi=xxx" for various values of xxx.
      let parts = query & split(query, '=');
      let cgi = parts & parts.size >= 2 & parts[1];
      select (cgi by \=)
        "env" =>
          cgi-show-environment();
        "location" =>
          cgi-emit-location-header();
        "status" =>
          cgi-emit-status-header();
        otherwise =>
          error("unrecognized query string: %s", query);
      end;
    end;
  end;
end method main;

begin
  main()
end;

