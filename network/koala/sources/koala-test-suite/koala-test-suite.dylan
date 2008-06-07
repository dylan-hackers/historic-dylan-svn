Module: koala-test-suite

define constant fmt = format-to-string;

define variable *test-port* :: <integer> = 8080;

define method make-listener
    (address :: <string>) => (listener :: <string>)
  format-to-string("%s:%d", address, *test-port*)
end;

define constant $listener-any = make-listener("0.0.0.0");
define constant $listener-127 = make-listener("127.0.0.1");

define function make-server
    (#rest keys, #key listeners, #all-keys)
  apply(make, <http-server>,
        listeners: listeners | list($listener-any),
        keys)
end;

define function connect-and-close
    (addr, #key port = *test-port*)
  block ()
    with-http-stream(stream to addr, port: port)
      #t
    end;
  exception (ex :: <connection-failed>)
    #f
  end;
end function connect-and-close;


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

    log-debug("STARTING SERVER WITH ADDRS = %s", addrs);
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

define suite koala-test-suite ()
  suite start-stop-test-suite;
  suite configuration-test-suite;
  suite xml-rpc-test-suite;
end suite koala-test-suite;

define function main ()
  start-sockets();
  run-test-application(koala-test-suite);
end;

begin
  main();
end;

