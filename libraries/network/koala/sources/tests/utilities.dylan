Module: koala-test-suite


// Would this be useful in koala itself?
//
define macro with-http-server
  { with-http-server (?server:variable = ?ctor:expression) ?body:body end }
  => { let _server = #f;
       block ()
         _server := ?ctor;
         let ?server = _server;
         start-server(_server, background: #t, wait: #t);
         ?body
       cleanup
         if (_server)
           stop-server(_server);
         end
       end;
     }
end macro with-http-server;

define constant fmt = format-to-string;

define variable *test-port* :: <integer> = 8080;

define method make-test-url
    (path-etc :: <string>) => (url :: <url>)
  parse-url(fmt("http://localhost:%d%s", *test-port*, path-etc))
end;

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

define function echo-responder ()
  output(request-content(current-request()));
end;

