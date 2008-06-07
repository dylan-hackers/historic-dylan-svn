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

