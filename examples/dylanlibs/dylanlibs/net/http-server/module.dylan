Module:    dylan-user
Synopsis:  Simple HTTP Server
Author:    Chris Double
Copyright: (C) 2000, Chris Double.  All rights reserved.

// Routines for handling URI encoding and decoding
define module encode-decode
  use functional-dylan;

  export 
    uri-encode,
    uri-decode,
    <form-query>,
    form-query-encode,
    form-query-decode;    
end module encode-decode;

define module http-server
  use functional-dylan;
  use format-out;
  use threads;
  use sockets;
  use streams;
  use format;
  use sequence-utilities;
  use dom-builder;
  use html-generator;
  use encode-decode;

  // Add binding exports here.
  export 
    <request>,
    request-type,
    request-path,
    request-headers,
    request-query,
    request-host,
    request-body,
    <handler>,
    handler-path,
    handler-host,
    handler-request-type,
    handler-exact-match?,
    process-handler,
    <dynamic-handler>,
    handler-function,
    publish-dynamic-handler,
    <file-handler>,
    handler-file-name,
    handler-cache?,
    <simple-http-server>,
    <threaded-http-server>,
    handler-not-found,
    handle-request,
    publish-handler,
    unpublish-handler,
    unpublish-all-handlers,
    process-client-connection,
    start-http-server,    
    write-standard-headers,
    write-standard-ending,
    with-standard-http-result,
    quit-handler,
    display-header-handler,
    initialize-http-server;
end module http-server;


