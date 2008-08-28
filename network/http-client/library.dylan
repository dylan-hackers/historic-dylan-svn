Module: dylan-user

define library http-client
  use common-dylan;
  use http-common;
  use io,
    import: { format,
              format-out,
              streams };
  use network,
    import: { sockets };
  //use system,
  //  import: { locators };
  use uri;

  export http-client;
end library http-client;

define module http-client
  use common-dylan,
    exclude: { format-to-string };
  use format;
  use format-out;
  use http-common;
  use sockets,
    exclude: { start-server };
  use streams;
  //use locators;
  use uri;

  export
    <http-request>,
    <http-response>,
    send-http-request,
    simple-http-get,

    <http-error>,
    http-error-message,
    
    // Low level APIs
    open-http-stream,
    with-http-stream,
    read-http-response,
    close-http-stream,
    write-http-get,
    read-http-response-header,
    read-http-response-header-as;
end module http-client;
