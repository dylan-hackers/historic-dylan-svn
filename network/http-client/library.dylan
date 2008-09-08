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
    // Requests   (see also the methods on <base-http-request> in http-common)
    <http-request>,

    // Responses  (see also the methods on <base-http-request> in http-common)
    <http-response>,
    response-content,
    response-content-setter,
    send-http-request,
    simple-http-get,

    // Low level APIs
    open-http-stream,
    with-http-stream,
    read-http-response,
    close-http-stream,
    write-http-get;
end module http-client;
