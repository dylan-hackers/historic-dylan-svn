Module: dylan-user

define library http-client
  use common-dylan;
  use http-common;
  use io,
    import: { format,
              format-out,
              streams };
  use logging;
  use network,
    import: { sockets };
  use strings;
  use uncommon-dylan;
  use uri;

  export http-client;
  export http-client-internals;
end library http-client;

// See also the exports from http-common
//
define module http-client
  // Connections
  create
    <http-connection>,
    with-http-connection,
    connection-host,
    connection-port,
    outgoing-chunk-size,
    outgoing-chunk-size-setter;

  // Progress protocol
  create
      note-bytes-sent,
      note-bytes-received;

  // Request/response
  create
    send-request,
    start-request,
    finish-request,
    read-response,
    <http-response>,
    response-content,
    http-get;

  // Utilities
  create
    encode-form-data;

end module http-client;

define module http-client-internals
  use common-dylan,
    exclude: { format-to-string };
  use format;
  use format-out;
  use http-client, export: all;
  use http-common;
  use logging;
  use sockets,
    exclude: { start-server };
  use streams;
  use strings;
  use uncommon-dylan;
  use uri;

  // Internals
  export
    connection-socket;

end module http-client-internals;

