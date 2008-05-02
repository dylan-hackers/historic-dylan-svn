Module: dylan-user

define library http-client
  use common-dylan;
  use network,
    import: { sockets };
  use io,
    import: { format,
              format-out,
              streams };
  use system,
    import: { locators };
  use koala;                    // to be replaced with http-common

  export http-client;
end library http-client;

define module http-client
  use common-dylan,
    exclude: { format-to-string };
  use sockets,
    exclude: { start-server };
  use streams;
  use format;
  use format-out;
  use locators;
//  use koala,                   // to be replaced with http-common
//    import: { };

  export <http-error>,
         http-error-message,
         with-http-stream,
         open-http-stream,
         close-http-stream,
         write-http-get,
         read-http-response-header,
         read-http-response-header-as,
         simple-http-get;
end module http-client;
