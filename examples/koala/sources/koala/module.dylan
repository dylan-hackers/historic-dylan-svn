Module:    dylan-user
Synopsis:  HTTP Support
Author:    Gail Zacharias

define module http-server
  create
    <avalue>, // TODO: need better name.
    $empty-avalue,
    avalue-value, avalue-value-setter;
    
  // Header parsing
  create
    <header-table>,
    *max-single-header-size*,
    *header-buffer-growth-amount*,
    // read-message-headers(stream) => header-table
    read-message-headers;
  create
    header-value;
  create 
    start-server,
    stop-server;
end;

// Additional interface for extending the server
define module http-server-extender
  create parse-header-value;
end;

define module internals
  use http-server;
  use http-server-extender;

  use functional-dylan, exclude: {string-to-integer};
  use locators, rename: {<http-server> => <http-server-url>,
                         <ftp-server> => <ftp-server-url>,
                         <file-server> => <file-server-url>};
  use dylan-extensions,
    import: {element-no-bounds-check,
             element-no-bounds-check-setter,
             element-range-check,
             element-range-error,
             // make-symbol,
             // case-insensitive-equal,
             // case-insensitive-string-hash
             };
  use threads;
  use format;
  use format-out;
  use standard-io;
  use streams;
  use sockets, rename: {start-server => start-socket-server };
  use date;
  use file-system;
  use operating-system;

end;

