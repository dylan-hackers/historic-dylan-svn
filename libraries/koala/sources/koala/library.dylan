Module:    dylan-user
Author:    Gail Zacharias, Carl Gay
Copyright: Copyright (c) 2001 Carl L. Gay.  All rights reserved.
           Original Code is Copyright (c) 2001 Functional Objects, Inc.  All rights reserved.
License:   Functional Objects Library Public License Version 1.0
Warranty:  Distributed WITHOUT WARRANTY OF ANY KIND


define library koala
  use functional-dylan;
  use io;
  use network;
  use system;
  //use ssl-sockets;  // until integrated into FD?

  export http-server;
  export http-server-extender;
end library koala;


define module http-server

  // Headers
  // Do these really need to be exported?
  create
    <header-table>,
    *max-single-header-size*,
    *header-buffer-growth-amount*,
    // read-message-headers(stream) => header-table
    read-message-headers,
    header-value;

  // Server proper
  create
    http-server,        // Get the active HTTP server object.
    ensure-server,      // Get (or create) the active HTTP server object.
    start-server,
    stop-server,
    register-uri,
    register-alias-uri,
    <request>,
    request-query-values,        // get the keys/vals from the current GET or POST request
    request-method,              // Returns #"get", #"post", etc
    responder-definer;

  // Responses
  create
    <response>,
    output-stream,
    add-header,
    add-cookie;

  // Sessions
  create
    <session>,
    get-session,
    get-attribute,
    set-attribute,
    remove-attribute,
    set-session-max-age;

  // Pages
  create
    <page>,                      // Subclass this using the "define page" macro
    <static-page>,
    register-page,               // Register a page for a given URI
    respond-to-get,              // Implement this for your page to handle GET requests
    respond-to-post,             // Implement this for your page to handle POST requests
    respond-to-head,             // Implement this for your page to handle HEAD requests

    // Form/query values.  (Is there a good name that covers both of these?)
    get-query-value,             // Get a query value that was passed in a URI or a form
    get-form-value,              // A synonym for get-query-value
    do-query-values,             // Call f(key, val) for each query in the URI or form
    do-form-values,              // A synonym for do-query-values
    count-query-values,
    count-form-values,
    document-location;

  // Dylan Server Pages
  create
    <dylan-server-page>,         // Subclass this using the "define page" macro
    page-definer,                // Defines a new page class
    <taglib>,
    register-taglib,
    tag-definer,                 // Defines a new DSP tag function and registers it with a page
    register-tag,                // This can be used to register tag functions that weren't
                                 //   created by "define tag".
    <page-context>,
    page-context,                // Returns a <page-context> if a page is being processed.
                                 //   i.e., essentially within the dynamic scope of respond-to-get/post/etc
    register-label,
    get-label,
    quote-html;                  // Change < to &lt; etc

  // Logging
  create
    log-debug,
    log-error,
    log-warning,
    log-info;
  // Debugging
  create
    print-object;
end;

// Additional interface for extending the server
define module http-server-extender
  create parse-header-value;
end;

define module internals
  use http-server;
  use http-server-extender;

  use functional-dylan;
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
  use sockets, rename: { start-server => start-socket-server };
  use date;
  use file-system;
  use operating-system;
  //use ssl-sockets;
end;

