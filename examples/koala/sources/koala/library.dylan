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
  use xml-parser;
  use xml-rpc-common;
  use dylan-basics;                             // basic dylan utils

  export koala;
  export koala-extender;
  export dsp;
end library koala;


define module utilities
  use functional-dylan;
  use dylan-extensions,
    import: { element-no-bounds-check,
              element-no-bounds-check-setter,
              element-range-check,
              element-range-error,
              // make-symbol,
              // case-insensitive-equal,
              // case-insensitive-string-hash
              };
  use file-system, import: { with-open-file, <file-does-not-exist-error> };
  use date;
  use streams;
  use locators;
  use standard-io;
  use file-system;
  use format;
  use threads;
  use dylan-basics, export: all;

  export
    // General one-off utilities
    <sealed-constructor>,
    wrapping-inc!,
    file-contents,
    pset,                // multiple-value-setq
    ignore-errors,
    path-element-equal?,
    parent-directory,
    date-to-stream,
    kludge-read-into!,   // work around bug in read-into! in FD 2.0
    quote-html,          // Change < to &lt; etc
    
    <expiring-mixin>,
    expired?,
    mod-time,
    mod-time-setter,

    // Resource pools
    allocate-resource,
    deallocate-resource,
    new-resource,
    reinitialize-resource,
    resource-deallocated,
    resource-size,
    with-resource,
    test-resource,

    // Attributes
    <attributes-mixin>,
    get-attribute,
    set-attribute,
    remove-attribute,

    // Strings
    $cr,
    $lf,
    char-position-if,
    char-position,
    char-position-from-end,
    whitespace?,
    whitespace-position,
    skip-whitespace,
    trim-whitespace,
    looking-at?,
    key-match,
    string-match,
    string-position,
    string-equal?,
    digit-weight,
    token-end-position,

    // Non-copying substring
    <substring>,
    substring,
    substring-base,
    substring-start,
    string-extent,
    string->integer,

    // Logging
    with-log-output-to,
    <log-level>,
    <log-error>, log-error,
    <log-warning>, log-warning,
    <log-info>, log-info,
    <log-debug>, log-debug, log-debug-if,
    log-message,
    log-date,
    add-log-level, remove-log-level, clear-log-levels;
    
end module utilities;
    

define module koala

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
    register-url,
    register-alias-url,
    <request>,
    request-query-values,        // get the keys/vals from the current GET or POST request
    request-method,              // Returns #"get", #"post", etc
    responder-definer,

    // Form/query values.  (Is there a good name that covers both of these?)
    get-query-value,             // Get a query value that was passed in a URL or a form
    get-form-value,              // A synonym for get-query-value
    do-query-values,             // Call f(key, val) for each query in the URL or form
    do-form-values,              // A synonym for do-query-values
    count-query-values,
    count-form-values;

  // Responses
  create
    <response>,
    output-stream,
    add-header,
    add-cookie,
    get-request;

  // Sessions
  create
    <session>,
    get-session,
    //get-attribute,
    //set-attribute,
    //remove-attribute,
    set-session-max-age;

  // XML-RPC
  create
    register-xml-rpc-method;

  // Documents
  create
    document-location;

  // Logging
/*
  create
    log-debug,
    log-debug-if,
    log-error,
    log-warning,
    log-info,
    log-message;
*/

  // Not sure if these should really be exported.
  create
    http-error-code,
    unsupported-request-method-error,
    resource-not-found-error,
    unimplemented-error,
    internal-server-error,
    request-url,
    *auto-register-map*;

  // Debugging
  create
    print-object;

end module koala;

// Additional interface for extending the server
define module koala-extender
  create parse-header-value;
end;

define module httpi                             // http internals
  use utilities;
  use koala;
  use koala-extender;

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
  use xml-parser,
    prefix: "xml$";
  use xml-rpc-common;
end module httpi;

define module dsp
  use koala, export: all;
  use utilities, export: all;

  use functional-dylan;
  use locators, rename: {<http-server> => <http-server-url>,
                         <ftp-server> => <ftp-server-url>,
                         <file-server> => <file-server-url>};
  use format;
  use threads;
  use format-out;
  use standard-io;
  use streams;
  //use sockets, rename: { start-server => start-socket-server };
  use date;
  use file-system;
  use operating-system;
  //use ssl-sockets;

  export
    <page>,                      // Subclass this using the "define page" macro
    <static-page>,
    register-page,               // Register a page for a given URL
    respond-to-get,              // Implement this for your page to handle GET requests
    respond-to-post,             // Implement this for your page to handle POST requests
    respond-to-head,             // Implement this for your page to handle HEAD requests

    <dylan-server-page>,         // Subclass this using the "define page" macro
    page-definer,                // Defines a new page class
    <taglib>,
    taglib-definer,
    tag-definer,            // Defines a new DSP tag function and registers it with a page
    register-tag,           // This can be used to register tag functions that weren't created by "define tag".
    tag-call-arguments,
    map-tag-call-arguments,
    show-tag-call-arguments,

    <page-context>,
    page-context,                // Returns a <page-context> if a page is being processed.
                                 //   i.e., essentially within the dynamic scope of respond-to-get/post/etc
    named-method-definer,
    get-named-method,

    // Utils associated with DSP tag definitions
    current-row,                 // dsp:table
    current-row-number;          // dsp:table

end module dsp;

