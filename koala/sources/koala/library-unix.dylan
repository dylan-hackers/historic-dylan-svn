Module:    dylan-user
Author:    Gail Zacharias, Carl Gay
Copyright: Copyright (c) 2001-2004 Carl L. Gay.  All rights reserved.
           Original Code is Copyright (c) 2001 Functional Objects, Inc.  All rights reserved.
License:   Functional Objects Library Public License Version 1.0
Warranty:  Distributed WITHOUT WARRANTY OF ANY KIND


define library koala
  use functional-dylan,
    import: { dylan-extensions };
  use common-dylan;
  use io,
    import: { format, standard-io, streams };
  use network,
    import: { sockets };
  use system,
    import: { date, file-system, locators, operating-system, threads };
  //use ssl-sockets;  // until integrated into FD?
  use xml-parser;
  use xml-rpc-common;
  use dylan-basics;                             // basic dylan utils

  export koala;
  export koala-extender;
  export dsp;
end library koala;


define module utilities
  use dylan;
  use common-extensions,
    exclude: { format-to-string, split };
  use dylan-extensions,
    import: { element-no-bounds-check,
              element-no-bounds-check-setter,
              element-range-check,
              element-range-error,
              // make-symbol,
              // case-insensitive-equal,
              // case-insensitive-string-hash
              };
  use file-system,
    import: { with-open-file, <file-does-not-exist-error> };
  use date;
  use streams;
  use locators;
  use standard-io;
  use file-system;
  use format;
  use threads;
  use dylan-basics,
    export: all;

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
    register-init-function,
    run-init-functions,
    
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
    trim,
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
    <log-level>,
    <log-target>, <null-log-target>, <stream-log-target>, <file-log-target>,
    <rolling-file-log-target>,
    <log-error>, <log-warning>, <log-info>, <log-debug>, <log-verbose>, <log-copious>,
    log-error, log-warning, log-info, log-debug, log-debug-if, log-verbose, log-copious,
    log, log-raw,
    log-level, log-level-setter,
    as-common-logfile-date, as-rfc-1123-date;
    
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

  // Basic server stuff
  create
    http-server,        // Get the active HTTP server object.
    ensure-server,      // Get (or create) the active HTTP server object.
    start-server,
    stop-server,
    register-url,
    register-alias-url,
    <request>,
    *request*,                   // Holds the active request, per thread.
    request-query-values,        // get the keys/vals from the current GET or POST request
    request-method,              // Returns #"get", #"post", etc
    responder-definer,

    // Form/query values.  (Is there a good name that covers both of these?)
    get-query-value,             // Get a query value that was passed in a URL or a form
    get-form-value,              // A synonym for get-query-value
    do-query-values,             // Call f(key, val) for each query in the URL or form
    do-form-values,              // A synonym for do-query-values
    count-query-values,
    count-form-values,
    application-error;

  // Virtual hosts
  create
    <virtual-host>, *virtual-host*,
    document-root, locator-below-document-root?;

  // Responses
  create
    <response>,
    output-stream,
    clear-output,
    set-content-type,
    add-header,
    add-cookie,
    get-request;

  // Cookies
  create
    cookie-name,
    cookie-value,
    cookie-domain,
    cookie-path,
    cookie-max-age,
    cookie-comment,
    cookie-version;

  // Sessions
  create
    <session>,
    get-session,
    //get-attribute,
    //set-attribute,
    //remove-attribute,
    set-session-max-age;

  // Logging
  create
    // These are wrappers for the defs by the same name in the utilities module.
    log-copious, log-verbose, log-debug, log-info, log-warning, log-error;

  // XML-RPC
  create
    register-xml-rpc-method;

  // Documents
  create
    document-location;

  // Errors
  create
    access-forbidden-error,
    <koala-api-error>;

  // Not sure if these should really be exported.
  create
    http-error-code,
    unsupported-request-method-error,
    resource-not-found-error,
    unimplemented-error,
    internal-server-error,
    request-url,
    register-auto-responder;

  // Debugging
  create
    print-object;

end module koala;

// Additional interface for extending the server
define module koala-extender
  create parse-header-value;
end;

define module httpi                             // http internals
  use dylan;
  use threads;               // from dylan lib
  use common-extensions, exclude: { format-to-string, split };
  use dylan-basics;
  use utilities,
    rename: { log-copious => %log-copious,
              log-verbose => %log-verbose,
              log-debug => %log-debug,
              log-info => %log-info,
              log-warning => %log-warning,
              log-error => %log-error };
  use koala;
  use koala-extender;

  use locators,
    rename: { <http-server> => <http-server-url>,
              <ftp-server> => <ftp-server-url>,
              <file-server> => <file-server-url> };
  use dylan-extensions,
    import: { element-no-bounds-check,
              element-no-bounds-check-setter,
              element-range-check,
              element-range-error,
              // make-symbol,
              // case-insensitive-equal,
              // case-insensitive-string-hash
              };
  use format;
  use standard-io;
  use streams;
  use sockets,
    rename: { start-server => start-socket-server };
  use date;                    // from system lib
  use file-system;             // from system lib
  use operating-system;        // from system lib
  //use ssl-sockets;
  use xml-parser,
    prefix: "xml$";
  use xml-rpc-common;
end module httpi;

define module dsp
  use dylan;
  use common-extensions,
    exclude: { split };
  use dylan-basics;
  use koala,
    export: all;
  use utilities,
    rename: { log-copious => %log-copious,
              log-verbose => %log-verbose,
              log-debug => %log-debug,
              log-info => %log-info,
              log-warning => %log-warning,
              log-error => %log-error },
    export: all;

  use locators,
    rename: {<http-server> => <http-server-url>,
             <ftp-server> => <ftp-server-url>,
             <file-server> => <file-server-url>};
  use format,
    rename: { format-to-string => sformat };
  use threads;
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
    url-to-page,
    respond-to-get,              // Implement this for your page to handle GET requests
    respond-to-post,             // Implement this for your page to handle POST requests
    respond-to-head,             // Implement this for your page to handle HEAD requests

    <dylan-server-page>,         // Subclass this using the "define page" macro
    page-definer,                // Defines a new page class
    process-template,            // Call this (or next-method()) from respond-to-get/post if
                                 //   you decide you want the DSP template to be processed.
    <taglib>,
    taglib-definer,
    tag-definer,            // Defines a new DSP tag function and registers it with a page
    register-tag,           // This can be used to register tag functions that weren't created by "define tag".
    map-tag-call-attributes,
    show-tag-call-attributes,
    get-tag-call-attribute,

    <page-context>,
    page-context,                // Returns a <page-context> if a page is being processed.
                                 //   i.e., essentially within the dynamic scope of respond-to-get/post/etc
    named-method-definer,
    get-named-method,

    // Utils associated with DSP tag definitions
    current-row,                 // dsp:table
    current-row-number,          // dsp:table

    note-form-error,             // for any error encountered while processing a web form
    note-form-message,           // for informative messages in response to processing a web form
    note-field-error;            // for errors related to processing a specific form field

/*
  // Persistence layer maps database records <-> web pages.
  export
    with-database-connection,
    <database-record>,
    <modifiable-record>,
    initialize-record,
    record-id,
    next-record-id,              // ---TODO: don't export this.
    record-definer,
    load-record,
    load-records,                // Load all records of a given record class, matching a query string.
    load-all-records,            // Load all records of a given record class
    save-record,
    class-prototype,
    initialize-database,         // for doing any initialization when the database is first initialized.
    query-db,
    query-integer,
    update-db,
    <edit-record-page>,
    get-edit-record,
    respond-to-get-edit-record,
    respond-to-post-edit-record,
    *record*,
    validate-record-field,       // define methods on this to validate record page form fields
    display-hidden-field,
    *default-origin-page*,       // Return to this page when a record is submitted, if no origin page
                                 // was specified in the link to the edit-record page.

    // Consider renaming to field-descriptor?  or column-descriptor?
    <slot-descriptor>,
    slot-getter,
    slot-setter,
    slot-column-number,
    slot-column-name,
    slot-type,
    slot-database-type,
    slot-init-keyword,
    slot-required?,
    db-type-to-slot-type,
    record-table-name,
*/
end module dsp;

