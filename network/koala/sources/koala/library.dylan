Module:    dylan-user
Author:    Gail Zacharias, Carl Gay
Copyright: Copyright (c) 2001-2004 Carl L. Gay.  All rights reserved.
           Original Code is Copyright (c) 2001 Functional Objects, Inc.  All rights reserved.
License:   Functional Objects Library Public License Version 1.0
Warranty:  Distributed WITHOUT WARRANTY OF ANY KIND

define library koala
  use functional-dylan,
    import: { dylan-extensions };
  use common-dylan,
    import: { dylan, common-extensions, threads, simple-random };
  use io,
    import: { format, standard-io, streams };
  use network,
    import: { sockets };
  use system,
    import: { date, file-system, locators, operating-system };
  //use ssl-sockets;  // until integrated into FD?
  use xml-parser;
  use xml-rpc-common;
  use dylan-basics;                             // basic dylan utils
  use base64;
  use memory-manager;
  use command-line-parser;
  use uri;
  use regular-expressions;

  export koala;
  export koala-extender;
  export koala-unit;
  export dsp;
end library koala;


define module utilities
  use dylan;
  use common-extensions,
    exclude: { format-to-string };
  use dylan-extensions,
    import: { element-no-bounds-check,
              element-no-bounds-check-setter,
              element-range-check,
              element-range-error,
              // make-symbol,
              // case-insensitive-equal,
              // case-insensitive-string-hash
              <format-string-condition>
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
    parent-directory,
    date-to-stream,
    kludge-read-into!,   // work around bug in read-into! in FD 2.0
    quote-html,          // Change < to &lt; etc

    <string-trie>, 
    find-object, 
    add-object, 
    remove-object,
    trie-children,
    trie-object,
    <trie-error>,

    <expiring-mixin>,
    expired?,
    date-modified,
    date-modified-setter,

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
    as-common-logfile-date;
end module utilities;
    

define module koala
  //needed for last-modified stuff
  create
    get-header,
    request-method-setter,
    not-modified;

  // Server startup/shutdown
  create
    <http-server>,
    configure-server,
    start-server,
    stop-server,
    koala-main,
    *argument-list-parser*;

  // Servers
  create
    current-server,
    development-mode?;

  // Requests
  create
    <request>,
    current-request,             // Returns the active request of the thread.
    request-method,              // Returns #"get", #"post", etc
    request-host,
    request-url,
    request-tail-url,
    request-query-values,        // get the keys/vals from the current GET or POST request
      get-query-value,           // Get a query value that was passed in a URL or a form
      do-query-values,           // Call f(key, val) for each query in the URL or form
      count-query-values,
    request-content,
    request-content-type,
    request-content-setter,
    process-request-content;

  // Responders
  create
    <responder>,
    responder-definer,
    responder-map,
    add-responder,
    remove-responder,
    find-responder,
    invoke-responder,
    url-map-definer;

  // Virtual hosts
  create
    <virtual-host>,
    virtual-host,                // Return virtual host of current request.
    document-root,
    dsp-root,
    vhost-name,
    locator-below-document-root?,
    locator-below-dsp-root?,
    locator-below-root?;

  // Responses
  create
    <response>,
    current-response,            // Returns the active response of the thread.
    output,
    output-stream,
    clear-output,
    set-content-type,
    add-header,
    add-cookie,
    get-request,
    response-code,
    response-code-setter,
    response-message,
    response-message-setter,
    response-headers;

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
    ensure-session,
    clear-session;
    //get-attribute,
    //set-attribute,
    //remove-attribute,

  // Logging
  create
    // These are wrappers for the defs by the same name in the utilities module.
    log-copious,
    log-verbose,
    log-debug,
    log-info,
    log-warning,
    log-error;

  // Configuration
  create
    process-config-element,
    get-attr;

  // XML RPC
  create
    xml-rpc-server-definer,
    <xml-rpc-server>,
    error-fault-code,
    error-fault-code-setter,
    debugging-enabled?,
    debugging-enabled?-setter,
    register-xml-rpc-method,
    $default-xml-rpc-url;

  // Documents
  create
    maybe-serve-static-file,
    document-location;

  // Redirection
  create
    redirect-to,
    redirect-temporarily-to,
    moved-permanently-redirect,
    moved-temporarily-redirect,
    see-other-redirect,
    unauthorized-error;

  // Errors
  create
    <koala-api-error>,
    <configuration-error>,
    http-error-code,
    http-error-headers,
    access-forbidden-error,
    application-error,
    unsupported-request-method-error,
    resource-not-found-error,
    unimplemented-error,
    internal-server-error,
    bad-request;

  // Files
  create
    static-file-responder;

  create
    <avalue>,
    avalue-value,
    avalue-alist;

  create
    <http-file>,
    http-file-filename,
    http-file-content,
    http-file-mime-type;

  // Headers
  // Do these really need to be exported?
  create
    <header-table>,
    *max-single-header-size*,
    *header-buffer-growth-amount*,
    // read-message-headers(stream) => header-table
    read-message-headers,
    header-value;

end module koala;

// Additional interface for extending the server
define module koala-extender
  create parse-header-value;
end;

// Additional interface for unit tests.
define module koala-unit
  create configure-from-string
end module koala-unit;

define module dsp
  use dylan;
  use common-extensions;
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
    import: { <locator>,
              <file-locator>,
              <directory-locator>,
              locator-relative?,
              simplify-locator,
              merge-locators,
              locator-directory };
  use uri;
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
    <page>,
    <static-page>,
    respond-to,                  // Implement this for your page to handle a request
    respond-to-get,              // Convenience.
    respond-to-post,             // Convenience.

    page-source,
    page-source-setter,
    page-template,
    page-template-setter,

    <dylan-server-page>,
    process-template,
    process-page,
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
    note-form-message;           // for informative messages in response to processing a web form

end module dsp;

define module httpi                             // http internals
  use dylan;
  use threads;               // from dylan lib
  use common-extensions,
    exclude: { format-to-string };
  use dylan-basics;
  use simple-random;
  use utilities,
    rename: { log-copious => %log-copious,
              log-verbose => %log-verbose,
              log-debug => %log-debug,
              log-info => %log-info,
              log-warning => %log-warning,
              log-error => %log-error };
  use koala;
  use koala-extender;
  use koala-unit;
  use memory-manager;
  use locators,
    rename: { <http-server> => <http-server-url>,
              <ftp-server> => <ftp-server-url>,
              <file-server> => <file-server-url>
            },
    exclude: { <url> };  // this comes from the uri library now.
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
  use base64;
  use command-line-parser;
  use uri;
  use regular-expressions;
  use dsp;
end module httpi;

