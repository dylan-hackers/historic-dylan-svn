Module:    dylan-user
Author:    Gail Zacharias, Carl Gay
Copyright: Copyright (c) 2001-2004 Carl L. Gay.  All rights reserved.
           Original Code is Copyright (c) 2001 Functional Objects, Inc.  All rights reserved.
License:   Functional Objects Library Public License Version 1.0
Warranty:  Distributed WITHOUT WARRANTY OF ANY KIND

define library koala
  use base64;
  use collections,
    import: { table-extensions };
  use command-line-parser;
  use common-dylan,
    import: { dylan, common-extensions, threads, simple-random };
  use functional-dylan,
    import: { dylan-extensions };
  use http-common;
  use io,
    import: { format, standard-io, streams, streams-internals };
  use logging;
  use memory-manager;
  use network,
    import: { sockets };
  use regular-expressions;
  use strings;
  use system,
    import: { date, file-system, locators, operating-system };
  use uncommon-dylan;
  use uri;
  use xml-parser;
  use xml-rpc-common;

  export dsp;
  export koala-unit;
  export koala;
end library koala;


define module koala
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
    default-virtual-host,
    current-server,
    development-mode?;

  // Requests
  create
    // See also: the methods for requests in http-common
    <request>,
    current-request,             // Returns the active request of the thread.
    request-host,
    request-tail-url,
    request-absolute-url,
    request-query-values,        // get the keys/vals from the current GET or POST request
      get-query-value,           // Get a query value that was passed in a URL or a form
      do-query-values,           // Call f(key, val) for each query in the URL or form
      count-query-values,
      with-query-values,
    request-content-type,
    process-request-content,
    <page-context>,
    page-context;                // Returns a <page-context> if a page is being processed.
                                 //   i.e., essentially within the dynamic scope of
                                 //   respond-to-get/post/etc

  // Responders
  create
    <responder>,
      request-method-map,
    <tail-responder>,
      tail-responder-regex,
      tail-responder-action,
    add-responder,
    add-tail-responder,
    remove-responder,
    find-responder,
    invoke-responder,
    <skip-remaining-responders>,
    url-map-definer,
    add-urls;

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
    // See also: methods on <base-http-response> in common-dylan.
    current-response,            // Returns the active response of the thread.
    output,
    output-stream,
    add-cookie;

  // Sessions
  create
    <session>,
    get-session,
    ensure-session,
    clear-session;

  // Redirect
  create
    redirect-to,
    redirect-temporarily-to;

  // Logging
  create
    // These are wrappers for the defs by the same name in the logging library.
    log-trace,
    log-debug,
    log-info,
    log-warning,
    log-error;

  // Configuration
  create
    process-config-element,
    server-root,
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
    serve-static-file-or-cgi-script,
    document-location,
    document-root,
    static-file-responder,
    file-contents;

  // Errors
  create
    <koala-api-error>,
    <configuration-error>;

  create
    <http-file>,
    http-file-filename,
    http-file-content,
    http-file-mime-type;

end module koala;

// Additional interface for unit tests.
define module koala-unit
  create configure-from-string
end module koala-unit;

define module dsp
  use table-extensions,
    import: { table },
    rename: { table => make-table };
  use common-extensions,
    exclude: { false?, true? };
  use date;
  use dylan;
  use file-system;
  use format,
    rename: { format-to-string => sformat };
  use http-common;
  use koala,
    export: all;
  use locators,
    import: { <locator>,
              <file-locator>,
              <directory-locator>,
              locator-relative?,
              simplify-locator,
              merge-locators,
              locator-directory };
  use logging,
    rename: { log-trace => %log-trace,
              log-debug => %log-debug,
              log-info => %log-info,
              log-warning => %log-warning,
              log-error => %log-error },
    export: all;
  use operating-system;
  use standard-io;
  use streams;
  use strings;
  use threads;
  use uncommon-dylan;
  use uri;
  use simple-xml,
    import: { with-xml };

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

    named-method-definer,
    get-named-method,

    // Utils associated with the <dsp:table> tag
    current-row,                 // dsp:table
    current-row-number,          // dsp:table

    // Utils associated with the <dsp:loop> tag.
    loop-index,
    loop-value,

    // Form handling
    validate-form-field,
    add-field-error,
    get-field-errors,
    add-page-note,
    add-page-error,
    page-has-errors?,

    <paginator>,
    paginator-sequence,
    current-page-number,
    current-page-number-setter,
    previous-page-number,
    next-page-number,
    page-count,
    page-size,
    page-links,
    <page-link>,
    page-link-page-number,
    page-link-label;

end module dsp;

define module httpi                             // http internals
  use base64;
  use command-line-parser;
  use common-extensions,
    exclude: { format-to-string };
  use date;                    // from system lib
  use dsp;
  use dylan;
  use dylan-extensions,
    import: { element-no-bounds-check,
              element-no-bounds-check-setter,
              element-range-check,
              element-range-error,
              // make-symbol,
              // case-insensitive-equal,
              // case-insensitive-string-hash
              };
  use file-system;             // from system lib
  use format;
  use http-common;
  use koala;
  use koala-unit;
  use locators,
    rename: { <http-server> => <http-server-url>,
              <ftp-server> => <ftp-server-url>,
              <file-server> => <file-server-url>
            },
    exclude: { <url> };  // this comes from the uri library now.
  use logging,
    rename: { log-trace => %log-trace,
              log-debug => %log-debug,
              log-info => %log-info,
              log-warning => %log-warning,
              log-error => %log-error };
  use memory-manager;
  use operating-system;        // from system lib
  use regular-expressions;
  use simple-random;
  use sockets,
    rename: { start-server => start-socket-server };
  use standard-io;
  use streams;
  use streams-internals;
  use strings;
  use threads;               // from dylan lib
  use uncommon-dylan;
  use uri;
  use xml-parser,
    prefix: "xml$";
  use xml-rpc-common;
end module httpi;

