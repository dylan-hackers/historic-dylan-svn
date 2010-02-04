Module: dylan-user
Synopsis: Code shared by HTTP client and server

define library http-common
  use base64;
  use common-dylan,
    import: { dylan,
              common-extensions,
              threads,
              simple-random };
  use functional-dylan,
    import: { dylan-extensions };
  use io,
    import: { format,
              standard-io,
              streams };
  use logging;
  use system,
    import: { date,
              file-system,
              locators,
              operating-system };
  use strings;
  use uncommon-dylan;
  use uri;
  export http-common;
end library http-common;

define module http-common
  use base64;
  use common-extensions,
    exclude: { format-to-string };
  use date;
  use dylan;
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
    import: { with-open-file,
              <file-does-not-exist-error>,
              <pathname> };
  use format;
  use locators,
    import: { <locator>,
              <file-locator>,
              <directory-locator>,
              locator-directory,
              simplify-locator,
              subdirectory-locator
              };
  use logging;
  use standard-io;
  use streams;
  use strings;
  use threads;
  use uncommon-dylan;
  use uri;

  export
    $http-version,
    $default-http-port,
    *http-common-log*,

    // Things that expire
    <expiring-mixin>,
    expired?,
    date-modified,
    date-modified-setter,

    // Thing with attributes
    <attributes-mixin>,
    has-attribute?,
    get-attribute,
    set-attribute,
    remove-attribute,

    quote-html,

    <chunking-input-stream>,
    content-length,
    note-bytes-received,

    // Request objects
    <base-http-request>,
    request-content,
    request-content-setter,
    request-method,
    request-method-setter,          // todo -- remove this export
    request-raw-url-string,
    request-raw-url-string-setter,  // todo -- remove this export
    request-url,
    request-url-setter,             // todo -- remove this export
    request-version,
    request-version-setter,         // todo -- remove this export

    // Response objects
    <base-http-response>,
    response-chunked?,
    response-chunked?-setter,
    response-code,
    response-code-setter,
    response-reason-phrase,
    response-reason-phrase-setter,
    response-request,

    // Errors and redirects
    <http-error>,                            // Any client or server error
    <http-protocol-condition>,               // Any client or server protocol condition

    <http-redirect-condition>,               // Superclass of all redirects
    <moved-permanently-redirect>,            // 301
    $moved-permanently-redirect,
    moved-permanently-redirect,
    <found-redirect>,                        // 302 (compare 307)
    $found-redirect,
    found-redirect,
    <see-other-redirect>,                    // 303
    $see-other-redirect,
    see-other-redirect,
    <not-modified-redirect>,                 // 304
    $not-modified-redirect,
    not-modified-redirect,
    <use-proxy-redirect>,                    // 305
    $use-proxy-redirect,
    use-proxy-redirect,
                                             // 306 unused
    <moved-temporarily-redirect>,            // 307 (compare 302)
    $moved-temporarily-redirect,
    moved-temporarily-redirect,

    <http-client-protocol-error>,            // Superclass of all client errors
    <bad-request-error>,                     // 400
    $bad-request-error,
    bad-request-error,
    <header-too-large-error>,                // 400
    $header-too-large-error,
    header-too-large-error,
    <bad-header-error>,                      // 400
    $bad-header-error,
    bad-header-error,
    <unauthorized-error>,                    // 401
    $unauthorized-error,
    unauthorized-error,
    <payment-required-error>,                // 402
    $payment-required-error,
    payment-required-error,
    <forbidden-error>,                       // 403
    $forbidden-error,
    forbidden-error,
    <resource-not-found-error>,              // 404
    $resource-not-found-error,
    resource-not-found-error,
    <method-not-allowed-error>,              // 405
    $method-not-allowed-error,
    method-not-allowed-error,
    <not-acceptable-error>,                  // 406
    $not-acceptable-error,
    not-acceptable-error,
    <proxy-authentication-required-error>,   // 407
    $proxy-authentication-required-error,
    proxy-authentication-required-error,
    <request-timeout-error>,                 // 408
    $request-timeout-error,
    request-timeout-error,
    <conflict-error>,                        // 409
    $conflict-error,
    conflict-error,
    <gone-error>,                            // 410
    $gone-error,
    gone-error,
    <content-length-required-error>,         // 411
    $content-length-required-error,
    content-length-required-error,
    <precondition-failed-error>,             // 412
    $precondition-failed-error,
    precondition-failed-error,
    <request-entity-too-large-error>,        // 413
    $request-entity-too-large-error,
    request-entity-too-large-error,
    <request-uri-too-long-error>,            // 414
    $request-uri-too-long-error,
    request-uri-too-long-error,
    <unsupported-media-type-error>,          // 415
    $unsupported-media-type-error,
    unsupported-media-type-error,
    <requested-range-not-satisfiable-error>, // 416
    $requested-range-not-satisfiable-error,
    requested-range-not-satisfiable-error,
    <expectation-failed-error>,              // 417
    $expectation-failed-error,
    expectation-failed-error,

    <http-server-protocol-error>,            // Superclass of all server errors
    <internal-server-error>,                 // 500
    $internal-server-error,
    internal-server-error,
    <not-implemented-error>,                 // 501
    $not-implemented-error,
    not-implemented-error,
    <bad-gateway-error>,                     // 502
    $bad-gateway-error,
    bad-gateway-error,
    <service-unavailable-error>,             // 503
    $service-unavailable-error,
    service-unavailable-error,
    <gateway-timeout-error>,                 // 504
    $gateway-timeout-error,
    gateway-timeout-error,
    <http-version-not-supported-error>,      // 505
    $http-version-not-supported-error,
    http-version-not-supported-error,
    <application-error>,                     // 599  (local extension)
    $application-error,
    application-error,

    condition-class-for-status-code,
    http-status-code,
    http-error-headers,
    http-error-message-no-code,   // get rid of this, use condition-to-string

    // Parsing
    token-end-position,
    validate-http-version,
    validate-http-status-code,
    parse-http-date,

    // Headers
    <header-table>,
    add-header,  // should probably be set-header, for symmetry with get-header.
    get-header,
    read-message-headers,
    raw-headers,
    parsed-headers,
    <avalue>,
    avalue-value,
    avalue-alist,
    chunked-transfer-encoding?,

    // lower level header APIs...
    read-header-line,
    read-http-line,
    parse-header-value,
    grow-header-buffer,

    // Cookies
    cookie-name,
    cookie-value,
    cookie-domain,
    cookie-path,
    cookie-max-age,
    cookie-comment,
    cookie-version,
    $default-cookie-version;   // get rid of this

end module http-common;

