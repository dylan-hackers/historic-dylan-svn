Module: dylan-user

/*
Logging
~~~~~~~

Basic Usage
===========

Define a primary logger that will 

  define constant $log = make(<logger>, name: "my-app");
  add-target($log, $stdout-log-target);
  log-info($log, "My-app starting with args %s", application-arguments());

Make another logger for debugging server requests::

  define constant $request-log = make(<logger>, name: "my-app.debug.request");

There are several things to notice about the above setup::

  * Loggers have no log targets by default.  The simplest way to add a target
    is to add a pre-existing target such as $stdout-log-target or 
    $stderr-log-target.

  * Different loggers are associated by name.  In this example the logger
    named "my-app" is a parent of the one named "my-app.debug.request"
    because the first dotted name component matches.

  * No targets were added to the my-app.debug.request logger.  Since all
    log messages sent to a child are also sent to its ancestors (but see
    logger-additive?-setter), anything logged to the my-app.debug.request
    logger will be logged to stdout via the my-app logger.

    So what's the benefit of having both loggers?  You can enabled/disable
    them separately at runtime.  Also, if for example you wanted to log
    debug messages to a separate file  you could add a target to the
    my-app.debug logger.

    (todo -- this implies morphing placeholders into loggers if targets are
    added.  Also, should we attempt to eliminate duplicates if the user
    specifies two loggers in an ancestor relationship that have the same
    log target?)

Log to a file::

  add-target(my-logger, make(<rolling-file-log-target>,
                             file: "/var/log/my-app.log",
                             max-file-size: 10000000))  // optional
                             
*/

define library logging
  use common-dylan;
  use big-integers;
  use generic-arithmetic;
  use io,
    import: { format, standard-io, streams };
  use system,
    import: { date, file-system, locators, threads };
  use uncommon-dylan;

  export
    logging,
    logging-impl;

end library logging;

define module logging
  create
    // Loggers
    // Maybe rename to <log> and log-*
    <logger>,
    log-level,
    log-level-setter,
    log-targets,
    logger-name,
    logger-additive?,
    logger-additive?-setter,
    logger-enabled?,
    logger-enabled?-setter,
    get-logger,
    get-root-logger,
    add-target,
    remove-target,

    // Levels
    <log-level>,
    <trace-level>, $trace-level,
    <debug-level>, $debug-level,
    <info-level>,  $info-level,
    <warn-level>,  $warn-level,
    <error-level>, $error-level,
    level-name,

    // Targets
    <log-target>,
    <null-log-target>,
    <stream-log-target>,
      target-stream,
    <file-log-target>,
      target-pathname,
      open-target-stream,
    <rolling-file-log-target>,
    $stdout-log-target,
    $stderr-log-target,

    // Functions
    log-trace,
    log-debug,
    log-debug-if,
    log-info,
    log-warning,
    log-error,

    // Formatters
    <log-formatter>,
    $default-log-formatter,

    // Errors
    <logging-error>,

    // For building your own logging classes
    <abstract-logger>,
    <placeholder-logger>,
    log-to-target,
    log-message,
    current-log-object,
    current-log-args,
    pattern-to-stream,
    write-message,

    // Misc
    date-to-stream,           // questionable
    as-common-logfile-date;

end module logging;

define module logging-impl
  use common-dylan,
    exclude: { format-to-string };
  use date;
  use file-system;
    //import: { <file-stream>, <pathname>, rename-file };
  use format;
  use generic-arithmetic,
    import: { <integer> => <double-integer>,
              + => plus,
              * => mul,
              / => div };
  use locators,
    import: { <locator>,
              <file-locator>,
              locator-name,
              merge-locators };
  use logging;
  use standard-io;
  use streams;
  use threads;
  use uncommon-dylan,
    import: { iff,
              inc!,
              <singleton-object>,
              <string-trie>, add-object, find-object };

  export
    // for test suite
    elapsed-milliseconds,
    reset-logging;

end module logging-impl;

