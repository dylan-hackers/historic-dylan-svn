Module: dylan-user

define library logging
  use common-dylan;
  use io,
    import: { format, standard-io, streams };
  use system,
    import: { date, file-system, locators, threads };
  use uncommon-dylan;
  export logging;
end library logging;

define module logging
  use common-dylan,
    exclude: { format-to-string };
  use date;
  use file-system,
    import: { <file-stream>, rename-file };
  use format;
  use locators,
    import: { <locator>,
              <file-locator>,
              locator-name,
              merge-locators };
  use standard-io;
  use streams;
  use threads;
  use uncommon-dylan,
    import: { iff,
              inc!,
              <singleton-object>,
              <string-trie>, add-object, find-object };

  export
    // Loggers
    <logger>,
    log-level,
    log-level-setter,
    logger-name,
    get-logger,
    get-root-logger,

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
    <file-log-target>,
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
    format-to-stream,

    // Errors
    <log-error>,

    // Mainly for extending the logging library
    log-to-target,
    log,
    current-log-object,
    current-log-args,

    // Misc
    date-to-stream,           // questionable
    as-common-logfile-date;

end module logging;
