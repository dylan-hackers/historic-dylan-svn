Module: dylan-user

define library logging
  use common-dylan;
  use io,
    import: { format, streams };
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
  use streams;
  use threads;
  use uncommon-dylan,
    import: { iff, inc!, <singleton-object> };

  export
    // Log targets
    <log-target>,
    <null-log-target>,
    <stream-log-target>,
    <file-log-target>,
    <rolling-file-log-target>,
    log-level,
    log-level-setter,

    // Log levels
    <log-level>,
    <log-error>,
    <log-warning>,
    <log-info>,
    <log-debug>,
    <log-verbose>,
    <log-copious>,

    // Logging functions
    log-error,
    log-warning,
    log-info,
    log-debug,
    log-debug-if,
    log-verbose,
    log-copious,
    log,
    log-raw,

    // Misc
    date-to-stream,           // questionable
    as-common-logfile-date;

end module logging;
