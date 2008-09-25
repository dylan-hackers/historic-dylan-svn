Module: logging-test-suite

define constant fmt = format-to-string;

// This class serves two testing purposes: it's an easy way to get the
// results of logging to a stream and it tests the ability to create
// new types of log targets from outside the logging library.
//
define class <string-log-target> (<stream-log-target>)
end;

define method make
    (class == <string-log-target>, #rest args, #key stream)
 => (target)
  apply(next-method, class,
        stream: stream | make(<string-stream>, direction: #"output"),
        args)
end;

define constant $message-only-formatter
  = make(<log-formatter>, pattern: "%{message}");

define constant $log-levels
  = list($trace-level, $debug-level, $info-level, $warn-level, $error-level);

define constant $log-functions
  = list(log-trace, log-debug, log-info, log-warning, log-error);

// given = error    pos = 4
// logger = trace   idx = 0           expected = xxx\n
define function test-log-level
    (logger-level :: <log-level>)
  reset-logging();
  let logger-priority = position($log-levels, logger-level);
  for (log-fn in $log-functions,
       current-level in $log-levels,
       current-priority from 0)
    let target = make(<string-log-target>);
    let logger = make(<logger>,
                      name: fmt("logger.%d", current-priority),
                      targets: list(target),
                      level: logger-level,
                      formatter: $message-only-formatter);
    log-fn(logger, "xxx");
    let expected = iff(current-priority >= logger-priority, "xxx\n", "");
    let actual = stream-contents(target.target-stream);
    check-equal(fmt("Log output (%s) does not match expected (%s). Given level %s, "
                    "logger level %s", actual, expected, logger-level, current-level),
                expected, actual);
  end;
end function test-log-level;

