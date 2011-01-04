Module: logging-test-suite
Author: Carl L Gay
Copyright: Copyright (c) 2011 Carl L Gay
           See License.txt for details.

define constant fmt = format-to-string;

// No tempfile library, so this'll have to do.
//
define constant $temp-directory :: <directory-locator>
  = subdirectory-locator(temp-directory(),
                         format-date("logging-%Y%m%d%H%M%S", current-date()));

define function temp-locator
    (filename :: <string>) => (temp-locator :: <file-locator>)
  // locators are a freakin' nightmare...falling back to strings.
  as(<file-locator>,
     concatenate(as(<string>, $temp-directory), "/", filename))
end;

define function file-contents
    (pathname :: <pathname>)
 => (text :: <string>)
  with-open-file(stream = pathname)
    read-to-end(stream)
  end
end;

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

// Make the most common type of logger for testing.
//
define function make-test-logger
    (name :: <string>, #rest init-args)
 => (logger :: <logger>)
  apply(make, <logger>,
        name: name,
        targets: list(make(<string-log-target>)),
        formatter: $message-only-formatter,
        init-args)
end;

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

// elapsed-milliseconds uses double integers.  This test just tries to
// make sure that number-to-string (should be integer-to-string) doesn't
// blow up.
//
define test test-elapsed-milliseconds ()
  // $maximum-integer is the standard value, not from the generic-arithmetic module.
  let int :: <double-integer> = plus($maximum-integer, 1);
  check-no-errors("number-to-string(<double-integer>)",
                  number-to-string(int));
end;

define test test-process-id ()
  for (pattern in #("%{pid}", "%p"),
       i from 1)
    let target = make(<string-log-target>);
    let logger = make(<logger>,
                      name: format-to-string("test-process-id-%s", i),
                      targets: list(target),
                      formatter: make(<log-formatter>, pattern: pattern),
                      level: $trace-level);
    log-info(logger, "this is ignored");
    check-equal("log stream contains process id only",
                stream-contents(target.target-stream),
                format-to-string(current-process-id()));
  end;
end;
