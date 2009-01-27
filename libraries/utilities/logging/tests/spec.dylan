Module: logging-test-suite

// Defines constant logging-test-suite
//
define library-spec logging ()
  module logging;
  test test-elapsed-milliseconds;
  test test-process-id;
end library-spec logging;

// Defines suite logging-module-test-suite.
//
define module-spec logging
    (setup-function: curry(ensure-directories-exist, $temp-directory))
  class <abstract-logger> (<object>);
  class <file-log-target> (<log-target>);
  class <log-formatter> (<object>);
  class <log-target> (<closable-object>);
  class <logger> (<abstract-logger>);
  instantiable class <logging-error> (<error>, <format-string-condition>);
  instantiable class <null-log-target> (<log-target>);
  class <placeholder-logger> (<abstract-logger>);
  class <rolling-file-log-target> (<file-log-target>);
  class <stream-log-target> (<log-target>);

  class <log-level> (<singleton-object>);
  instantiable class <debug-level> (<trace-level>);
  instantiable class <error-level> (<warn-level>);
  instantiable class <info-level> (<debug-level>);
  instantiable class <trace-level> (<log-level>);
  instantiable class <warn-level> (<info-level>);

  constant $debug-level :: <object>;
  constant $error-level :: <object>;
  constant $info-level :: <object>;
  constant $trace-level :: <object>;
  constant $warn-level :: <object>;

  constant log-debug :: <object>;
  constant log-error :: <object>;
  constant log-info :: <object>;
  constant log-trace :: <object>;
  constant log-warning :: <object>;

  constant $stderr-log-target :: <object>;
  constant $stdout-log-target :: <object>;

  function add-target (<logger>, <log-target>) => ();
  function as-common-logfile-date (<date>) => (<string>);
  function current-log-args () => (<sequence>);
  function current-log-object () => (<object>);
  function date-to-stream (<stream>, <date>) => ();
  function get-logger (<string>) => (<abstract-logger>);
  function get-root-logger () => (<logger>);
  function level-name (<log-level>) => (<string>);
  function log-debug-if (<object>, <abstract-logger>, <string>) => ();
  function log-level-setter (<log-level>, <logger>) => (<log-level>);
  function log-level (<logger>) => (<log-level>);
  function log-message (<log-level>, <logger>, <object>) => ();
  function log-to-target (<log-target>, <log-formatter>, <object>) => ();
  function logger-additive?-setter (<boolean>, <abstract-logger>) => (<boolean>);
  function logger-additive? (<abstract-logger>) => (<boolean>);
  function logger-enabled?-setter (<boolean>, <abstract-logger>) => (<boolean>);
  function logger-enabled? (<abstract-logger>) => (<boolean>);
  function logger-name (<abstract-logger>) => (<string>);
  function pattern-to-stream (<log-formatter>, <stream>) => ();
  function remove-target (<logger>, <log-target>) => ();
  function write-message (<log-target>, <object>) => ();
end module-spec logging;

define logging class-test <abstract-logger> ()
end class-test <abstract-logger>;

define logging class-test <debug-level> ()
end class-test <debug-level>;

define logging class-test <error-level> ()
end class-test <error-level>;

define logging class-test <info-level> ()
end class-test <info-level>;

define logging class-test <trace-level> ()
end class-test <trace-level>;

define logging class-test <warn-level> ()
end class-test <warn-level>;

define logging class-test <log-formatter> ()
end class-test <log-formatter>;

define logging class-test <log-level> ()
end class-test <log-level>;

define logging class-test <placeholder-logger> ()
end class-test <placeholder-logger>;

define logging class-test <logger> ()
  check-no-errors("make a logger with a <string> formatter",
                  make(<logger>,
                       name: "<logger>-test",
                       formatter: "foo"));
end class-test <logger>;

define logging class-test <logging-error> ()
end class-test <logging-error>;

define logging class-test <log-target> ()
end class-test <log-target>;

define logging class-test <null-log-target> ()
end class-test <null-log-target>;

define logging class-test <stream-log-target> ()
end class-test <stream-log-target>;

define logging constant-test $stderr-log-target ()
end constant-test $stderr-log-target;

define logging constant-test $stdout-log-target ()
end constant-test $stdout-log-target;

define logging class-test <file-log-target> ()
  let locator = temp-locator("file-log-target-test.log");
  let target = make(<file-log-target>, pathname: locator);
  let log = make(<logger>,
                 name: "file-log-target-test",
                 targets: list(target),
                 formatter: $message-only-formatter);
  log-info(log, "test");
  close(target);
  with-open-file (stream = locator, direction: #"input")
    check-equal("file-log-target has expected contents",
                read-to-end(stream), "test\n");
  end;
end class-test <file-log-target>;

define logging class-test <rolling-file-log-target> ()
  // Make sure the file rolls when it reaches max size
  let locator = temp-locator("rolling-log-file.log");
  if (file-exists?(locator))
    delete-file(locator)
  end;
  let target = make(<rolling-file-log-target>,
                    pathname: locator,
                    max-size: 10);
  let logger = make(<logger>,
                    name: "rolling-file-test",
                    targets: list(target),
                    formatter: $message-only-formatter);
  // I figure this could log 8 or 9 characters, including CR and/or LF.
  log-info(logger, "1234567");
  close(target);  // can't read file on Windows unless it's closed
  check-equal("log doesn't roll when below max size",
              file-contents(locator),
              "1234567\n");
  open-target-stream(target);
  log-info(logger, "890");
  close(target);  // can't read file on Windows unless it's closed
  check-equal("log rolls when max size exceeded",
              file-contents(locator),
              "");
end class-test <rolling-file-log-target>;

define logging constant-test $debug-level ()
end constant-test $debug-level;

define logging constant-test $error-level ()
end constant-test $error-level;

define logging constant-test $info-level ()
end constant-test $info-level;

define logging constant-test $trace-level ()
end constant-test $trace-level;

define logging constant-test $warn-level ()
end constant-test $warn-level;

define logging constant-test log-debug ()
    test-log-level($debug-level);
end constant-test log-debug;

define logging constant-test log-error ()
    test-log-level($error-level);
end constant-test log-error;

define logging constant-test log-info ()
    test-log-level($info-level);
end constant-test log-info;

define logging constant-test log-trace ()
    test-log-level($trace-level);
end constant-test log-trace;

define logging constant-test log-warning ()
    test-log-level($warn-level);
end constant-test log-warning;

define logging function-test add-target ()
end function-test add-target;

define logging function-test as-common-logfile-date ()
end function-test as-common-logfile-date;

define logging function-test current-log-args ()
end function-test current-log-args;

define logging function-test current-log-object ()
end function-test current-log-object;

define logging function-test date-to-stream ()
end function-test date-to-stream;

define logging function-test get-logger ()
end function-test get-logger;

define logging function-test get-root-logger ()
end function-test get-root-logger;

define logging function-test level-name ()
end function-test level-name;

define logging function-test log-debug-if ()
end function-test log-debug-if;

define logging function-test log-level ()
end function-test log-level;

define logging function-test log-level-setter ()
end function-test log-level-setter;

define logging function-test log-message ()
end function-test log-message;

define logging function-test log-to-target ()
end function-test log-to-target;

define logging function-test logger-additive? ()
  // Make sure non-additive logger DOESN'T pass it on to parent.
  let logger1 = make-test-logger("aaa");
  let logger2 = make-test-logger("aaa.bbb", additive: #f);
  log-error(logger2, "xxx");
  check-equal("non-additivity respected for target1",
              stream-contents(logger1.log-targets[0].target-stream),
              "");
  check-equal("non-additivity respected for target2",
              stream-contents(logger2.log-targets[0].target-stream),
              "xxx\n");

  // Make sure additive logger DOES pass it on to parent.
  let logger1 = make-test-logger("xxx");
  let logger2 = make-test-logger("xxx.yyy", additive: #t);
  log-error(logger2, "xxx");
  check-equal("additivity respected for target1",
              stream-contents(logger1.log-targets[0].target-stream),
              "xxx\n");
  check-equal("additivity respected for target2",
              stream-contents(logger2.log-targets[0].target-stream),
              "xxx\n");
end function-test logger-additive?;

define logging function-test logger-additive?-setter ()
end function-test logger-additive?-setter;

define logging function-test logger-enabled? ()
end function-test logger-enabled?;

define logging function-test logger-enabled?-setter ()
  // Make sure disabled logger doesn't do output
  let logger = make-test-logger("logger-enabled-test");
  logger-enabled?(logger) := #f;
  log-info(logger, "xxx");
  check-equal("disabled logger does no output",
              stream-contents(logger.log-targets[0].target-stream),
              "");

  // Make sure disabled logger still respects additivity.
  let parent = make-test-logger("parent");
  let child = make-test-logger("parent.child");
  logger-enabled?(child) := #f;
  log-info(child, "xxx");
  check-equal("additivity respected for disabled logger",
              stream-contents(parent.log-targets[0].target-stream),
              "xxx\n");
end function-test logger-enabled?-setter;

define logging function-test logger-name ()
end function-test logger-name;

define logging function-test pattern-to-stream ()
end function-test pattern-to-stream;

define logging function-test remove-target ()
end function-test remove-target;

define logging function-test write-message ()
end function-test write-message;
