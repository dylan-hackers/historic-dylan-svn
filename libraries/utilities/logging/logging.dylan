Module:    logging-impl
Author:    Carl Gay
Synopsis:  Simple logging mechanism.  Some ideas taken from log4j.


/* 

todo -- implement keep-versions in <rolling-file-log-target>

todo -- implement compress-on-close? in <rolling-file-log-target>

todo -- current-process-id is a stub

todo -- configuration parser

todo -- more documentation

todo -- more tests

todo -- Handle errors gracefully.  e.g., if the disk fills up it may be
        better to do nothing than to err.  Catch errors in user code when
        logging a message and log "*** error generating log message ***",
        for example.  If logging to stream/file fails, log to stderr as
        a fallback.  (e.g., someone forks and closes all fds)

todo -- <file-log-target> should accept a string for the filename to
        avoid making people import locators.  God I hate the locators
        library.

todo -- <rolling-file-log-target>: Should roll the file when either a
        max size or a max time is reached, whichever comes first.
        Should make it possible for users to override roll-log-file?
        and rolled-log-file-name methods if they want to roll their
        own.  Should also have option to compress on roll.  Should also
        be able to specify that it roll "daily at midnight" etc.

todo -- Add a way to extend the set of format directives from outside
        the library.  Get rid of code duplication in formatter parsing.

??? -- Is there a reasonable use case where you might not want \n at the
       end of each log entry?  Rather than hard-coding the \n one could
       specify it in the formatter's control string.  The worry is that
       everyone would forget to add it every time they write a new formatter.

idea -- There's often a tension between the level of logging you want
        to retain permanently and the level of logging you need for
        debugging.  Could support writing different log levels to
        additional targets.  Then one could log debug messages to a
        separate file and set them to roll every 5 minutes, with a
        small number of revisions, and you have essentially a circular
        buffer of recent debug info.  Log to RAMdisk...even better, to
        avoid disk contention.  :-)

idea -- It is useful for general purpose libraries (e.g., an XML parser)
        to do logging.  You normally want this logging disabled.  A calling
        library will probably want to turn on the XML parser's logging for
        specific threads, for debugging purposes.  The XML parser can use
        an exported thread variable to hold its debug logger and callers can
        rebind that to the logger they want.  (Not really an issue for this
        logging library to address...more of a suggestion for something to
        add to future documentation.)  Just enabling the XML parser's logger
        won't always be what users want because it will enable logging in
        all threads.

todo -- Look at concurrency issues.  For example, is it possible for log
        messages to be written with out-of-order timestamps when multiple
        threads log to the same file via different log targets.  Can either
        document that and say "don't do that" or fix it.

*/


///////////////////////////////////////////////////////////
//// Loggers
////

define variable $root-logger :: false-or(<logger>) = #f;

define abstract class <abstract-logger> (<object>)
  // A dotted path name.  All parent loggers in the path must already exist.
  constant slot logger-name :: <string>,
    required-init-keyword: name:;

  slot logger-parent :: false-or(<abstract-logger>),
    init-value: #f,
    init-keyword: parent:;

  constant slot logger-children :: <string-table>,
    init-keyword: children:,
    init-function: curry(make, <string-table>);

  // If this is #t then log messages sent to this logger will be passed up
  // the hierarchy to parent loggers as well, until it reaches a logger
  // whos additivity is #f.  Terminology stolen from log4j.
  //
  slot logger-additive? :: <boolean>,
    init-keyword: additive:,
    init-value: #t;

  // If disabled, no messages will be logged to this logger's targets.
  // The value of logger-additive? will still be respected.  In other
  // words, logging to a disabled logger will still log to ancestor
  // loggers if they are themselves enabled.
  //
  slot logger-enabled? :: <boolean>,
    init-keyword: enabled:,
    init-value: #t;

end class <abstract-logger>;

define method initialize
    (logger :: <abstract-logger>, #key name :: <string>)
  next-method();
  if ($root-logger)
    add-logger($root-logger, logger, as(<list>, split(name, '.')), name);
  end;
end method initialize;

define method local-name
    (logger :: <abstract-logger>)
 => (local-name :: <string>)
  last(split(logger.logger-name, '.'))
end;

// Instances of this class are used as placeholders in the logger hierarchy when
// a logger is created before its parents are created.  i.e., if the first logger
// created is named "x.y.z" then both x and x.y will be <placeholder-logger>s.
// (If x.y is later created as a real logger then the placeholder will be replaced.)
//
define open class <placeholder-logger> (<abstract-logger>)
end;

define open class <logger> (<abstract-logger>)
  slot log-level :: <log-level>,
    init-keyword: level:,
    init-value: $trace-level;

  constant slot log-targets :: <stretchy-vector>,
    init-keyword: targets:,
    init-function: curry(make, <stretchy-vector>);

  slot log-formatter :: <log-formatter>,
    init-keyword: formatter:,
    init-value: $default-log-formatter;

end class <logger>;

define method make
    (class :: subclass(<logger>),
     #rest args,
     #key formatter, targets :: false-or(<sequence>))
 => (logger)
  // Formatter may be specified as a string for convenience.
  if (instance?(formatter, <string>))
    formatter := make(<log-formatter>, pattern: formatter);
  end;
  // Make sure targets is a <stretchy-vector>.  It's convenient for users
  // to be able to pass list(make(<target> ...)) though.
  let targets = as(<stretchy-vector>, targets | #[]);
  let logger
    = apply(next-method, class,
            targets: targets,
            formatter: formatter | $default-log-formatter,
            args);
  logger
end method make;

define method print-object
    (logger :: <logger>, stream :: <stream>)
 => ()
  if (*print-escape?*)
    next-method();
  else
    format(stream, "%s (%sadditive, level: %s, targets: %s)",
           logger.logger-name,
           iff(logger.logger-additive?, "", "non-"),
           logger.log-level.level-name,
           iff(empty?(logger.log-targets),
               "None",
               join(logger.log-targets, ", ",
                    key: curry(format-to-string, "%s"))));
  end;
end method print-object;

define method add-target
    (logger :: <logger>, target :: <log-target>)
  add-new!(logger.log-targets, target)
end;

define method remove-target
    (logger :: <logger>, target :: <log-target>)
  remove!(logger.log-targets, target);
end;

define method remove-all-targets
    (logger :: <logger>)
  for (target in logger.log-targets)
    remove-target(logger, target)
  end;
end;

define open class <logging-error> (<error>, <format-string-condition>)
end;

define function logging-error
    (control-string, #rest args)
  signal(make(<logging-error>,
              format-string: control-string,
              format-arguments: args))
end;

define method get-root-logger
    () => (logger :: <logger>)
  $root-logger
end;

define method get-logger
    (name :: <string>) => (logger :: false-or(<abstract-logger>))
  %get-logger($root-logger, as(<list>, split(name, '.')), name)
end;

define method %get-logger
    (logger :: <abstract-logger>, path :: <list>, original-name :: <string>)
  if (empty?(path))
    logger
  else
    let child = element(logger.logger-children, first(path), default: #f);
    child & %get-logger(child, rest(path), original-name)
  end
end method %get-logger;

define method %get-logger
    (logger :: <placeholder-logger>, path :: <list>, original-name :: <string>)
  ~empty?(path) & next-method()
end method %get-logger;

define method %get-logger
    (logger == #f, path :: <list>, original-name :: <string>)
  logging-error("Logger not found: %s", original-name);
end method %get-logger;


define method add-logger
    (parent :: <abstract-logger>, new :: <abstract-logger>, path :: <list>,
     original-name :: <string>)
  let name :: <string> = first(path);
  let child = element(parent.logger-children, name, default: #f);
  if (path.size == 1)
    if (child)
      if (instance?(child, <placeholder-logger>))
        // Copy the placeholder's children into the new logger that
        // is replacing it.
        for (grandchild in child.logger-children)
          new.logger-children[local-name(grandchild)] := grandchild;
          grandchild.logger-parent := new;
        end;
      else
        logging-error("Invalid logger name, %s.  A child logger named %s "
                      "already exists.", original-name, name);
      end;
    end;
    parent.logger-children[name] := new;
    new.logger-parent := parent;
  else
    if (~child)
      child := make(<placeholder-logger>, name: name, parent: parent);
      parent.logger-children[name] := child;
    end;
    add-logger(child, new, rest(path), original-name);
  end;
end method add-logger;




///////////////////////////////////////////////////////////
//// Log levels
////

// Root of the log level hierarchy.  Logging uses a simple class
// hierarchy to determine what messages should be logged.
//
define open abstract primary class <log-level> (<singleton-object>)
  constant slot level-name :: <byte-string>,
    init-keyword: name:;
end;

define open class <trace-level> (<log-level>)
  inherited slot level-name = "trace";
end;

define open class <debug-level> (<trace-level>)
  inherited slot level-name = "debug";
end;

define open class <info-level> (<debug-level>)
  inherited slot level-name = "info";
end;

define open class <warn-level> (<info-level>)
  inherited slot level-name = "WARN";
end;

define open class <error-level> (<warn-level>)
  inherited slot level-name = "ERROR";
end;

define constant $trace-level = make(<trace-level>);
define constant $debug-level = make(<debug-level>);
define constant $info-level = make(<info-level>);
define constant $warn-level = make(<warn-level>);
define constant $error-level = make(<error-level>);

define method log-level-applicable?
    (given-level :: <log-level>, logger-level :: <log-level>)
 => (applicable? :: <boolean>)
  instance?(given-level, logger-level.object-class)
end;




///////////////////////////////////////////////////////////
//// Logging messages
////

define thread variable *current-log-object* = #f;

define function current-log-object
    () => (obj :: <object>)
  *current-log-object*
end;

define thread variable *current-log-args* :: <sequence> = #[];

define function current-log-args
    () => (args :: <sequence>)
  *current-log-args*
end;

define thread variable *current-log-level* :: false-or(<log-level>) = #f;

define thread variable *current-log-target* :: false-or(<log-target>) = #f;

// This is generally called via log-info, log-error, etc, which simply curry
// the first argument.
//
define method log-message
    (given-level :: <log-level>, logger :: <logger>, object :: <object>, #rest args)
  if (logger.logger-enabled?
        & log-level-applicable?(given-level, logger.log-level))
    dynamic-bind (*current-log-object* = object,
                  *current-log-args* = args,
                  *current-log-level* = given-level)
      for (target :: <log-target> in logger.log-targets)
        dynamic-bind (*current-log-target* = target)
          apply(log-to-target, target, logger.log-formatter, object, args);
        end;
      end;
    end;
  end;
  if (logger.logger-additive?)
    apply(log-message, given-level, logger.logger-parent, object, args);
  end;
end method log-message;

define method log-message
    (given-level :: <log-level>, logger :: <placeholder-logger>, object :: <object>,
     #rest args)
  if (logger.logger-additive?)
    apply(log-message, given-level, logger.logger-parent, object, args)
  end;
end;

// I'm not sure log-trace is a useful distinction from log-debug.
// I copied it from log4j terminology.  I dropped log-fatal.

define constant log-trace = curry(log-message, $trace-level);

define constant log-debug = curry(log-message, $debug-level);

define method log-debug-if
    (test, logger :: <logger>, object, #rest args)
  if (test)
    apply(log-debug, logger, object, args);
  end;
end;

define constant log-info = curry(log-message, $info-level);

define constant log-warning = curry(log-message, $warn-level);

define constant log-error = curry(log-message, $error-level);


///////////////////////////////////////////////////////////
//// Targets
////

// Absract target for logging.  Subclasses represent different
// backend targets such as streams, files, databases, etc.
//
define open abstract class <log-target> (<closable-object>)
end;


// When this is called, the decision has already been made that this object
// must be logged for the given log level, so methods should unconditionally
// write the object to the backing store.
//
define open generic log-to-target
    (target :: <log-target>, formatter :: <log-formatter>, object :: <object>,
     #rest args);

// Override this if you want to use a normal formatter string but
// want to write objects to the log stream instead of strings.
//
define open generic write-message
    (target :: <log-target>, object :: <object>, #rest args);


// Note that there is no default method on "object :: <object>".

define method close
    (target :: <log-target>, #key)
 => ()
  // do nothing
end;

// A log target that simply discards its output.
define sealed class <null-log-target> (<log-target>)
end;

define method log-to-target
    (target :: <null-log-target>,
     formatter :: <log-formatter>, format-string :: <string>, #rest args)
  // do nothing
end;


// A log target that outputs directly to a stream.
// e.g., make(<stream-log-target>, stream: *standard-output*)
//
define open class <stream-log-target> (<log-target>)
  constant slot target-stream :: <stream>,
    required-init-keyword: #"stream";
end;

define method print-object
    (target :: <stream-log-target>, stream :: <stream>)
 => ()
  if (*print-escape?*)
    next-method();
  else
    write(stream, "stream target");
  end;
end method print-object;

define constant $stdout-log-target
  = make(<stream-log-target>, stream: *standard-output*);

define constant $stderr-log-target
  = make(<stream-log-target>, stream: *standard-error*);

define method log-to-target
    (target :: <stream-log-target>,
     formatter :: <log-formatter>, format-string :: <string>, #rest args)
  let stream :: <stream> = target.target-stream;
  with-stream-locked (stream)
    pattern-to-stream(formatter, stream);
    write(stream, "\n");
    force-output(stream);
  end;
end method log-to-target;

define method write-message
    (target :: <stream-log-target>, format-string :: <string>, #rest args)
  apply(format, target.target-stream, format-string, args);
end;

define method date-to-stream
    (stream :: <stream>, date :: <date>)
  let (year, month, day, hours, minutes, seconds, day-of-week, time-zone-offset)
    = decode-date(date);
  let millis = round/(date-microseconds(date), 1000);
  format(stream, "%d-%s%d-%s%d %s%d:%s%d:%s%d.%s%s%d %s%s%d%s%d",
         year,
         iff(month < 10, "0", ""),
         month,
         iff(day < 10, "0", ""),
         day,
         iff(hours < 10, "0", ""),
         hours,
         iff(minutes < 10, "0", ""),
         minutes,
         iff(seconds < 10, "0", ""),
         seconds,
         iff(millis < 100, "0", ""),
         iff(millis < 10, "0", ""),
         millis,
         iff(negative?(time-zone-offset), "-", "+"),  // +0000
         iff(abs(floor/(time-zone-offset, 60)) < 10, "0", ""),
         abs(floor/(time-zone-offset, 60)),
         iff(modulo(time-zone-offset, 60) < 10, "0", ""),
         modulo(time-zone-offset, 60));
end;

define method as-common-logfile-date
    (date :: <date>)
 => (common-logfile-date :: <string>)
  //Common Logfile Format Date: "28/Mar/2004:04:47:19 +0200"
  //http://www.w3.org/Daemon/User/Config/Logging.html
  format-date("%d/%b/%Y:%T %z", date)
end method as-common-logfile-date;


// A log target that is backed by a single, monolithic file.
// (Why is this not a subclass of <stream-log-target>?)
//
define class <file-log-target> (<log-target>)
  constant slot target-pathname :: <pathname>,
    required-init-keyword: pathname:;
  slot target-stream :: false-or(<file-stream>),
    init-value: #f;
end;

define method initialize
    (target :: <file-log-target>, #key)
  next-method();
  open-target-stream(target);
end;

define method print-object
    (target :: <file-log-target>, stream :: <stream>)
 => ()
  if (*print-escape?*)
    next-method();
  else
    format(stream, "file %s", as(<string>, target.target-pathname));
  end;
end method print-object;

define open generic open-target-stream
    (target :: <file-log-target>) => (stream :: <stream>);

define method open-target-stream
    (target :: <file-log-target>)
 => (stream :: <file-stream>)
  target.target-stream := make(<file-stream>,
                               locator: target.target-pathname,
                               element-type: <character>,
                               direction: #"output",
                               if-exists: #"append",
                               if-does-not-exist: #"create")
end;

define method log-to-target
    (target :: <file-log-target>,
     formatter :: <log-formatter>, format-string :: <string>, #rest format-args)
  let stream :: <stream> = target.target-stream;
  with-stream-locked (stream)
    pattern-to-stream(formatter, stream);
    write(stream, "\n");
    force-output(stream);
  end;
end method log-to-target;

define method write-message
    (target :: <file-log-target>, format-string :: <string>, #rest args)
  apply(format, target.target-stream, format-string, args);
end;

define method close
    (target :: <file-log-target>, #key abort?)
 => ()
  if (target.target-stream)
    close(target.target-stream, abort?: abort?);
  end;
end;

// A log target that is backed by a file and ensures that the file
// only grows to a certain size, after which it is renamed to
// filename.<date-when-file-was-opened>.
//
// I investigated making this a subclass of <wrapper-stream> but it
// didn't work well due to the need to create the inner-stream
// first and pass it as an init arg.  That doesn't work too well
// given that I want to roll the log if the file exists when I
// first attempt to open it.  It leads to various special cases.
//
// Attempt to re-open the file if logging to it gets (the equivalent
// of) bad file descriptor?
//
define class <rolling-file-log-target> (<file-log-target>)

  constant slot max-file-size :: <integer>,
    init-keyword: #"max-size",
    init-value: 20 * 1024 * 1024;

  // TODO: not yet implemented
  // If this is #f then all versions are kept.
  //constant slot keep-versions :: false-or(<integer>) = #f,
  //  init-keyword: #"keep-versions";

  // TODO: not yet implemented
  //constant slot compress-on-close? :: <boolean> = #t,
  //  init-keyword: #"compress?";

  // Date when the underlying file was created.  When it gets closed
  // it will be renamed with this date in the name.
  slot file-creation-date :: <date>,
    init-function: current-date;

end class <rolling-file-log-target>;

define constant $log-roller-lock :: <lock> = make(<lock>);


define method initialize
    (target :: <rolling-file-log-target>, #key roll :: <boolean> = #t)
  if (roll
        & file-exists?(target.target-pathname)
        & file-property(target.target-pathname, #"size") > 0)
    roll-log-file(target);
  end;
  next-method();
end method initialize;

define method print-object
    (target :: <rolling-file-log-target>, stream :: <stream>)
 => ()
  if (*print-escape?*)
    next-method();
  else
    format(stream, "rolling file %s", as(<string>, target.target-pathname));
  end;
end method print-object;

define method log-to-target
    (target :: <rolling-file-log-target>,
     formatter :: <log-formatter>, format-string :: <string>, #rest format-args)
  next-method();
  // todo -- calling stream-size may be very slow?  Maybe log-to-target should
  // return the number of bytes written, but that could be inefficient (e.g.,
  // it might have to format to string and then write that to the underlying
  // stream instead of formatting directly to the stream).
  if (stream-size(target.target-stream) >= target.max-file-size)
    roll-log-file(target);
  end;
end;

define method roll-log-file
    (target :: <rolling-file-log-target>)
  with-lock ($log-roller-lock)
    if (target.target-stream)  // may be #f first time
      close(target.target-stream);
    end;
    // todo -- make the archived log filename accept %{date:fmt} and
    //         %{version} escapes.  e.g., "foo.log.%{version}"
    let date = format-date("%Y%m%dT%H%M%S", target.file-creation-date);
    let oldloc = as(<file-locator>, target.target-pathname);
    let newloc = merge-locators(as(<file-locator>,
                                   concatenate(locator-name(oldloc), ".", date)),
                                oldloc);
    rename-file(oldloc, newloc);
    target.file-creation-date := current-date();
    open-target-stream(target);
  end with-lock;
end method roll-log-file;


///////////////////////////////////////////////////////////
//// Formatting
////

define open class <log-formatter> (<object>)
  constant slot formatter-pattern :: <string>,
    required-init-keyword: pattern:;
  slot parsed-pattern :: <sequence>;
end class <log-formatter>;

begin
  // ignore.  leave in for debugging for now.
  formatter-pattern;
end;

define method initialize
    (formatter :: <log-formatter>, #key pattern :: <string>)
  next-method();
  formatter.parsed-pattern := parse-formatter-pattern(pattern);
end;

// Should be called with the stream locked.
//
define method pattern-to-stream
    (formatter :: <log-formatter>, stream :: <stream>)
  for (item in formatter.parsed-pattern)
    if (instance?(item, <string>))
      write(stream, item);
    else
      // This is a little hokey, but it was easier to allow some
      // formatter functions to just return a string and others
      // to write to the underlying stream, so if the function
      // returns #f it means "i already did my output".
      let result = item();
      if (result)
        write(stream, result);
      end;
    end;
  end;
end method pattern-to-stream;

// Parse a string of the form "%{r} blah %{m} ..." into a list of functions
// and/or strings.  The functions can be called with no arguments and return
// strings.  The concatenation of all the resulting strings is the log message.
// (The concatenation needn't ever be done if writing to a stream, but I do
// wonder which would be faster, concatenation or multiple stream writes.
// Might be worth benchmarking at some point.)
//
define method parse-formatter-pattern
    (pattern :: <string>)
 => (parsed :: <sequence>)
  let result :: <stretchy-vector> = make(<stretchy-vector>);
  block (exit)
    let dispatch-char :: <byte-character> = '%';
    let index :: <integer> = 0;
    let control-size :: <integer> = pattern.size;
    local method next-char () => (char :: <character>)
            if (index >= control-size)
              logging-error("Log format control string ended prematurely: %s",
                            pattern);
            else
              let char = pattern[index];
              inc!(index);
              char
            end
          end method;
    local method peek-char () => (char :: false-or(<character>))
            if (index < control-size)
              pattern[index]
            end
          end;
    while (index < control-size)
      // Skip to dispatch char.
      for (i :: <integer> = index then (i + 1),
           until: ((i == control-size)
                   | (pattern[i] == dispatch-char)))
      finally
        if (i ~== index)
          add!(result, copy-sequence(pattern, start: index, end: i));
        end;
        if (i == control-size)
          exit();
        else
          index := i + 1;
        end;
      end for;
      let start :: <integer> = index;
      let align :: <symbol> = #"right";
      let width :: <integer> = 0;
      let char = next-char();
      if (char == '-')
        align := #"left";
        char := next-char();
      end;
      if (member?(char, "0123456789"))
        let (wid, idx) = string-to-integer(pattern, start: index - 1);
        width := wid;
        index := idx;
        char := next-char();
      end;
      local method pad (string :: <string>)
              let len :: <integer> = string.size;
              if (width <= len)
                string
              else
                let fill :: <string> = make(<string>, size: width - len, fill: ' ');
                if (align == #"left")
                  concatenate(string, fill)
                else
                  concatenate(fill, string)
                end
              end
            end method;
      local method parse-long-format-control ()
              let bpos = index;
              while (~member?(peek-char(), ":}")) next-char() end;
              let word = copy-sequence(pattern, start: bpos, end: index);
              let arg = #f;
              if (pattern[index] == ':')
                next-char();
                let start = index;
                while(peek-char() ~= '}') next-char() end;
                arg := copy-sequence(pattern, start: start, end: index);
              end;
              next-char();   // eat '}'
              select (word by \=)
                "date" => method ()
                            pad(iff(arg,
                                    format-date(arg, current-date()),
                                    as-iso8601-string(current-date())))
                          end;
                "level" => method () pad(level-name(*current-log-level*)) end;
                "message" =>
                  method ()
                    apply(write-message, *current-log-target*, *current-log-object*,
                          *current-log-args*);
                    #f
                  end;
                "pid" => compose(pad, integer-to-string, current-process-id);
                "millis" => compose(pad, number-to-string, elapsed-milliseconds);
                "thread" => compose(pad, thread-name, current-thread);
                otherwise =>
                  // Unknown control string.  Just output the text we've seen...
                  copy-sequence(pattern, start: start, end: index);
              end select;
            end method;
      add!(result,
           select (char)
             '{' => parse-long-format-control();
             'd' => compose(pad, as-iso8601-string, current-date);
             'l', 'L' => method ()
                           pad(level-name(*current-log-level*))
                         end;
             'm' => method ()
                      apply(write-message, *current-log-target*, *current-log-object*,
                            *current-log-args*);
                      #f
                    end;
             'p' => compose(pad, integer-to-string, current-process-id);
             'r' => compose(pad, number-to-string, elapsed-milliseconds);
             't' => compose(pad, thread-name, current-thread);
             '%' => pad("%");
             otherwise =>
               // Unknown control char.  Just output the text we've seen...
               copy-sequence(pattern, start: start, end: index);
           end);
    end while;
  end block;
  result
end method parse-formatter-pattern;

define constant $default-log-formatter :: <log-formatter>
  = make(<log-formatter>, pattern: "%d %-5L [%t] %m");

// stub -- not sure if getpid exists yet.
//         it's not in the operating-system module at least.
//
define function current-process-id
    () => (pid :: <integer>)
  0
end;

define constant $application-start-date :: <date> = current-date();

define function elapsed-milliseconds
    () => (millis :: <double-integer>)
  let duration :: <duration> = current-date() - $application-start-date;
  let (days, hours, minutes, seconds, microseconds) = decode-duration(duration);
  plus(div(microseconds, 1000.0),
       plus(mul(seconds, 1000),
            plus(mul(minutes, 60000),
                 plus(mul(hours, 3600000), mul(days, 86400000)))))
end function elapsed-milliseconds;


/////////////////////////////////////////////////////
//// For use by the test suite
////

define function reset-logging
    ()
  // maybe should close existing log targets?
  $root-logger := make(<logger>, name: "root", additive: #f, enabled: #f);
end;

/////////////////////////////////////////////////////
//// Initialize
////

begin
  reset-logging();
end;


