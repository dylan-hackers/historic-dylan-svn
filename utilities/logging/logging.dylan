Module:    logging
Author:    Carl Gay
Synopsis:  Simple logging mechanism.  Some ideas taken from log4j.


/* 

todo -- documentation, tests

todo -- add-target, remove-target

todo -- Long format controls, e.g. %{thread}

todo -- Add a way to extend the set of format directives from outside
        the library.

todo -- Handle errors gracefully.  e.g., if the disk fills up it may be
        better to do nothing than to err.

todo -- A log target that rolls the file when either a max size or a max
        time is reached, whichever comes first.  Should make it possible
        for users to override roll-log-file? and roll-log-file-name
        methods if they want to roll their own.

??? -- Is there a reasonable use case where you might not want \n at the
       end of each log entry?  Rather than hard-coding the \n one could
       specify it in the formatter's control string.  The worry is that
       everyone would forget to add it every time they write a new formatter.

*/


////
//// Loggers
////

define class <logger> (<object>)
  // A dotted path name.  All parent loggers in the path must already exist.
  constant slot logger-name :: <string>,
    required-init-keyword: name:;

  slot log-level :: <log-level>,
    init-keyword: level:,
    init-value: $trace-level;

  constant slot log-targets :: <sequence>,
    init-keyword: targets:,
    init-function: curry(list, $stdout-log-target);

  constant slot log-formatter :: <log-formatter>,
    init-keyword: formatter:,
    init-value: $default-log-formatter;

  // Child loggers, by dotted name component
  constant slot logger-children :: <string-table>,
    init-keyword: children:,
    init-function: curry(make, <string-table>);

/*
  // If disabled then logging requests will be ignored for this logger
  // and its ancestors (if any, and if additivity is enabled).
  slot enabled? :: <boolean>,
    init-keyword: enabled:,
    init-value: #t;
*/
end class <logger>;

define method initialize
    (logger :: <logger>, #key name :: <string>)
  next-method();
  add-logger($root-logger, logger, as(<list>, split(name, '.')), name);
end;

define open class <log-error> (<error>, <format-string-condition>)
end;

define constant $root-logger = make(<logger>, name: "root" /*, enabled: #f */);

define method get-root-logger
    () => (logger :: <logger>)
  $root-logger
end;

define method get-logger
    (name :: <string>) => (logger :: false-or(<logger>))
  local method find-logger (path, logger)
          if (empty?(path))
            logger
          else
            let child = element(logger.logger-children, first(path), default: #f);
            if (child)
              find-logger(rest(path), child)
            else
              signal(make(<log-error>,
                          format-string: "Logger %s not found",
                          format-arguments: list(name)));
            end
          end
        end;
  find-logger(as(<list>, split(name, '.')), $root-logger)
end method get-logger;

// todo -- log4j doesn't require that parent loggers be created before child
//         loggers, and I think that's a nice feature to add here.
//
define method add-logger
    (parent :: <logger>, new-logger :: <logger>, path :: <list>,
     original-name :: <string>)
  let name :: <string> = first(path);
  let child = element(parent.logger-children, name, default: #f);
  if (path.size == 1)
    if (child)
      signal(make(<log-error>,
                  format-string: "Invalid logger name, %s.  A child named %s already exists.",
                  format-arguments: list(original-name, name)));
    else
      parent.logger-children[name] := new-logger;
    end;
  elseif (child)
    add-logger(child, new-logger, rest(path), original-name);
  else
    signal(make(<log-error>,
                format-string: "Invalid logger name, %s.  No child logger named %s found.",
                format-arguments: list(original-name, name)));
  end;
end method add-logger;


////
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
  inherited slot level-name = "TRACE";
end;

define open class <debug-level> (<trace-level>)
  inherited slot level-name = "DEBUG";
end;

define open class <info-level> (<debug-level>)
  inherited slot level-name = "INFO";
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




////
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

define thread variable *current-log-level* = #f;

// This is generally called via log-info, log-error, etc, which simply curry
// the first argument.
//
define function log
    (given-level :: <log-level>, logger :: <logger>, object :: <object>, #rest args)
  if (log-level-applicable?(given-level, logger.log-level))
    dynamic-bind (*current-log-object* = object,
                  *current-log-args* = args,
                  *current-log-level* = given-level)
      for (target :: <log-target> in logger.log-targets)
        apply(log-to-target, target, logger.log-formatter, object, args);
      end;
    end;
  end;
end function log;

// I'm not sure log-trace is a useful distinction from log-debug.
// I copied it from log4j terminology.  I dropped log-fatal.

define constant log-trace = curry(log, $trace-level);

define constant log-debug = curry(log, $debug-level);

define method log-debug-if
    (test, logger :: <logger>, object, #rest args)
  if (test)
    apply(log-debug, logger, object, args);
  end;
end;

define constant log-info = curry(log, $info-level);

define constant log-warning = curry(log, $warn-level);

define constant log-error = curry(log, $error-level);


////
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

// Note that there is no default method on "object :: <object>".

define method close
    (target :: <log-target>, #key)
 => ()
  // do nothing
end;

// A log target that simply discards its output.
define class <null-log-target> (<log-target>)
end;

define method log-to-target
    (target :: <null-log-target>,
     formatter :: <log-formatter>,
     format-string :: <string>,
     #rest args)
  // do nothing
end;


// A log target that outputs directly to a stream.
// e.g., make(<stream-log-target>, stream: *standard-output*)
//
define class <stream-log-target> (<log-target>)
  constant slot target-stream :: <stream>,
    required-init-keyword: #"stream";
end;

define constant $stdout-log-target
  = make(<stream-log-target>, stream: *standard-output*);

define constant $stderr-log-target
  = make(<stream-log-target>, stream: *standard-error*);

define method log-to-target
    (target :: <stream-log-target>,
     formatter :: <log-formatter>,
     format-string :: <string>,
     #rest args)
  let stream :: <stream> = target.target-stream;
  with-stream-locked (stream)
    format-to-stream(formatter, stream);
    write(stream, "\n");
    force-output(stream);
  end;
end method log-to-target;

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
  constant slot target-file :: <locator>,
    required-init-keyword: file:;
  slot target-stream :: false-or(<file-stream>),
    init-value: #f;
end;

define method initialize
    (target :: <file-log-target>, #key)
  next-method();
  target.target-stream := open-target-stream(target);
end;

define method open-target-stream
    (target :: <file-log-target>)
 => (stream :: <file-stream>)
  make(<file-stream>,
       locator: target.target-file,
       element-type: <character>,
       direction: #"output",
       if-exists: #"append",
       if-does-not-exist: #"create")
end;

define method log-to-target
    (target :: <file-log-target>,
     formatter :: <log-formatter>,
     format-string :: <string>,
     #rest format-args)
  let stream :: <stream> = target.target-stream;
  with-stream-locked (stream)
    format-to-stream(formatter, stream);
    write(stream, "\n");
    force-output(stream);
  end;
end method log-to-target;

define method close
    (target :: <file-log-target>, #key abort?)
 => ()
  if (target.target-stream)
    close(target.target-stream, abort?: abort?);
  end;
end;

// A log target that is backed by a file and ensures that the file
// only grows to a certain size, after which it is renamed to
// filename.<date-when-file-was-opened>.  (TODO: optionally compress
// the old log files as well.)
//
// I investigated making this a subclass of <wrapper-stream> but it
// didn't work well due to the need to create the inner-stream
// first and pass it as an init arg.  That doesn't work too well
// given that I want to roll the log if the file exists when I
// first attempt to open it.  It leads to various special cases.
//
define class <rolling-file-log-target> (<file-log-target>)

  constant slot max-file-size :: <integer>,
    required-init-keyword: #"max-size";

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

define method log-to-target
    (target :: <rolling-file-log-target>,
     formatter :: <log-formatter>,
     format-string :: <string>,
     #rest format-args)
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
    let date = as-iso8601-string(target.file-creation-date);
    let oldloc = target.target-file;
    let newloc = merge-locators(as(<file-locator>,
                                   concatenate(locator-name(oldloc), ".", date)),
                                oldloc);
    rename-file(oldloc, newloc);
    target.file-creation-date := current-date();
    target.target-stream := open-target-stream(target);
  end with-lock;
end method roll-log-file;


////
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
define method format-to-stream
    (formatter :: <log-formatter>, stream :: <stream>)
  for (item in formatter.parsed-pattern)
    if (instance?(item, <string>))
      write(stream, item);
    else
      write(stream, item());
    end;
  end;
end method format-to-stream;

// Parse a string of the form "%{r} blah %{m} ..." into a list of functions
// and/or strings.  The functions can be called with no arguments and return
// strings.  The concatenation of all the resulting strings is the log message.
// (The concatenation needn't ever be done if writing to a stream, but I do
// wonder which would be faster, concatenation or multiple stream writes.
// Might be worth benchmarking at some point.)
//
// "%{date:%Y%M%d:%H%M%S%z} - %{level} - %{pid} - %{elapsed} %{thread} -- verbatim %{message}"
// "%-5L" = "%-5{level}"
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
              signal(make(<log-error>,
                          format-string: "Log format control string ended prematurely: %s",
                          format-arguments: list(pattern)));
            else
              let char = pattern[index];
              inc!(index);
              char
            end
          end method;
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
      if (digit?(char))
        let (wid, idx) = string-to-integer(pattern, start: index);
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
      add!(result,
           select (char)
             '{' =>
               error("Long format controls (e.g., %{foo}) not yet implemented.");
             'd' => compose(pad, as-iso8601-string, current-date);
             'l', 'L' => method ()
                           pad(level-name(*current-log-level*))
                         end;
             'm' => method ()
                      apply(write-object, *current-log-target*, *current-log-object*,
                            *current-log-args*);
                    end;
             'p' => compose(pad, integer-to-string, current-process-id);
             'r' => compose(pad, elapsed-milliseconds);
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

