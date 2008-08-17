Module:    logging
Author:    Carl Gay
Synopsis:  Simplistic logging mechanism


/* 

The most basic method for doing logging is log-message, which is
passed a <log-target> and a <log-level>.  If the log-level passed to
log-message is an instance of the active log level for the current
virtual host then the message will be logged. Otherwise it'll be
dropped on the floor.  <log-target>s allow for arbitrary backing
store for logs, like files, databases, the net, etc.

todo -- Thread safety?

todo -- Handle errors gracefully.  e.g., if the disk fills up it may be
        better to do nothing than to err.

todo -- Improved formatting control.

todo -- A log target that rolls the file when either a max size or a max
        time is reached, whichever comes first.  Should make it possible
        for users to override roll-log-file? and roll-log-file-name
        methods if they want to roll their own.

*/

// Root of the log level hierarchy.  Logging uses a simple class
// hierarchy to determine what messages should be logged.
//
define open abstract primary class <log-level> (<singleton-object>)
  constant slot name :: <byte-string>, init-keyword: #"name";
end;

define open class <log-copious> (<log-level>)
  inherited slot name = "BLAH";
end;

define open class <log-verbose> (<log-copious>)
  inherited slot name = "VERB";
end;

define open class <log-debug> (<log-verbose>)
  inherited slot name = "DBG ";
end;

define open class <log-info> (<log-debug>)
  inherited slot name = "INFO";
end;

define open class <log-warning> (<log-info>)
  inherited slot name = "WARN"
end;

define open class <log-error> (<log-warning>)
  inherited slot name = "ERR ";
end;

define constant $log-copious :: <log-copious> = make(<log-copious>);
define constant $log-verbose :: <log-verbose> = make(<log-verbose>);
define constant $log-debug :: <log-debug> = make(<log-debug>);
define constant $log-info :: <log-info> = make(<log-info>);
define constant $log-warning :: <log-warning> = make(<log-warning>);
define constant $log-error :: <log-error> = make(<log-error>);


// Absract target for logging.  Subclasses represent different
// backend targets such as streams, files, databases, etc.
//
define abstract class <log-target> (<closable-object>)
  slot log-level :: <log-level> = $log-verbose,
    init-keyword: #"log-level";
end;

// Log a message with date and log level prepended.
define generic log
    (level :: <log-level>, target :: <log-target>, format-string :: <string>,
     #rest format-args);

// Raw logging, with no date prepended, and no formatting performed.
// The 'line' arg normally shouldn't have a \n at the end.
define generic log-raw
    (target :: <log-target>, line :: <string>);

define method close
    (target :: <log-target>, #key)
 => ()
  // do nothing
end;

define method log-level-applicable?
    (level :: <log-level>, target :: <log-target>)
 => (applicable? :: <boolean>)
  instance?(level, target.log-level.object-class)
end;

// Standard logging, with date and log-level prepended.
define method log
    (level :: <log-level>, target :: <log-target>, format-string :: <string>,
     #rest format-args)
  if (log-level-applicable?(level, target))
    // TODO: Using concatenate would be more efficient than with-output-to-string.
    let line :: <string>
      = with-output-to-string (stream)
          date-to-stream(stream, current-date());
          format(stream, " [%s] ", name(level));
          apply(format, stream, format-string, format-args);
        end;
    log-raw(target, line);
  end;
end;

define constant log-copious = curry(log, $log-copious);
define constant log-verbose = curry(log, $log-verbose);
define constant log-debug = curry(log, $log-debug);
define constant log-info = curry(log, $log-info);
define constant log-warning = curry(log, $log-warning);
define constant log-error = curry(log, $log-error);


// A log target that simply discards its output.
define class <null-log-target> (<log-target>)
end;

define method log-raw
    (target :: <null-log-target>, line :: <string>)
 => ()
  // do nothing
end;


// A log target that outputs directly to a stream.
// e.g., make(<stream-log-target>, stream: *standard-output*)
//
define class <stream-log-target> (<log-target>)
  constant slot target-stream :: <stream>,
    required-init-keyword: #"stream";
end;

define method log-raw
    (target :: <stream-log-target>, line :: <string>)
 => ()
  let stream :: <stream> = target.target-stream;
  write(stream, line);
  write(stream, "\n");
  force-output(stream);
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

define method log-debug-if (test, format-string, #rest format-args)
  if (test)
    apply(log-debug, format-string, format-args);
  end;
end;

define method as-common-logfile-date
    (date :: <date>)
 => (common-logfile-date :: <string>)
  //Common Logfile Format Date: "28/Mar/2004:04:47:19 +0200"
  //http://www.w3.org/Daemon/User/Config/Logging.html
  format-date("%d/%b/%Y:%T %z", date);
end method as-common-logfile-date;


// A log target that is backed by a single monolithic file.
//
define class <file-log-target> (<log-target>)
  constant slot target-file :: <locator>,
    required-init-keyword: #"file";
  slot target-stream :: false-or(<file-stream>) = #f;
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

define method log-raw
    (target :: <file-log-target>, line :: <string>)
  let stream :: <stream> = target.target-stream;
  write(stream, line);
  write(stream, "\n");
  force-output(stream);
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

  // Number of bytes written since the current inner-stream was opened.
  slot bytes-written :: <integer> = 0;

end class <rolling-file-log-target>;

define method initialize
    (target :: <rolling-file-log-target>, #key)
  next-method();
  // Need this in case we're appending to an old log.
  target.bytes-written := stream-size(target.target-stream);
end;

define constant $log-roller-lock :: <lock> = make(<lock>);

define method log-raw
    (target :: <rolling-file-log-target>, line :: <string>)
  next-method();
  inc!(target.bytes-written, line.size);
  if (target.bytes-written >= target.max-file-size)
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
    target.bytes-written := 0;
    target.file-creation-date := current-date();
    target.target-stream := open-target-stream(target);
  end with-lock;
end;

