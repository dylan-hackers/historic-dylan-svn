Module:    utilities
Author:    Carl Gay
Synopsis:  Simple logging mechanism
Copyright: Copyright (c) 2001 Carl L. Gay.  All rights reserved.
License:   Functional Objects Library Public License Version 1.0
Warranty:  Distributed WITHOUT WARRANTY OF ANY KIND


// Use this around your top-level loop (for example) to redirect log
// output somewhere other than *standard-output*.
//
define macro with-log-output-to
  { with-log-output-to (?stream:expression) ?:body end }
  => { dynamic-bind (*log-stream* = ?stream) ?body end }
end;

define thread variable *log-stream* = *standard-error*;

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

// Messages will be logged if the specified log level is a subclass of any
// of the classes in *log-levels*.  Configuration code should add to this.
//
define variable *log-levels* :: <sequence> = make(<stretchy-vector>);

define method add-log-level
    (level :: <class>) => ()
  *log-levels* := add-new!(*log-levels*, level);
end;

define method remove-log-level
    (level :: <class>) => ()
  *log-levels* := remove!(*log-levels*, level);
end;

define method clear-log-levels
    () => ()
  remove-all-keys!(*log-levels*);
end;

// All log messages should pass through here.
define method log-message
    (level :: <log-level>, format-string :: <string>, #rest format-args)
  when (any?(curry(instance?, level), *log-levels*))
    log-date();
    format(*log-stream*, " [%s] ", name(level));
    apply(format, *log-stream*, format-string, format-args);
    format(*log-stream*, "\n");
    force-output(*log-stream*);
  end;
end;

define method date-to-stream
    (stream :: <stream>, date :: <date>)
  let (year, month, day, hours, minutes, seconds, day-of-week, time-zone-offset) = decode-date(date);
  format(stream, "%d-%s%d-%s%d %s%d:%s%d:%s%d %s%s%d:%s%d",
         year, iff(month < 10, "0", ""), month, iff(day < 10, "0", ""), day,
         iff(hours < 10, "0", ""), hours, iff(minutes < 10, "0", ""), minutes,
         iff(seconds < 10, "0", ""), seconds, iff(positive?(time-zone-offset), "+", "-"),
         iff(floor/(time-zone-offset, 60) < 10, "0", ""), floor/(time-zone-offset, 60),
         iff(modulo(time-zone-offset, 60) < 10, "0", ""), modulo(time-zone-offset, 60)
  );
end;

define function log-date (#key date :: <date> = current-date())
  date-to-stream(*log-stream*, date);
end;

define method debug-format (format-string, #rest format-args)
  apply(log-message, $log-debug, format-string, format-args);
end;

define constant log-copious = curry(log-message, $log-copious);
define constant log-verbose = curry(log-message, $log-verbose);
define constant log-debug = curry(log-message, $log-debug);
define constant log-info = curry(log-message, $log-info);
define constant log-warning = curry(log-message, $log-warning);
define constant log-error = curry(log-message, $log-error);

define method log-debug-if (test, format-string, #rest format-args)
  if (test)
    apply(log-message, $log-debug, format-string, format-args);
  end;
end;

define method as-common-logfile-date (date :: <date>) => (common-logfile-date :: <string>)
  let $month-names
    = #["January", "February", "March", "April", "May", "June",
        "July", "August", "September", "October", "November", "December"];
  //Common Logfile Format Date: "28/Mar/2004:04:47:19 +0200"
  //http://www.w3.org/Daemon/User/Config/Logging.html
  let (iyear, imonth, iday, ihours, iminutes, iseconds, day-of-week, time-zone-offset) = decode-date(date);
  local method wrap0 (int :: <integer>) => (string :: <string>)
    if (int < 10)
      concatenate("0", integer-to-string(int));
    else
      integer-to-string(int);
    end if;
  end;

  let day = wrap0(iday);
  let month = substring($month-names[imonth - 1], 0, 3);
  let year = integer-to-string(iyear);
  let hours = wrap0(ihours);
  let minutes = wrap0(iminutes);
  let seconds = wrap0(iseconds);
  let prefix = if (positive?(time-zone-offset))
                 "+";
               else
                 "-";
               end if;
  let timezone = concatenate(prefix,
                   wrap0( floor/(time-zone-offset, 60) ),
                   wrap0( modulo(time-zone-offset, 60) )
                  );
  concatenate(day, "/", month, "/", year, ":", hours, ":", minutes,
              ":", seconds, " ", timezone); 
end method as-common-logfile-date;

define method log-logfile(file :: <string>, entry :: <string>)
  with-open-file(logfile = file, direction: #"output", if-exists: #"append",
                 if-does-not-exist: #"create", element-type: <byte>)
    write(logfile, entry);
  end;
end method;

begin
  add-log-level(<log-info>);
end;

