Module:    logging
Synopsis:  Logging functionality
Author:    Chris Double
Copyright: Copyright (c) 2001, Chris Double.  All rights reserved.

// Default location for log files. What should this be set to?
// Seems wrong to hard code to a directory...
// Must end in a trailing '\'.
define thread variable *log-directory* = "c:\\logs\\";
define thread variable *log-system-name* = "logging";
define thread variable *logging-enabled* :: <boolean> = #t;

define macro with-logging-system
{ with-logging-system(?system:expression)
    ?:body
  end }
=> { dynamic-bind(*log-system-name* = ?system)
       ?body
     end }
end macro with-logging-system;

define macro without-logging
{ without-logging()
    ?:body
  end }
=> { dynamic-bind(*logging-enabled* = #f)
       ?body
     end }
end macro without-logging;

define macro with-logging
{ with-logging()
    ?:body
  end }
=> { dynamic-bind(*logging-enabled* = #t)
       ?body
     end }
end macro with-logging;

define macro log-block
{ log-block(?args:*)
    ?:body
  end }
=> { begin
       let message = format-to-string(?args);
       block()
         log-message("[%s\n", message);
         ?body
       cleanup
         log-message(" %s]\n", message);
       end block 
      end }
end macro log-block;

define macro log-block-out
{ log-block-out(?args:*)
    ?:body
  end }
=> { begin
       let message = format-to-string(?args);
       block()
         log-message-out("[%s\n", message);
         ?body
       cleanup
         log-message-out(" %s]\n", message);
       end block 
      end }
end macro log-block-out;

// Used mainly when creating threads to bind the logging
// variables to current values. Returns a function that 
// binds the values of the logging variables to the current values.
define method bind-logging-vars(function :: <function>)
  let enabled? = *logging-enabled*;
  let directory = *log-directory*;
  let system = *log-system-name*;

  method()
    dynamic-bind(*logging-enabled* = enabled?,
                 *log-directory* = directory,
                 *log-system-name* = system)
      function()
    end dynamic-bind;
  end;
end method bind-logging-vars;

define constant $log-lock = make(<lock>);

define method construct-log-info()
 => (time :: <string>, filename :: <string>)
    let date = current-date();
    let fn = format-to-string("%s%s-%s-%s-%s.txt",
                              *log-directory*,
                              integer-to-string(date.date-year, size: 4, fill: '0'),
                              integer-to-string(date.date-month, size: 2, fill: '0'),
                              integer-to-string(date.date-day, size: 2, fill: '0'),
                              *log-system-name*);
    let ts = format-to-string("%s:%s:%s",
                              integer-to-string(date.date-hours, size: 2, fill: '0'),
                              integer-to-string(date.date-minutes, size: 2, fill: '0'),
                              integer-to-string(date.date-seconds, size: 2, fill: '0'));
    values(ts, fn);
end method construct-log-info;

define method log-message(#rest args)
 => ()
  when(*logging-enabled*)
    let (ts, fn) = construct-log-info();
    with-lock($log-lock)
      with-open-file(fs = fn, 
                     direction: #"output",
                     if-exists: #"append", 
                     if-does-not-exist: #"create")
        write(fs, ts);
        write(fs, " - ");
        apply(format, fs, args);
      end with-open-file;
    end with-lock;
  end when;
end method log-message;

// Logs message to *standard-out* as well as to the
// log file.
define method log-message-out(#rest args)
 => ()
  when(*logging-enabled*)
    let (ts, fn) = construct-log-info();
    with-lock($log-lock)
      with-open-file(fs = fn, 
                     direction: #"output",
                     if-exists: #"append", 
                     if-does-not-exist: #"create")
        write(fs, ts);
        write(fs, " - ");
        apply(format, fs, args);
      end with-open-file;

      format(*standard-output*, "%s - ", ts);
      apply(format, *standard-output*, args);
      force-output(*standard-output*);
    end with-lock;
  end when;
end method log-message-out;

