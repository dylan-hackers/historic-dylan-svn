Module:    internals
Author:    Carl Gay
Synopsis:  Log HTTP server messages
Copyright: Copyright (c) 2001 Carl L. Gay.  All rights reserved.
License:   Functional Objects Library Public License Version 1.0
Warranty:  Distributed WITHOUT WARRANTY OF ANY KIND


define constant $log-info :: <symbol> = #"info";
define constant $log-warn :: <symbol> = #"warn";
define constant $log-error :: <symbol> = #"error";
define constant $log-debug :: <symbol> = #"debug";

define function log-level-name (level :: <symbol>) => (name :: <byte-string>)
  select (level)
    $log-info  => "info";
    $log-warn  => "warn";
    $log-error => "err";
    $log-debug => "dbg";
    otherwise  => "";
  end;
end;

// Log only these types of messages.
define variable *log-types* :: <simple-object-vector>
  = vector($log-info, $log-warn, $log-error, $log-debug);

define method log-message (log-level :: <symbol>, format-string :: <string>, #rest format-args)
  if (member?(log-level, *log-types*))
    log-date();
    format-out(" [%s] ", log-level-name(log-level));
    apply(format-out, format-string, format-args);
    format-out("\n");
    force-output(*standard-output*);
  end;
end log-message;

define function log-date ()
  date-to-stream(*standard-output*, current-date());
end;

define method debug-format (format-string, #rest format-args)
  apply(log-message, $log-debug, format-string, format-args);
end;

define method log-info (format-string, #rest format-args)
  apply(log-message, $log-info, format-string, format-args);
end;

define method log-warning (format-string, #rest format-args)
  apply(log-message, $log-warn, format-string, format-args);
end;

define method log-error (err :: <error>)
  log-message($log-error, "%d %s", http-error-code(err), condition-to-string(err));
end;

define method log-debug (format-string, #rest format-args)
  apply(log-message, $log-debug, format-string, format-args);
end;


