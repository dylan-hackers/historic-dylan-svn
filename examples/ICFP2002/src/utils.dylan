module: utils

define variable *debug* = #t;

define function debug(fmt :: <string>, #rest args) => ()
  if (*debug*)
    apply(format, *standard-error*, fmt, args);
    force-output(*standard-error*);
  end if;
end function debug;

define function force-format(s :: <stream>, fmt :: <string>, #rest args) => ()
  apply(format, s, fmt, args);
  force-output(s);
end function force-format;


