module: utils

define variable *debug* = #t;

define function debug(fmt :: <string>, #rest args) => ()
  if (*debug*)
    apply(format, *standard-error*, fmt, args);
    force-output(*standard-error*);
  end if;
end function debug;


