module: gettimeofday

define interface
  #include "sys/time.h",
    import: { "struct timeval", "gettimeofday", "struct timezone"},
    name-mapper: c-to-dylan;
end interface;

define method current-time() => (now :: <double-float>);
  let tv = make(<timeval>);
  let rc = gettimeofday(tv, as(<timezone>, 0));
  while(rc == -1) // Fscking Unix!!1!
    format-out("Error %= occurred!\n", errno());
    force-output(*standard-output*);
    rc := gettimeofday(tv, as(<timezone>, 0));
  end;
  as(<double-float>, tv);
end method current-time;

define method as(class == <double-float>, tv :: <timeval>) 
 => (d :: <double-float>);
  as(<double-float>, tv.get-tv-sec) +
    as(<double-float>, tv.get-tv-usec) / 1000000.0;
end method as;

define method as(class == <timeval>, d :: <double-float>)
 => (tv :: <timeval>);
  if(d < 0.0)
    error("Cannot convert negative time value to <timeval>");
  end if;
  let tv = make(<timeval>);
  let (seconds, rest) = floor(d);
  tv.get-tv-sec := seconds;
  tv.get-tv-usec := floor/(rest, 1000000.0);
  tv;
end method as;

define method errno() => (error :: <integer>);
  c-include("errno.h");
  c-expr(int:, "errno");
end method errno;


