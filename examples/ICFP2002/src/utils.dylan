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

define method choose-one (pred? :: <function>, s :: <sequence>,
                          #key default = $unsupplied) => <object>;
  block(return)
    for (x in s)
      when (x.pred?)
        return(x)
      end when;
    finally
      if (default.unsupplied?)
        error("choose-one: no key satisfies the predicate!")
      else
        default
      end if;
    end for;
  end block;
end method choose-one;

