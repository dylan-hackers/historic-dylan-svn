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

define function choose-one (pred? :: <function>, s :: <sequence>,
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
end function choose-one;

define function report-and-flush-error(error :: <error>)
 => ();
  force-output(*standard-output*);
  report-condition(error, *standard-error*);
  new-line(*standard-error*);
  force-output(*standard-error*);
end;

//

define class <cons> (<object>)
  constant slot car :: <object>,
    required-init-keyword: car:;
  constant slot cdr :: <object>,
    required-init-keyword: cdr:;
end class <cons>;

define method cons (car :: <object>, cdr :: <object>) => <cons>;
  make(<cons>, car: car, cdr: cdr);
end method cons;

define method \= (a1 :: <cons>, a2 :: <cons>) => (b :: <boolean>)
  a1.car = a2.car & a1.cdr = a2.cdr
end method;

define method equal-hash (a :: <cons>, s :: <hash-state>) =>
    (i :: <integer>, s* :: <hash-state>)
  values-hash(equal-hash, s, a.car, a.cdr)
end method equal-hash;

define method print-object (c :: <cons>, s :: <stream>) => ()
  format(s, "(cons %= . %=)", c.car, c.cdr);
end method print-object;

