//  -*- Mode: Lisp; Syntax: Common-Lisp; -*-
//  Code from Paradigms of Artificial Intelligence Programming
//  Copyright (c) 1991 Peter Norvig
//  File interp3.lisp: Scheme interpreter with explicit continuations
//  One bug fix by Cheng Lu Hsu, hsuc@cory.Berkeley.EDU
requires("interp1");

define method interp (x, env, cc)
  // Evaluate the expression x in the environment env,
  //   and pass the result to the continuation cc.
  if (instance?(x, <symbol>))
    cc(get-var(x, env));
  elseif (not(instance?(x, <list>)))
    cc(x);
  elseif (scheme-macro(first(x)))
    interp(scheme-macro-expand(x), env, cc);
  else
    select (first(x))
      #"quote"
         => cc(second(x));
      #"begin"
         => interp-begin(tail(x), env, cc);
      #"set!"
         => interp(third(x), env,
                   method (val)
                     cc(set-var!(second(x), val, env));
                   end method);
      #"if"
         => interp(second(x), env,
                   method (pred)
                     interp(if (pred) third(x); else x[3]; end if, env, cc);
                   end method);
      #"lambda"
         => let parms = second(x);
             let code = maybe-add(#"begin", rest2(x));
             cc(method (cont, #rest args)
                  interp(code, extend-env(parms, args, env), cont);
                end method);
      otherwise
         => interp-call(x, env, cc);
    end select;
  end if;
end method interp;

//  ==============================
define method scheme ()
  // A Scheme read-eval-print loop (using interp).
  //   Handles call/cc by explicitly passing continuations.
  init-scheme-interp();
  while (#t)
    format-out("\n==> ");
    interp(// LTD: Function READ not yet implemented.
           read(),
           #f, // LTD: Can't convert complex function PRINT.
           print);
  end while;
end method scheme;

define method interp-begin (body, env, cc)
  // Interpret each element of BODY, passing the last to CC.
  interp(first(body), env,
         method (val)
           if (empty?(tail(body)))
             cc(val);
           else
             //  fix, hsuc 2/20/93; forgot to call cc
             interp-begin(tail(body), env, cc);
           end if;
         end method);
end method interp-begin;

define method interp-call (call, env, cc)
  // Interpret the call (f x...) and pass the result to CC.
  map-interp(call, env,
             method (fn-and-args)
               apply(first(fn-and-args), cc, tail(fn-and-args));
             end method);
end method interp-call;

define method map-interp (list, env, cc)
  // Interpret each element of LIST, and pass the list to CC.
  if (empty?(list))
    cc(#f);
  else
    interp(first(list), env,
           method (x)
             map-interp(tail(list), env,
                        method (y) cc(pair(x, y)); end method);
           end method);
  end if;
end method map-interp;

//  ==============================
define method init-scheme-proc (f)
  // Define a Scheme primitive procedure as a CL function.
  if (instance?(f, <list>))
    set-global-var!(first(f),
                    method (cont, #rest args)
                      cont(apply(second(f), args));
                    end method);
  else
    init-scheme-proc(list(f, f));
  end if;
end method init-scheme-proc;

//  ==============================
define method call/cc (cc, computation)
  // Make the continuation accessible to a Scheme procedure.
  computation(cc, //  Package up CC into a Scheme function:
              method (cont, val) cc(val); end method);
end method call/cc;

//  Now install call/cc in the global environment
set-global-var!(#"call/cc", call/cc);

set-global-var!(#"call-with-current-continuation", call/cc);

