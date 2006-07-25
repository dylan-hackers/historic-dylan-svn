//  -*- Mode: Lisp; Syntax: Common-Lisp; -*-
//  Code from Paradigms of Artificial Intelligence Programming
//  Copyright (c) 1991 Peter Norvig
//  File interp2.lisp: Tail-recursive Scheme interpreter.
requires("interp1");

define method interp (x, #key env)
  // Evaluate the expression x in the environment env.
  //   This version is properly tail-recursive.
  block (return)
    local method go-interp ()
            return(if (instance?(x, <symbol>))
                     get-var(x, env);
                   elseif (not(instance?(x, <list>)))
                     x;
                   elseif (scheme-macro(first(x)))
                     x := scheme-macro-expand(x);
                     go-interp();
                   else
                     select (first(x))
                       #"quote"
                          => second(x);
                       #"begin"
                          => pop!(x);
                              for (while tail(x))
                                interp(pop!(x), env);
                              end for;
                              x := first(x);
                              go-interp();
                       #"set!"
                          => set-var!(second(x), interp(third(x), env), env);
                       #"if"
                          => x
                              := if (interp(second(x), env))
                                   third(x);
                                 else
                                   x[3];
                                 end if;
                              go-interp();
                       #"lambda"
                          => make-proc(env: env,
                                       parms: second(x),
                                       code: maybe-add(#"begin", rest2(x)));
                       otherwise
                          => let proc = interp(first(x), env);
                              let args
                                  = map(method (v) interp(v, env); end method,
                                        tail(x));
                              if (proc-p(proc))
                                x := proc-code(proc);
                                env
                                 := extend-env(proc-parms(proc),
                                               args,
                                               proc-env(proc));
                                go-interp();
                              else
                                apply(proc, args);
                              end if;
                     end select;
                   end if);
          end method go-interp;
    go-interp();
  end block;
end method interp;

// Represent a Scheme procedure
define class <proc> (<object>)
  slot proc-code, init-keyword: #"proc-code";
  slot proc-env = #f, init-keyword: #"proc-env";
  slot proc-name = #f, init-keyword: #"proc-name";
  slot proc-parms = #f, init-keyword: #"proc-parms";
end class <proc>;

define method print-proc (proc, #key stream = *standard-output*, depth)
  format(stream, "{%S}", proc.proc-name | #"??");
end method print-proc;

