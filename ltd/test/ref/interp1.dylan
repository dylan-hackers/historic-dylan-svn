//  -*- Mode: Lisp; Syntax: Common-Lisp; -*-
//  Code from Paradigms of Artificial Intelligence Programming
//  Copyright (c) 1991 Peter Norvig
//  File interp1.lisp: simple Scheme interpreter, including macros.
define method interp (x, #key env)
  // Interpret (evaluate) the expression x in the environment env.
  if (instance?(x, <symbol>))
    get-var(x, env);
  elseif (not(instance?(x, <list>)))
    x;
  else
    select (first(x))
      #"quote"
         => second(x);
      #"begin"
         => last1(map(method (y) interp(y, env); end method, tail(x)));
      #"set!"
         => set-var!(second(x), interp(third(x), env), env);
      #"if"
         => if (interp(second(x), env))
              interp(third(x), env);
            else
              interp(x[3], env);
            end if;
      #"lambda"
         => let parms = second(x);
             let code = maybe-add(#"begin", rest2(x));
             method (#rest args)
               interp(code, extend-env(parms, args, env));
             end method;
      otherwise
         => //  a procedure application
             apply(interp(first(x), env),
                   map(method (v) interp(v, env); end method, tail(x)));
    end select;
  end if;
end method interp;

define method set-var! (var, val, env)
  // Set a variable to a value, in the given or global environment.
  if (cl-assoc(var, env))
    second(cl-assoc(var, env)) := val;
  else
    set-global-var!(var, val);
  end if;
  val;
end method set-var!;

define method get-var (var, env)
  // Get the value of a variable, from the given or global environment.
  if (cl-assoc(var, env))
    second(cl-assoc(var, env));
  else
    get-global-var(var);
  end if;
end method get-var;

define method set-global-var! (var, val)
  symbol-get-property(var, #"global-val") := val;
end method set-global-var!;

define method get-global-var (var)
  let default = "unbound";
  let val = symbol-get-property(var, #"global-val", default);
  if (val == default)
    error("Unbound scheme variable: %S", var);
  else
    val;
  end if;
end method get-global-var;

define method extend-env (vars, vals, env)
  // Add some variables and values to an environment.
  concatenate!(map(list, vars, vals), env);
end method extend-env;

define variable *scheme-procs* =
  #(#"+", #"-", #"*", #"/", #"=", #"<", #">", #"<=", #">=", #"cons", #"car",
    #"cdr", #"not", #"append", #"list", #"read", #"member",
    #(#"null?", #"null"), #(#"eq?", #"eq"), #(#"equal?", #"equal"),
    #(#"eqv?", #"eql"), #(#"write", #"prin1"), #(#"display", #"princ"),
    #(#"newline", #"terpri"));

define method init-scheme-interp ()
  // Initialize the scheme interpreter with some global variables.
  //  Define Scheme procedures as CL functions:
  begin do(init-scheme-proc, *scheme-procs*); *scheme-procs*; end;
  //  Define the boolean `constants'. Unfortunately, this won't 
  //  stop someone from saying: (set! t nil)
  set-global-var!(#t, #t);
  set-global-var!(#f, #f);
end method init-scheme-interp;

define method init-scheme-proc (f)
  // Define a Scheme procedure as a corresponding CL function.
  if (instance?(f, <list>))
    set-global-var!(first(f), second(f));
  else
    set-global-var!(f, f);
  end if;
end method init-scheme-proc;

define method scheme ()
  // A Scheme read-eval-print loop (using interp)
  init-scheme-interp();
  while (#t)
    format-out("\n==> ");
    print(interp(// LTD: Function READ not yet implemented.
                 read(),
                 #f),
          *standard-output*);
  end while;
end method scheme;

//  The following version handles macros:
define method interp (x, #key env)
  // Interpret (evaluate) the expression x in the environment env.
  //   This version handles macros.
  if (instance?(x, <symbol>))
    get-var(x, env);
  elseif (not(instance?(x, <list>)))
    x;
  elseif (scheme-macro(first(x)))
    // ***
    interp(scheme-macro-expand(x), env);
    // ***
    else
    select (first(x))
      #"quote"
         => second(x);
      #"begin"
         => last1(map(method (y) interp(y, env); end method, tail(x)));
      #"set!"
         => set-var!(second(x), interp(third(x), env), env);
      #"if"
         => if (interp(second(x), env))
              interp(third(x), env);
            else
              interp(x[3], env);
            end if;
      #"lambda"
         => let parms = second(x);
             let code = maybe-add(#"begin", rest2(x));
             method (#rest args)
               interp(code, extend-env(parms, args, env));
             end method;
      otherwise
         => //  a procedure application
             apply(interp(first(x), env),
                   map(method (v) interp(v, env); end method, tail(x)));
    end select;
  end if;
end method interp;

//  ==============================
define method scheme-macro (symbol)
  instance?(symbol, <symbol>) & symbol-get-property(symbol, #"scheme-macro");
end method scheme-macro;

// LTD: No macros.
#"def-scheme-macro";

define method scheme-macro-expand (x)
  // Macro-expand this Scheme expression.
  if (instance?(x, <list>) & scheme-macro(first(x)))
    scheme-macro-expand(apply(scheme-macro(first(x)), tail(x)));
  else
    x;
  end if;
end method scheme-macro-expand;

//  ==============================
def-scheme-macro(let, bindings(&rest, body),
                 pair(apply(list, #"lambda", map(first, bindings), body),
                      map(second, bindings)));

def-scheme-macro(let*, bindings(&rest, body),
                 if (empty?(bindings))
                   pair(#"begin", body);
                 else
                   list(#"let", list(first(bindings)),
                        apply(list, #"let*", tail(bindings), body));
                 end if);

def-scheme-macro(and, &rest(args),
                 if (empty?(args))
                   #"t";
                 elseif (length=1(args))
                   first(args);
                 else
                   list(#"if", first(args), pair(#"and", tail(args)));
                 end if);

def-scheme-macro(or, &rest(args),
                 if (empty?(args))
                   #();
                 elseif (length=1(args))
                   first(args);
                 else
                   begin
                     let var = generate-symbol();
                     list(#"let", list(list(var, first(args))),
                          list(#"if", var, var, pair(#"or", tail(args))));
                   end;
                 end if);

def-scheme-macro(cond, &rest(clauses),
                 if (empty?(clauses))
                   #f;
                 elseif (length=1(first(clauses)))
                   list(#"or", first(clauses), pair(#"cond", tail(clauses)));
                 elseif (starts-with(first(clauses), #"else"))
                   pair(#"begin", tail(first(clauses)));
                 else
                   list(#"if", first(first(clauses)),
                        pair(#"begin", tail(first(clauses))),
                        pair(#"cond", tail(clauses)));
                 end if);

def-scheme-macro(case, key(&rest, clauses),
                 begin
                   let key-val = generate-symbol(#"string"("KEY"));
                   list(#"let", list(list(key-val, key)),
                        pair(#"cond",
                             map(method (clause)
                                   if (starts-with(clause, #"else"))
                                     clause;
                                   else
                                     pair(list(#"member",
                                               key-val,
                                               list(#"quote", first(clause))),
                                          tail(clause));
                                   end if;
                                 end method,
                                 clauses)));
                 end);

def-scheme-macro(define, name(&rest, body),
                 if (not(instance?(name, <list>)))
                   list(#"begin", apply(list, #"set!", name, body),
                        list(#"quote", name));
                 else
                   list(#"define", first(name),
                        apply(list, #"lambda", tail(name), body));
                 end if);

def-scheme-macro(delay, computation(), list(#"lambda", #"()", computation));

def-scheme-macro(letrec, bindings(&rest, body),
                 apply(list, #"let",
                       map(method (v) list(first(v), #f); end method,
                           bindings),
                       concatenate(map(method (v)
                                       pair(#"set!", v);
                                       end method,
                                       bindings),
                                   body)));

