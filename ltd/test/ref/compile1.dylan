//  -*- Mode: Lisp; Syntax: Common-Lisp; -*-
//  Code from Paradigms of Artificial Intelligence Programming
//  Copyright (c) 1991 Peter Norvig
//  File compile1.lisp: Simplest version of Scheme compiler
requires("interp1");

//  Uses the Scheme macro facility
define method comp (x, env)
  // Compile the expression x into a list of instructions
  if (instance?(x, <symbol>))
    gen-var(x, env);
  elseif (not(instance?(x, <list>)))
    gen(#"const", x);
  elseif (scheme-macro(first(x)))
    comp(scheme-macro-expand(x), env);
  else
    select (first(x))
      #"quote"
         => gen(#"const", second(x));
      #"begin"
         => comp-begin(tail(x), env);
      #"set!"
         => seq(comp(third(x), env), gen-set(second(x), env));
      #"if"
         => comp-if(second(x), third(x), x[3], env);
      #"lambda"
         => gen(#"fn", comp-lambda(second(x), tail(tail(x)), env));
      #"t"
         => seq(mappend(method (y) comp(y, env); end method, tail(x)),
                comp(first(x), env), gen(#"call", size(tail(x))));
    end select;
  end if;
end method comp;

//  ==============================
define method comp-begin (exps, env)
  // Compile a sequence of expressions, popping all but the last.
  if (empty?(exps))
    gen(#"const", #f);
  elseif (length=1(exps))
    comp(first(exps), env);
  else
    seq(comp(first(exps), env), gen(#"pop"), comp-begin(tail(exps), env));
  end if;
end method comp-begin;

//  ==============================
define method comp-if (pred, then, else, env)
  // Compile a conditional expression.
  let l1 = gen-label();
  let l2 = gen-label();
  seq(comp(pred, env), gen(#"fjump", l1), comp(then, env), gen(#"jump", l2),
      list(l1), comp(else, env), list(l2));
end method comp-if;

//  ==============================
define class <fn> (<object>)
  slot fn-code, init-keyword: #"fn-code";
  slot fn-env = #f, init-keyword: #"fn-env";
  slot fn-name = #f, init-keyword: #"fn-name";
  slot fn-args = #f, init-keyword: #"fn-args";
end class <fn>;

define method comp-lambda (args, body, env)
  // Compile a lambda form into a closure with compiled code.
  assert(instance?(args, <list>)
          & every?(method (x) instance?(x, <symbol>); end method, args));
  //  For now, no &rest parameters.  
  //  The next version will support Scheme's version of &rest
  make-fn(env: env, args: args,
          code: seq(gen(#"args", size(args)),
                    comp-begin(body, pair(args, env)), gen(#"return")));
end method comp-lambda;

//  ==============================
define variable *label-num* = 0;

define method compiler (x)
  // Compile an expression as if it were in a parameterless lambda.
  *label-num* := 0;
  comp-lambda(#"()", list(x), #f);
end method compiler;

define method comp-show (x)
  // Compile an expression and show the resulting code
  show-fn(compiler(x));
  values();
end method comp-show;

//  ==============================
define method gen (opcode, #rest args)
  // Return a one-element list of the specified instruction.
  list(pair(opcode, args));
end method gen;

define method seq (#rest code)
  // Return a sequence of instructions
  apply(concatenate, code);
end method seq;

define method gen-label (#key label = #"l")
  // Generate a label (a symbol of the form Lnnn)
  as(<symbol>, format(#f, "%S%d", label, inc!(*label-num*)));
end method gen-label;

//  ==============================
define method gen-var (var, env)
  // Generate an instruction to reference a variable's value.
  let p = in-env-p(var, env);
  if (p)
    gen(#"lvar", first(p), second(p), ";", var);
  else
    gen(#"gvar", var);
  end if;
end method gen-var;

define method gen-set (var, env)
  // Generate an instruction to set a variable to top-of-stack.
  let p = in-env-p(var, env);
  if (p)
    gen(#"lset", first(p), second(p), ";", var);
  else
    gen(#"gset", var);
  end if;
end method gen-set;

//  ==============================
def-scheme-macro(define, name(&rest, body),
                 if (not(instance?(name, <list>)))
                   list(#"name!", apply(list, #"set!", name, body),
                        list(#"quote", name));
                 else
                   scheme-macro-expand(list(#"define",
                                            first(name),
                                            apply(list,
                                                  #"lambda",
                                                  tail(name),
                                                  body)));
                 end if);

define method name! (fn, name)
  // Set the name field of fn, if it is an un-named fn.
  if (fn-p(fn) & empty?(fn.fn-name)) fn.fn-name := name; end if;
  name;
end method name!;

//  This should also go in init-scheme-interp:
set-global-var!(#"name!", name!);

define method print-fn (fn, #key stream = *standard-output*, depth)
  format(stream, "{%S}", fn.fn-name | #"??");
end method print-fn;

define method show-fn (fn, #key stream = *standard-output*, depth = 0)
  // Print all the instructions in a function.
  //   If the argument is not a function, just princ it, 
  //   but in a column at least 8 spaces wide.
  if (~ fn-p(fn))
    (method (s, #rest args)
       apply(maybe-initiate-xp-printing,
             method (xp, #rest args)
               using-format(xp, "~8a", pop!(args));
               if (args) copy-sequence(args); end if;
             end method,
             s, args);
     end method)(stream, fn);
  else
    write-element(*standard-output*, '\n');
    inc!(depth, 8);
    for (instr in fn.fn-code)
      if (label-p(instr))
        format(stream, "%S:", instr);
      else
        (method (s, #rest args)
           apply(maybe-initiate-xp-printing,
                 method (xp, #rest args)
                   pprint-tab+(line: begin
                                       let _that = #f;
                                       if (_that := pop!(args))
                                       _that;
                                       else
                                       1;
                                       end if;
                                     end,
                               1, xp);
                   if (args) copy-sequence(args); end if;
                 end method,
                 s, args);
         end method)(stream, depth);
        for (arg in instr) show-fn(arg, stream, depth); end for;
        write-element(*standard-output*, '\n');
      end if;
    end for;
  end if;
end method show-fn;

define method label-p (x)
  // Is x a label?
  not(instance?(x, <list>));
end method label-p;

define method in-env-p (symbol, env)
  // If symbol is in the environment, return its index numbers.
  let frame = cl-find(symbol, env, test: cl-find);
  if (frame)
    list(find-key(env, curry(\==, frame)),
         find-key(frame, curry(\==, symbol)));
  end if;
end method in-env-p;

