//  -*- Mode: Lisp; Syntax: Common-Lisp; -*-
//  Code from Paradigms of Artificial Intelligence Programming
//  Copyright (c) 1991 Peter Norvig
//  File compile2.lisp: Scheme compiler with tail recursion
//  and some optimizations and primitive instructions.
requires("interp1");

//  Uses the Scheme macro facility
define method comp (x, env, val?, more?)
  // Compile the expression x into a list of instructions
  if (member?(x, #(#"t", #())))
    comp-const(x, val?, more?);
  elseif (instance?(x, <symbol>))
    comp-var(x, env, val?, more?);
  elseif (not(instance?(x, <list>)))
    comp-const(x, val?, more?);
  elseif (scheme-macro(first(x)))
    comp(scheme-macro-expand(x), env, val?, more?);
  else
    select (first(x))
      #"quote"
         => arg-count(x, 1); comp-const(second(x), val?, more?);
      #"begin"
         => comp-begin(tail(x), env, val?, more?);
      #"set!"
         => arg-count(x, 2);
             assert(instance?(second(x), <symbol>));
             seq(comp(third(x), env, #t, #t), gen-set(second(x), env),
                 if (~ val?) gen(#"pop"); end if,
                 if (~ more?) gen(#"return"); end if);
      #"if"
         => arg-count(x, 2, 3);
             comp-if(second(x), third(x), x[3], env, val?, more?);
      #"lambda"
         => if (val?)
              let f = comp-lambda(second(x), rest2(x), env);
              seq(gen(#"fn", f), if (~ more?) gen(#"return"); end if);
            end if;
      otherwise
         => comp-funcall(first(x), tail(x), env, val?, more?);
    end select;
  end if;
end method comp;

//  ==============================
define method arg-count (form, min, #key max = min)
  // Report an error if form has wrong number of args.
  let n-args = size(tail(form));
  assert(min <= n-args & n-args <= max);
end method arg-count;

//  ==============================
define method comp-begin (exps, env, val?, more?)
  // Compile a sequence of expressions,
  //   returning the last one as the value.
  if (empty?(exps))
    comp-const(#f, val?, more?);
  elseif (length=1(exps))
    comp(first(exps), env, val?, more?);
  else
    seq(comp(first(exps), env, #f, #t),
        comp-begin(tail(exps), env, val?, more?));
  end if;
end method comp-begin;

define method comp-list (exps, env)
  // Compile a list, leaving them all on the stack.
  if (empty?(exps))
    #f;
  else
    seq(comp(first(exps), env, #t, #t), comp-list(tail(exps), env));
  end if;
end method comp-list;

//  ==============================
define method comp-const (x, val?, more?)
  // Compile a constant expression.
  if (val?)
    seq(if (member?(x, #(#"t", #(), -1, 0, 1, 2)))
          gen(x);
        else
          gen(#"const", x);
        end if,
        if (~ more?) gen(#"return"); end if);
  end if;
end method comp-const;

define method comp-var (x, env, val?, more?)
  // Compile a variable reference.
  if (val?) seq(gen-var(x, env), if (~ more?) gen(#"return"); end if); end if;
end method comp-var;

//  ==============================
define method comp-if (pred, then, else, env, val?, more?)
  // Compile a conditional (IF) expression.
  if (empty?(pred))
    //  (if nil x y) ==> y
    comp(else, env, val?, more?);
  elseif (constant?(pred))
    //  (if t x y) ==> x
    comp(then, env, val?, more?);
  elseif (instance?(pred, <list>)
           & //  (if (not p) x y) ==> (if p y x)
          length=1(tail(pred))
           & primitive-p(first(pred), env, 1)
           & prim-opcode(primitive-p(first(pred), env, 1)) == #"not")
    comp-if(second(pred), else, then, env, val?, more?);
  else
    begin
      let pcode = comp(pred, env, #t, #t);
      let tcode = comp(then, env, val?, more?);
      let ecode = comp(else, env, val?, more?);
      if (tcode = ecode)
        //  (if p x x) ==> (begin p x)
        seq(comp(pred, env, #f, #t), ecode);
      elseif (empty?(tcode))
        //  (if p nil y) ==> p (TJUMP L2) y L2:
        begin
          let l2 = gen-label();
          seq(pcode, gen(#"tjump", l2), ecode, list(l2),
              if (~ more?) gen(#"return"); end if);
        end;
      elseif (empty?(ecode))
        //  (if p x) ==> p (FJUMP L1) x L1:
        begin
          let l1 = gen-label();
          seq(pcode, gen(#"fjump", l1), tcode, list(l1),
              if (~ more?) gen(#"return"); end if);
        end;
      else
        //  (if p x y) ==> p (FJUMP L1) x L1: y
        //  or p (FJUMP L1) x (JUMP L2) L1: y L2:
        begin
          let l1 = gen-label();
          let l2 = if (more?) gen-label(); end if;
          seq(pcode, gen(#"fjump", l1), tcode,
              if (more?) gen(#"jump", l2); end if, list(l1), ecode,
              if (more?) list(l2); end if);
        end;
      end if;
    end;
  end if;
end method comp-if;

//  ==============================
define method comp-funcall (f, args, env, val?, more?)
  // Compile an application of a function to arguments.
  let prim = primitive-p(f, env, size(args));
  if (prim)
    //  function compilable to a primitive instruction
    if (~ val? & ~ prim-side-effects(prim))
      //  Side-effect free primitive when value unused
      comp-begin(args, env, #f, more?);
    else
      //  Primitive with value or call needed
      seq(comp-list(args, env), gen(prim-opcode(prim)),
          if (~ val?) gen(#"pop"); end if,
          if (~ more?) gen(#"return"); end if);
    end if;
  elseif (starts-with(f, #"lambda") & empty?(second(f)))
    //  ((lambda () body)) => (begin body)
    assert(empty?(args));
    comp-begin(rest2(f), env, val?, more?);
  elseif (more?)
    //  Need to save the continuation point
    begin
      let k = gen-label(#"k");
      seq(gen(#"save", k), comp-list(args, env), comp(f, env, #t, #t),
          gen(#"callj", size(args)), list(k),
          if (~ val?) gen(#"pop"); end if);
    end;
  else
    //  function call as rename plus goto
    seq(comp-list(args, env), comp(f, env, #t, #t),
        gen(#"callj", size(args)));
  end if;
end method comp-funcall;

//  ==============================
define class <prim> (<object>)
  slot prim-symbol, init-keyword: #"prim-symbol";
  slot prim-n-args, init-keyword: #"prim-n-args";
  slot prim-opcode, init-keyword: #"prim-opcode";
  slot prim-always, init-keyword: #"prim-always";
  slot prim-side-effects, init-keyword: #"prim-side-effects";
end class <prim>;

//  Note change from book: some of the following primitive fns have had
//  trailing NIL fields made explicit, because some Lisp's will give
//  an error (rather than NIL), when asked to find the prim-side-effects
//  of a three-element list.
define variable *primitive-fns* =
  #(#(#"+", 2, #"+", #"true", #()), #(#"-", 2, #"-", #"true", #()),
    #(#"*", 2, #"*", #"true", #()), #(#"/", 2, #"/", #"true", #()),
    #(#"<", 2, #"<", #(), #()), #(#">", 2, #">", #(), #()),
    #(#"<=", 2, #"<=", #(), #()), #(#">=", 2, #">=", #(), #()),
    #(#"/=", 2, #"/=", #(), #()), #(#"=", 2, #"=", #(), #()),
    #(#"eq?", 2, #"eq", #(), #()), #(#"equal?", 2, #"equal", #(), #()),
    #(#"eqv?", 2, #"eql", #(), #()), #(#"not", 1, #"not", #(), #()),
    #(#"null?", 1, #"not", #(), #()), #(#"cons", 2, #"cons", #"true", #()),
    #(#"car", 1, #"car", #(), #()), #(#"cdr", 1, #"cdr", #(), #()),
    #(#"cadr", 1, #"cadr", #(), #()), #(#"list", 1, #"list1", #"true", #()),
    #(#"list", 2, #"list2", #"true", #()),
    #(#"list", 3, #"list3", #"true", #()), #(#"read", 0, #"read", #(), #"t"),
    #(#"write", 1, #"write", #(), #"t"),
    #(#"display", 1, #"display", #(), #"t"),
    #(#"newline", 0, #"newline", #(), #"t"),
    #(#"compiler", 1, #"compiler", #"t", #()),
    #(#"name!", 2, #"name!", #"true", #"t"),
    #(#"random", 1, #"random", #"true", #()));

define method primitive-p (f, env, n-args)
  // F is a primitive if it is in the table, and is not shadowed
  //   by something in the environment, and has the right number of args.
  ~ in-env-p(f, env)
   & cl-find(f, *primitive-fns*,
             test: method (f, prim)
                     (f == prim.prim-symbol & n-args = prim.prim-n-args);
                   end method);
end method primitive-p;

define method list1 (x) list(x); end method list1;

define method list2 (x, y) list(x, y); end method list2;

define method list3 (x, y, z) list(x, y, z); end method list3;

define method display (x) print(x, *standard-output*); end method display;

define method newline ()
  write-element(*standard-output*, '\n');
end method newline;

//  ==============================
define method gen-set (var, env)
  // Generate an instruction to set a variable to top-of-stack.
  let p = in-env-p(var, env);
  if (p)
    gen(#"lset", first(p), second(p), ";", var);
  elseif (cl-assoc(var, *primitive-fns*))
    error("Can't alter the constant %S", var);
  else
    gen(#"gset", var);
  end if;
end method gen-set;

//  ==============================
define method init-scheme-comp ()
  // Initialize the primitive functions.
  block (return)
    for (prim in *primitive-fns*)
      symbol-get-property(prim.prim-symbol, #"global-val")
       := new-fn(env: #f, name: prim.prim-symbol,
                 code: seq(gen(#"prim", prim.prim-symbol), gen(#"return")));
    end for;
  end block;
end method init-scheme-comp;

//  ==============================
define method comp-lambda (args, body, env)
  // Compile a lambda form into a closure with compiled code.
  new-fn(env: env, args: args,
         code: seq(gen-args(args, 0),
                   comp-begin(body, pair(make-true-list(args), env), #t,
                              #f)));
end method comp-lambda;

define method gen-args (args, n-so-far)
  // Generate an instruction to load the arguments.
  if (empty?(args))
    gen(#"args", n-so-far);
  elseif (instance?(args, <symbol>))
    gen(#"args.", n-so-far);
  elseif (instance?(args, <pair>) & instance?(first(args), <symbol>))
    gen-args(tail(args), n-so-far + 1);
  else
    error("Illegal argument list");
  end if;
end method gen-args;

define method make-true-list (dotted-list)
  // Convert a possibly dotted list into a true, non-dotted list.
  if (empty?(dotted-list))
    #f;
  elseif (not(instance?(dotted-list, <list>)))
    list(dotted-list);
  else
    pair(first(dotted-list), make-true-list(tail(dotted-list)));
  end if;
end method make-true-list;

define method new-fn (#key code, env, name, args)
  // Build a new function.
  assemble(make-fn(env: env, name: name, args: args, code: optimize(code)));
end method new-fn;

//  ==============================
define method optimize (code) code; end method optimize;

define method assemble (fn) fn; end method assemble;

