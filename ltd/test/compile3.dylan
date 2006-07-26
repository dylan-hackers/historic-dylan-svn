//  -*- Mode: Lisp; Syntax: Common-Lisp; -*-
//  Code from Paradigms of Artificial Intelligence Programming
//  Copyright (c) 1991 Peter Norvig
//  File compile3.lisp: Scheme compiler with assembler
//  and peephole optimizer.  Also the abstract machine simulator.
//  After loading this file, load the optimizers in compopt.lisp.
//  Bug fixes by Erann Gat, gat@aig.Jpl.Nasa.Gov, November 1992
requires("interp1", "compile1", "compile2");

//  ==============================
define method opcode (instr)
  if (label-p(instr)) #"label"; else first(instr); end if;
end method opcode;

define method args (instr)
  if (instance?(instr, <list>)) tail(instr); end if;
end method args;

define method arg1 (instr)
  if (instance?(instr, <list>)) second(instr); end if;
end method arg1;

define method arg2 (instr)
  if (instance?(instr, <list>)) third(instr); end if;
end method arg2;

define method arg3 (instr)
  if (instance?(instr, <list>)) instr[3]; end if;
end method arg3;

// LTD: No setf macros.
#"arg1";

//  ==============================
define method assemble (fn)
  // Turn a list of instructions into a vector.
  let (length, labels) = asm-first-pass(fn.fn-code);
  fn.fn-code := asm-second-pass(fn.fn-code, length, labels);
  fn;
end method assemble;

define method asm-first-pass (code)
  // Return the labels and the total code length.
  let length = 0;
  let labels = #f;
  for (instr in code)
    if (label-p(instr))
      push!(pair(instr, length), labels);
    else
      inc!(length);
    end if;
  end for;
  values(length, labels);
end method asm-first-pass;

define method asm-second-pass (code, length, labels)
  // Put code into code-vector, adjusting for labels.
  let addr = 0;
  let code-vector = make(<array>, dimensions: length);
  for (instr in code)
    if (~ label-p(instr))
      if (is(instr, #(#"jump", #"tjump", #"fjump", #"save")))
        arg1(instr) := tail(cl-assoc(arg1(instr), labels));
      end if;
      code-vector[addr] := instr;
      inc!(addr);
    end if;
  end for;
  code-vector;
end method asm-second-pass;

//  ==============================
define method show-fn (fn, #key stream = *standard-output*, indent = 2)
  // Print all the instructions in a function.
  //   If the argument is not a function, just princ it, 
  //   but in a column at least 8 spaces wide.
  //  This version handles code that has been assembled into a vector
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
    for (i from 0 below size(fn.fn-code))
      let instr = fn.fn-code[i];
      if (label-p(instr))
        format(stream, "%S:", instr);
      else
        (method (s, #rest args)
           apply(maybe-initiate-xp-printing,
                 method (xp, #rest args)
                   begin
                     pprint-tab+(line: begin
                                       let _that = #f;
                                       if (_that := pop!(args))
                                       _that;
                                       else
                                       1;
                                       end if;
                                       end,
                                 1, xp);
                     using-format(xp, "~2d", pop!(args));
                     write-string++(": ", xp, 0, 2);
                   end;
                   if (args) copy-sequence(args); end if;
                 end method,
                 s, args);
         end method)(stream, indent, i);
        for (arg in instr) show-fn(arg, stream, indent + 8); end for;
        write-element(*standard-output*, '\n');
      end if;
    end for;
  end if;
end method show-fn;

//  ==============================
define class <ret-addr> (<object>)
  slot ret-addr-fn, init-keyword: #"ret-addr-fn";
  slot ret-addr-pc, init-keyword: #"ret-addr-pc";
  slot ret-addr-env, init-keyword: #"ret-addr-env";
end class <ret-addr>;

define method is (instr, op)
  // True if instr's opcode is OP, or one of OP when OP is a list.
  if (instance?(op, <list>))
    member?(opcode(instr), op);
  else
    opcode(instr) == op;
  end if;
end method is;

define method top (stack) first(stack); end method top;

define method machine (f)
  // Run the abstract machine on the code for f.
  let code = f.fn-code;
  let pc = 0;
  let env = #f;
  let stack = #f;
  let n-args = 0;
  let instr = #f;
  block (return)
    while (#t)
      instr := code[pc];
      inc!(pc);
      select (opcode(instr))
        #"lvar"
           => push!(env[arg1(instr)][arg2(instr)], stack);
        #"lset"
           => env[arg1(instr)][arg2(instr)] := top(stack);
        #"gvar"
           => push!(symbol-get-property(arg1(instr), #"global-val"), stack);
        #"gset"
           => symbol-get-property(arg1(instr), #"global-val") := top(stack);
        #"pop"
           => pop!(stack);
        #"const"
           => push!(arg1(instr), stack);
        #"jump"
           => pc := arg1(instr);
        #"fjump"
           => if (empty?(pop!(stack))) pc := arg1(instr); end if;
        #"tjump"
           => if (pop!(stack)) pc := arg1(instr); end if;
        #"save"
           => push!(make-ret-addr(pc: arg1(instr), fn: f, env: env), stack);
        #"return"
           => //  return value is top of stack; ret-addr is second
               begin
                 f := ret-addr-fn(second(stack));
                 code := f.fn-code;
                 env := ret-addr-env(second(stack));
                 pc := ret-addr-pc(second(stack));
               end;
               //  Get rid of the ret-addr, but keep the value
               stack := pair(first(stack), rest2(stack));
        #"callj"
           => pop!(env);
               //  discard the top frame
               begin
                 f := pop!(stack);
                 code := f.fn-code;
                 env := f.fn-env;
                 pc := 0;
                 n-args := arg1(instr);
               end;
        #"args"
           => assert(n-args = arg1(instr));
               push!(make(<array>, dimensions: arg1(instr)), env);
               for (i from n-args - 1 to 0)
                 first(env)[i] := pop!(stack);
               end for;
        #"args."
           => assert(n-args >= arg1(instr));
               push!(make(<array>, dimensions: 1 + arg1(instr)), env);
               for (% from 1 to n-args - arg1(instr))
                 push!(pop!(stack), first(env)[arg1(instr)]);
               end for;
               for (i from arg1(instr) - 1 to 0)
                 first(env)[i] := pop!(stack);
               end for;
        #"fn"
           => push!(make-fn(code: fn-code(arg1(instr)), env: env), stack);
        #"prim"
           => push!(apply(arg1(instr),
                          begin
                            let args = #f;
                            block (return)
                              for (% from 1 to n-args)
                                push!(pop!(stack), args);
                              finally
                                return(args);
                                #f;
                              end for;
                            end block;
                          end),
                    stack);
        #"set-cc"
           => stack := top(stack);
        #"cc"
           => push!(make-fn(env: list(vector(stack)),
                            code: #(#(#"args", 1),
                                    #(#"lvar", 1, 0, ";", #"stack"),
                                    #(#"set-cc"), #(#"lvar", 0, 0),
                                    #(#"return"))),
                    stack);
        (#"scheme-read", #"newline")
           => //  *** fix, gat, 11/9/92
               push!((opcode(instr))(), stack);
        (#"car", #"cdr", #"cadr", #"not", #"list1", #"compiler", #"display",
         #"write", #"random")
           => push!((opcode(instr))(pop!(stack)), stack);
        (#"+", #"-", #"*", #"/", #"<", #">", #"<=", #">=", #"/=", #"=",
         #"cons", #"list2", #"name!", #"eq", #"equal", #"eql")
           => stack
               := pair((opcode(instr))(second(stack), first(stack)),
                       rest2(stack));
        #"list3"
           => stack
               := pair((opcode(instr))(third(stack),
                                       second(stack),
                                       first(stack)),
                       rest3(stack));
        (#"t", #(), -1, 0, 1, 2)
           => push!(opcode(instr), stack);
        (#"halt")
           => return(top(stack));
        otherwise
           => error("Unknown opcode: %S", instr);
      end select;
    end while;
  end block;
end method machine;

define method init-scheme-comp ()
  // Initialize values (including call/cc) for the Scheme compiler.
  set-global-var!(#"exit",
                  new-fn(name: #"exit", args: #(#"val"),
                         code: #(#(#"halt"))));
  set-global-var!(#"call/cc",
                  new-fn(name: #"call/cc", args: #(#"f"),
                         code: #(#(#"args", 1), #(#"cc"),
                                 #(#"lvar", 0, 0, ";", #"f"),
                                 #(#"callj", 1))));
  //  *** Bug fix, gat, 11/9/92
  block (return)
    for (prim in *primitive-fns*)
      symbol-get-property(prim.prim-symbol, #"global-val")
       := new-fn(env: #f, name: prim.prim-symbol,
                 code: seq(gen(#"prim", prim.prim-symbol), gen(#"return")));
    end for;
  end block;
end method init-scheme-comp;

//  ==============================
define constant scheme-top-level =
  #(#"begin",
    #(#"define", #(#"scheme"), #(#"newline"), #(#"display", "=> "),
      #(#"write", #(#(#"compiler", #(#"read")))), #(#"scheme")),
    #(#"scheme"));

define method scheme ()
  // A compiled Scheme read-eval-print loop
  init-scheme-comp();
  machine(compiler(scheme-top-level));
end method scheme;

define method comp-go (exp)
  // Compile and execute the expression.
  machine(compiler(list(#"exit", exp)));
end method comp-go;

//  Peephole Optimizer
//  ==============================
define method optimize (code)
  // Perform peephole optimization on assembly code.
  let any-change = #f;
  //  Optimize each tail
  for (code-tail = code then tail(code-tail), until empty?(code-tail))
    any-change := optimize-1(code-tail, code) | any-change;
  end for;
  //  If any changes were made, call optimize again
  if (any-change) optimize(code); else code; end if;
end method optimize;

//  ==============================
define method optimize-1 (code, all-code)
  // Perform peephole optimization on a tail of the assembly code.
  //   If a change is made, return true.
  let instr = first(code);
  let optimizer = get-optimizer(opcode(instr));
  if (optimizer) optimizer(instr, code, all-code); end if;
end method optimize-1;

//  ==============================
begin
  let optimizers = make(<table>, test: \==);
  define method get-optimizer (opcode)
    // Get the assembly language optimizer for this opcode.
    optimizers[opcode];
  end method get-optimizer;
  define method put-optimizer (opcode, fn)
    // Store an assembly language optimizer for this opcode.
    optimizers[opcode] := fn;
  end method put-optimizer;
end;

//  ==============================
define method gen1 (#rest args)
  // Generate a single instruction
  args;
end method gen1;

define method target (instr, code)
  second(member?(arg1(instr), code));
end method target;

define method next-instr (code)
  cl-find-if(complement(label-p), code);
end method next-instr;

//  ==============================
// LTD: No macros.
#"def-optimizer";

//  Now for some additions and answers to exercises:
//  ==============================
define constant eof = "EoF";

define method eof-object? (x) x == eof; end method eof-object?;

define variable *scheme-readtable* =
  // LTD: Function COPY-READTABLE not yet implemented.
  copy-readtable();

define method scheme-read (#key stream = *standard-input*)
  fluid-bind (*readtable* = *scheme-readtable*)
    // LTD: Function READ not yet implemented.
    read(stream, #f, eof);
  end fluid-bind;
end method scheme-read;

//  ==============================
set-dispatch-macro-character('#', 't', method (#rest ignore) #t; end method,
                             *scheme-readtable*);

set-dispatch-macro-character('#', 'f', method (#rest ignore) #f; end method,
                             *scheme-readtable*);

set-dispatch-macro-character('#', 'd',
                             //  In both Common Lisp and Scheme,
                             //  #x, #o and #b are hexidecimal, octal, and binary,
                             //  e.g. #xff = #o377 = #b11111111 = 255
                             //  In Scheme only, #d255 is decimal 255.
                             method (stream, #rest ignore)
                               fluid-bind (*read-base* = 10)
                                 scheme-read(stream);
                               end fluid-bind;
                             end method,
                             *scheme-readtable*);

// LTD: Function SET-MACRO-CHARACTER not yet implemented.
set-macro-character('`',
                    method (s, ignore)
                      list(#"quasiquote", scheme-read(s));
                    end method,
                    #f, *scheme-readtable*);

// LTD: Function SET-MACRO-CHARACTER not yet implemented.
set-macro-character(',',
                    method (stream, ignore)
                      let ch = read-element(stream, nil);
                      if (ch = '@')
                        list(#"unquote-splicing",
                             // LTD: Function READ not yet implemented.
                             read(stream));
                      else
                        unread-element(stream, ch);
                        list(#"unquote",
                             // LTD: Function READ not yet implemented.
                             read(stream));
                      end if;
                    end method,
                    #f, *scheme-readtable*);

//  ==============================
define variable *primitive-fns* =
  #(#(#"+", 2, #"+", #"true"), #(#"-", 2, #"-", #"true"),
    #(#"*", 2, #"*", #"true"), #(#"/", 2, #"/", #"true"), #(#"<", 2, #"<"),
    #(#">", 2, #">"), #(#"<=", 2, #"<="), #(#">=", 2, #">="),
    #(#"/=", 2, #"/="), #(#"=", 2, #"="), #(#"eq?", 2, #"eq"),
    #(#"equal?", 2, #"equal"), #(#"eqv?", 2, #"eql"), #(#"not", 1, #"not"),
    #(#"null?", 1, #"not"), #(#"car", 1, #"car"), #(#"cdr", 1, #"cdr"),
    #(#"cadr", 1, #"cadr"), #(#"cons", 2, #"cons", #"true"),
    #(#"list", 1, #"list1", #"true"), #(#"list", 2, #"list2", #"true"),
    #(#"list", 3, #"list3", #"true"),
    #(#"read", 0, #"scheme-read", #(), #"t"),
    #(#"eof-object?", 1, #"eof-object?"), // ***
    #(#"write", 1, #"write", #(), #"t"),
    #(#"display", 1, #"display", #(), #"t"),
    #(#"newline", 0, #"newline", #(), #"t"),
    #(#"compiler", 1, #"compiler", #"t"),
    #(#"name!", 2, #"name!", #"true", #"t"),
    #(#"random", 1, #"random", #"true", #()));

//  ==============================
// (setf (scheme-macro 'quasiquote) 'quasi-q)
define method quasi-q (x)
  // Expand a quasiquote form into append, list, and cons calls.
  if (instance?(x, <vector>))
    list(#"apply", #"vector", quasi-q(as(<list>, x)));
  elseif (not(instance?(x, <list>)))
    if (constant?(x)) x; else list(#"quote", x); end if;
  elseif (starts-with(x, #"unquote"))
    assert(tail(x) & empty?(rest2(x)));
    second(x);
  elseif (starts-with(x, #"quasiquote"))
    assert(tail(x) & empty?(rest2(x)));
    quasi-q(quasi-q(second(x)));
  elseif (starts-with(first(x), #"unquote-splicing"))
    if (empty?(tail(x)))
      second(first(x));
    else
      list(#"append", second(first(x)), quasi-q(tail(x)));
    end if;
  else
    combine-quasiquote(quasi-q(head(x)), quasi-q(tail(x)), x);
  end if;
end method quasi-q;

define method combine-quasiquote (left, right, x)
  // Combine left and right (car and cdr), possibly re-using x.
  if (constant?(left) & constant?(right))
    if (// LTD: Function EVAL not yet implemented.
        eval(left)
         == first(x)
         & // LTD: Function EVAL not yet implemented.
           eval(right)
            == tail(x))
      list(#"quote", x);
    else
      list(#"quote",
           pair(// LTD: Function EVAL not yet implemented.
                eval(left),
                // LTD: Function EVAL not yet implemented.
                eval(right)));
    end if;
  elseif (empty?(right))
    list(#"list", left);
  elseif (starts-with(right, #"list"))
    apply(list, #"list", left, tail(right));
  else
    list(#"cons", left, right);
  end if;
end method combine-quasiquote;

//  ==============================
define method scheme-read (#key stream = *standard-input*)
  fluid-bind (*readtable* = *scheme-readtable*)
    convert-numbers(// LTD: Function READ not yet implemented.
                    read(stream, #f, eof));
  end fluid-bind;
end method scheme-read;

define method convert-numbers (x)
  // Replace symbols that look like Scheme numbers with their values.
  //  Don't copy structure, make changes in place.
  select (x by instance?)
    cons
       => head(x) := convert-numbers(head(x));
           tail(x) := convert-numbers(tail(x));
           x;
    //  *** Bug fix, gat, 11/9/92
    symbol
       => convert-number(x) | x;
    vector
       => for (i from 0 below size(x)) x[i] := convert-numbers(x[i]); end for;
           x;
    //  *** Bug fix, gat, 11/9/92
    #t
       => x;
  end select;
end method convert-numbers;

define method convert-number (symbol)
  // If str looks like a complex number, return the number.
  let str = as(<string>, symbol);
  let pos = find-key(str, sign-p);
  let end = size(str) - 1;
  if (pos & char-equal?(str[end], 'i'))
    let re
        = // LTD: Function READ-FROM-STRING not yet implemented.
          read-from-string(str, #f, #f, start: 0, end: pos);
    let im
        = // LTD: Function READ-FROM-STRING not yet implemented.
          read-from-string(str, #f, #f, start: pos, end: end);
    if (instance?(re, <number>) & instance?(im, <number>))
      // LTD: Function COMPLEX not yet implemented.
      complex(re, im);
    end if;
  end if;
end method convert-number;

define method sign-p (char) cl-find(char, "+-"); end method sign-p;

