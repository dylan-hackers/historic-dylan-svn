//  -*- Mode: Lisp; Syntax: Common-Lisp; -*-
//  Code from Paradigms of Artificial Intelligence Programming
//  Copyright (c) 1991 Peter Norvig
//  File compopt.lisp:  Optimizers for Scheme compiler (compile3.lisp).
def-optimizer(#"label"(), instr(code, all-code),
              //  ... L ... => ... ... ;if no reference to L
              if (~ cl-find(instr, all-code, key: arg1))
                begin
                  first(code) := second(code);
                  tail(code) := rest2(code);
                end;
                #t;
              end if);

def-optimizer(gset(lset), instr(code, all-code),
              //  ex: (begin (set! x y) (if x z))
              //  (SET X) (POP) (VAR X) ==> (SET X)
              if (is(second(code), #"pop")
                   & is(third(code), #(#"gvar", #"lvar"))
                   & arg1(instr) == arg1(third(code)))
                tail(code) := nth-tail(code, 3);
                #t;
              end if);

def-optimizer(jump(call, callj, return), instr(code, all-code),
              //  (JUMP L1) ...dead code... L2 ==> (JUMP L1) L2
              tail(code) := any?(label-p, tail(code)),
              //  (JUMP L1) ... L1 (JUMP L2) ==> (JUMP L2)  ... L1 (JUMP L2)
              if (is(instr, #"jump")
                   & is(target(instr, code), #(#"jump", #"return"))
                   & (first(code) := copy-sequence(target(instr, code)))
                   & #t)
                #f;
              end if);

def-optimizer(tjump(fjump), instr(code, all-code),
              //  (FJUMP L1) ... L1 (JUMP L2) ==> (FJUMP L2) ... L1 (JUMP L2)
              if (is(target(instr, code), #"jump"))
                second(instr) := arg1(target(instr, code));
                #t;
              end if);

def-optimizer(t(-1, 0, 1, 2), instr(code, all-code),
              select (opcode(second(code)))
                #"not"
                   => //  (T) (NOT) ==> NIL
                       begin
                         first(code) := gen1(#());
                         tail(code) := rest2(code);
                       end;
                       #t;
                #"fjump"
                   => //  (T) (FJUMP L) ... => ...
                       begin
                         first(code) := third(code);
                         tail(code) := rest3(code);
                       end;
                       #t;
                #"tjump"
                   => //  (T) (TJUMP L) ... => (JUMP L) ...
                       first(code) := gen1(#"jump", arg1(next-instr(code)));
                       #t;
                otherwise
                   => #f;
              end select);

def-optimizer(nil(), instr(code, all-code),
              select (opcode(second(code)))
                #"not"
                   => //  (NIL) (NOT) ==> T
                       begin
                         first(code) := gen1(#"t");
                         tail(code) := rest2(code);
                       end;
                       #t;
                #"tjump"
                   => //  (NIL) (TJUMP L) ... => ...
                       begin
                         first(code) := third(code);
                         tail(code) := rest3(code);
                       end;
                       #t;
                #"fjump"
                   => //  (NIL) (FJUMP L) ==> (JUMP L)
                       first(code) := gen1(#"jump", arg1(next-instr(code)));
                       #t;
                otherwise
                   => #f;
              end select);

