//  -*- Mode: Lisp; Syntax: Common-Lisp -*-
//  Code from Paradigms of AI Programming
//  Copyright (c) 1991 Peter Norvig
//  student.lisp: Chapter 7's STUDENT program to solve algebra word problems.
requires("patmatch");

define class <rule> (<object>)
  slot rule-pattern, init-keyword: #"rule-pattern";
  slot rule-response, init-keyword: #"rule-response";
end class <rule>;

define class <exp> (<object>)
  slot exp-op, init-keyword: #"exp-op";
  slot exp-lhs, init-keyword: #"exp-lhs";
  slot exp-rhs, init-keyword: #"exp-rhs";
end class <exp>;

define method exp-p (x) instance?(x, <pair>); end method exp-p;

define method exp-args (x) tail(x); end method exp-args;

pat-match-abbrev(#"?x*", #(#"?*", #"?x"));

pat-match-abbrev(#"?y*", #(#"?*", #"?y"));

define variable *student-rules* =
  map(expand-pat-match-abbrev,
      #(#(#(#"?x*", #"."), #"?x"),
        #(#(#"?x*", #".", #"?y*"), #(#"?x", #"?y")),
        #(#(#"if", #"?x*", #",", #"then", #"?y*"), #(#"?x", #"?y")),
        #(#(#"if", #"?x*", #"then", #"?y*"), #(#"?x", #"?y")),
        #(#(#"if", #"?x*", #",", #"?y*"), #(#"?x", #"?y")),
        #(#(#"?x*", #",", #"and", #"?y*"), #(#"?x", #"?y")),
        #(#(#"find", #"?x*", #"and", #"?y*"),
          #(#(#"=", #"to-find-1", #"?x"), #(#"=", #"to-find-2", #"?y"))),
        #(#(#"find", #"?x*"), #(#"=", #"to-find", #"?x")),
        #(#(#"?x*", #"equals", #"?y*"), #(#"=", #"?x", #"?y")),
        #(#(#"?x*", #"same", #"as", #"?y*"), #(#"=", #"?x", #"?y")),
        #(#(#"?x*", #"=", #"?y*"), #(#"=", #"?x", #"?y")),
        #(#(#"?x*", #"is", #"equal", #"to", #"?y*"), #(#"=", #"?x", #"?y")),
        #(#(#"?x*", #"is", #"?y*"), #(#"=", #"?x", #"?y")),
        #(#(#"?x*", #"-", #"?y*"), #(#"-", #"?x", #"?y")),
        #(#(#"?x*", #"minus", #"?y*"), #(#"-", #"?x", #"?y")),
        #(#(#"difference", #"between", #"?x*", #"and", #"?y*"),
          #(#"-", #"?y", #"?x")),
        #(#(#"difference", #"?x*", #"and", #"?y*"), #(#"-", #"?y", #"?x")),
        #(#(#"?x*", #"+", #"?y*"), #(#"+", #"?x", #"?y")),
        #(#(#"?x*", #"plus", #"?y*"), #(#"+", #"?x", #"?y")),
        #(#(#"sum", #"?x*", #"and", #"?y*"), #(#"+", #"?x", #"?y")),
        #(#(#"product", #"?x*", #"and", #"?y*"), #(#"*", #"?x", #"?y")),
        #(#(#"?x*", #"*", #"?y*"), #(#"*", #"?x", #"?y")),
        #(#(#"?x*", #"times", #"?y*"), #(#"*", #"?x", #"?y")),
        #(#(#"?x*", #"/", #"?y*"), #(#"/", #"?x", #"?y")),
        #(#(#"?x*", #"per", #"?y*"), #(#"/", #"?x", #"?y")),
        #(#(#"?x*", #"divided", #"by", #"?y*"), #(#"/", #"?x", #"?y")),
        #(#(#"half", #"?x*"), #(#"/", #"?x", 2)),
        #(#(#"one", #"half", #"?x*"), #(#"/", #"?x", 2)),
        #(#(#"twice", #"?x*"), #(#"*", 2, #"?x")),
        #(#(#"square", #"?x*"), #(#"*", #"?x", #"?x")),
        #(#(#"?x*", #"%", #"less", #"than", #"?y*"),
          #(#"*", #"?y", #(#"/", #(#"-", 100, #"?x"), 100))),
        #(#(#"?x*", #"%", #"more", #"than", #"?y*"),
          #(#"*", #"?y", #(#"/", #(#"+", 100, #"?x"), 100))),
        #(#(#"?x*", #"%", #"?y*"), #(#"*", #(#"/", #"?x", 100), #"?y"))));

define method student (words)
  // Solve certain Algebra Word Problems.
  solve-equations(create-list-of-equations(translate-to-expression(choose(complement(noise-word-p),
                                                                          words))));
end method student;

define method translate-to-expression (words)
  // Translate an English phrase into an equation or expression.
  rule-based-translator(words, *student-rules*, rule-if: rule-pattern,
                        rule-then: rule-response,
                        action: method (bindings, response)
                                  replace-multiple-in-tree(map(translate-pair,
                                                               bindings),
                                                           response);
                                end method)
   | make-variable(words);
end method translate-to-expression;

define method translate-pair (pair)
  // Translate the value part of the pair into an equation or expression.
  pair(binding-var(pair), translate-to-expression(binding-val(pair)));
end method translate-pair;

define method create-list-of-equations (exp)
  // Separate out equations embedded in nested parens.
  if (empty?(exp))
    #f;
  elseif (not(instance?(first(exp), <list>)))
    list(exp);
  else
    concatenate(create-list-of-equations(first(exp)),
                create-list-of-equations(tail(exp)));
  end if;
end method create-list-of-equations;

define method noise-word-p (word)
  // Is this a low-content word which can be safely ignored?
  member?(word, #(#"a", #"an", #"the", #"this", #"number", #"of", #"$"));
end method noise-word-p;

define method make-variable (words)
  // Create a variable name based on the given list of words
  first(words);
end method make-variable;

define method solve-equations (equations)
  // Print the equations and their solution
  print-equations("The equations to be solved are:", equations);
  print-equations("The solution is:", solve(equations, #f));
end method solve-equations;

define method solve (equations, known)
  // Solve a system of equations by constraint propagation.
  //  Try to solve for one equation, and substitute its value into 
  //  the others. If that doesn't work, return what is known.
  any?(method (equation)
         let x = one-unknown(equation);
         if (x)
           let answer = solve-arithmetic(isolate(equation, x));
           solve(replace-in-tree(answer.exp-rhs, answer.exp-lhs,
                                 remove(equations, equation)),
                 pair(answer, known));
         end if;
       end method,
       equations)
   | known;
end method solve;

define method isolate (e, x)
  // Isolate the lone x in e on the left hand side of e.
  //  This assumes there is exactly one x in e,
  //  and that e is an equation.
  if (e.exp-lhs == x)
    //  Case I: X = A -> X = n
    e;
  elseif (in-exp(x, e.exp-rhs))
    //  Case II: A = f(X) -> f(X) = A
    isolate(mkexp(e.exp-rhs, #"=", e.exp-lhs), x);
  elseif (in-exp(x, e.exp-lhs.exp-lhs))
    //  Case III: f(X)*A = B -> f(X) = B/A
    isolate(mkexp(e.exp-lhs.exp-lhs, #"=",
                  mkexp(e.exp-rhs, inverse-op(e.exp-lhs.exp-op),
                        e.exp-lhs.exp-rhs)),
            x);
  elseif (commutative-p(e.exp-lhs.exp-op))
    //  Case IV: A*f(X) = B -> f(X) = B/A
    isolate(mkexp(e.exp-lhs.exp-rhs, #"=",
                  mkexp(e.exp-rhs, inverse-op(e.exp-lhs.exp-op),
                        e.exp-lhs.exp-lhs)),
            x);
  else
    //  Case V: A/f(X) = B -> f(X) = A/B
    isolate(mkexp(e.exp-lhs.exp-rhs, #"=",
                  mkexp(e.exp-lhs.exp-lhs, e.exp-lhs.exp-op, e.exp-rhs)),
            x);
  end if;
end method isolate;

define method print-equations (header, equations)
  // Print a list of equations.
  (formatter-1("~%~a~{~%  ~{ ~a~}~}~%"))(#t,
                                         header,
                                         map(prefix->infix, equations));
end method print-equations;

define constant operators-and-inverses =
  #(#(#"+", #"-"), #(#"-", #"+"), #(#"*", #"/"), #(#"/", #"*"), #(#"=", #"="));

define method inverse-op (op)
  second(cl-assoc(op, operators-and-inverses));
end method inverse-op;

define method unknown-p (exp) instance?(exp, <symbol>); end method unknown-p;

define method in-exp (x, exp)
  // True if x appears anywhere in exp
  x == exp
   | (instance?(exp, <list>)
       & (in-exp(x, exp.exp-lhs) | in-exp(x, exp.exp-rhs)));
end method in-exp;

define method no-unknown (exp)
  // Returns true if there are no unknowns in exp.
  if (unknown-p(exp))
    #f;
  elseif (not(instance?(exp, <list>)))
    #t;
  elseif (no-unknown(exp.exp-lhs))
    no-unknown(exp.exp-rhs);
  else
    #f;
  end if;
end method no-unknown;

define method one-unknown (exp)
  // Returns the single unknown in exp, if there is exactly one.
  if (unknown-p(exp))
    exp;
  elseif (not(instance?(exp, <list>)))
    #f;
  elseif (no-unknown(exp.exp-lhs))
    one-unknown(exp.exp-rhs);
  elseif (no-unknown(exp.exp-rhs))
    one-unknown(exp.exp-lhs);
  else
    #f;
  end if;
end method one-unknown;

define method commutative-p (op)
  // Is operator commutative?
  member?(op, #(#"+", #"*", #"="));
end method commutative-p;

define method solve-arithmetic (equation)
  // Do the arithmetic for the right hand side.
  //  This assumes that the right hand side is in the right form.
  mkexp(equation.exp-lhs, #"=",
        // LTD: Function EVAL not yet implemented.
        eval(equation.exp-rhs));
end method solve-arithmetic;

define method binary-exp-p (x)
  exp-p(x) & size(exp-args(x)) = 2;
end method binary-exp-p;

define method prefix->infix (exp)
  // Translate prefix to infix expressions.
  if (not(instance?(exp, <list>)))
    exp;
  else
    map(prefix->infix,
        if (binary-exp-p(exp))
          list(exp.exp-lhs, exp.exp-op, exp.exp-rhs);
        else
          exp;
        end if);
  end if;
end method prefix->infix;

