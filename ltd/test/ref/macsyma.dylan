//  -*- Mode: Lisp; Syntax: Common-Lisp -*-
//  Code from Paradigms of AI Programming
//  Copyright (c) 1991 Peter Norvig
//  File macsyma.lisp: The implementation of MACSYMA in Chapter 8
requires("patmatch");

//  From student.lisp:
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

//  Define x+ and y+ as a sequence:
pat-match-abbrev(#"x+", #(#"?+", #"x"));

pat-match-abbrev(#"y+", #(#"?+", #"y"));

//  Define n and m as numbers; s as a non-number:
pat-match-abbrev(#"n", #(#"?is", #"n", #"numberp"));

pat-match-abbrev(#"m", #(#"?is", #"m", #"numberp"));

pat-match-abbrev(#"s", #(#"?is", #"s", #"not-numberp"));

define variable *infix->prefix-rules* =
  map(expand-pat-match-abbrev,
      #(#(#(#"x+", #"=", #"y+"), #(#"=", #"x", #"y")),
        #(#(#"-", #"x+"), #(#"-", #"x")), #(#(#"+", #"x+"), #(#"+", #"x")),
        #(#(#"x+", #"+", #"y+"), #(#"+", #"x", #"y")),
        #(#(#"x+", #"-", #"y+"), #(#"-", #"x", #"y")),
        #(#(#"d", #"y+", #"/", #"d", #"x"), #(#"d", #"y", #"x")),
        // *** New rule
        #(#(#"int", #"y+", #"d", #"x"), #(#"int", #"y", #"x")), // *** New rule
        #(#(#"x+", #"*", #"y+"), #(#"*", #"x", #"y")),
        #(#(#"x+", #"/", #"y+"), #(#"/", #"x", #"y")),
        #(#(#"x+", #"^", #"y+"), #(#"^", #"x", #"y"))));

define method infix->prefix (exp)
  // Translate an infix expression into prefix notation.
  let _that = #f;
  if (not(instance?(exp, <list>)))
    exp;
  elseif (size(exp) = 1)
    infix->prefix(first(exp));
  elseif (_that
           := rule-based-translator(exp, *infix->prefix-rules*,
                                    rule-if: rule-pattern,
                                    rule-then: rule-response,
                                    action: method (bindings, response)
                                            replace-multiple-in-tree(map(method (pair)
                                                                         pair(first(pair),
                                                                              infix->prefix(tail(pair)));
                                                                         end method,
                                                                         bindings),
                                                                     response);
                                            end method))
    _that;
  elseif (instance?(first(exp), <symbol>))
    list(first(exp), infix->prefix(tail(exp)));
  else
    error("Illegal exp");
  end if;
end method infix->prefix;

define method variable-p (exp)
  // Variables are the symbols M through Z.
  //  put x,y,z first to find them a little faster
  member?(exp,
          #(#"x", #"y", #"z", #"m", #"n", #"o", #"p", #"q", #"r", #"s", #"t",
            #"u", #"v", #"w"));
end method variable-p;

define variable *simplification-rules* = #f;

// Rules are in file macsymar.lisp
define method ^ (x, y)
  // Exponentiation
  x ^ y;
end method ^;

define method simplifier ()
  // Read a mathematical expression, simplify it, and print the result.
  while (#t)
    print(#"simplifier>", *standard-output*);
    print(simp(// LTD: Function READ not yet implemented.
               read()),
          *standard-output*);
  end while;
end method simplifier;

define method simp (inf)
  prefix->infix(simplify(infix->prefix(inf)));
end method simp;

define method simplify (exp)
  // Simplify an expression by first simplifying its components.
  if (not(instance?(exp, <list>)))
    exp;
  else
    simplify-exp(map(simplify, exp));
  end if;
end method simplify;

//  simplify-exp is redefined below
// (defun simplify-exp (exp)
//   "Simplify using a rule, or by doing arithmetic."
//   (cond ((rule-based-translator exp *simplification-rules*
//            :rule-if #'exp-lhs :rule-then #'exp-rhs
//            :action #'(lambda (bindings response)
//                        (simplify (sublis bindings response)))))
//         ((evaluable exp) (eval exp))
//         (t exp)))
define method evaluable (exp)
  // Is this an arithmetic expression that can be evaluated?
  every?(method (x) instance?(x, <number>); end method, exp-args(exp))
   & (member?(exp.exp-op, #(#"+", #"-", #"*", #"/"))
       | (exp.exp-op == #"^" & instance?(second(exp-args(exp)), <integer>)));
end method evaluable;

define method not-numberp (x) ~ instance?(x, <number>); end method not-numberp;

define method simp-rule (rule)
  // Transform a rule into proper format.
  let exp = infix->prefix(rule);
  mkexp(expand-pat-match-abbrev(exp.exp-lhs), exp.exp-op, exp.exp-rhs);
end method simp-rule;

define method simp-fn (op)
  symbol-get-property(op, #"simp-fn");
end method simp-fn;

define method set-simp-fn (op, fn)
  symbol-get-property(op, #"simp-fn") := fn;
end method set-simp-fn;

define method simplify-exp (exp)
  // Simplify using a rule, or by doing arithmetic,
  //   or by using the simp function supplied for this operator.
  let _that = #f;
  if (_that := simplify-by-fn(exp))
    _that;
  elseif (_that
           := rule-based-translator(exp, *simplification-rules*,
                                    rule-if: exp-lhs, rule-then: exp-rhs,
                                    action: method (bindings, response)
                                            simplify(replace-multiple-in-tree(bindings,
                                                                              response));
                                            end method))
    _that;
  elseif (evaluable(exp))
    // LTD: Function EVAL not yet implemented.
    eval(exp);
  else
    exp;
  end if;
end method simplify-exp;

define method simplify-by-fn (exp)
  // If there is a simplification fn for this exp,
  //   and if applying it gives a non-null result,
  //   then simplify the result and return that.
  let fn = simp-fn(exp.exp-op);
  let result = if (fn) fn(exp); end if;
  if (empty?(result)) #f; else simplify(result); end if;
end method simplify-by-fn;

define method factorize (exp)
  // Return a list of the factors of exp^n,
  //   where each factor is of the form (^ y n).
  let factors = #f;
  let constant = 1;
  local method fac (x, n)
          if (instance?(x, <number>))
            constant := constant * x ^ n;
          elseif (starts-with(x, #"*"))
            fac(x.exp-lhs, n);
            fac(x.exp-rhs, n);
          elseif (starts-with(x, #"/"))
            fac(x.exp-lhs, n);
            fac(x.exp-rhs, - n);
          elseif (starts-with(x, #"-") & length=1(exp-args(x)))
            constant := - constant;
            fac(x.exp-lhs, n);
          elseif (starts-with(x, #"^") & instance?(x.exp-rhs, <number>))
            fac(x.exp-lhs, n * x.exp-rhs);
          else
            begin
              let factor = cl-find(x, factors, key: exp-lhs, test: \=);
              if (factor)
                inc!(factor.exp-rhs, n);
              else
                push!(list(#"^", x, n), factors);
              end if;
            end;
          end if;
        end method fac;
  //  Body of factorize:
  fac(exp, 1);
  select (constant)
    0
       => #(#(#"^", 0, 1));
    1
       => factors;
    otherwise
       => pair(apply(list, #"^", constant, #(1)), factors);
  end select;
end method factorize;

define method unfactorize (factors)
  // Convert a list of factors back into prefix form.
  if (empty?(factors))
    1;
  elseif (length=1(factors))
    first(factors);
  else
    list(#"*", first(factors), unfactorize(tail(factors)));
  end if;
end method unfactorize;

define method divide-factors (numer, denom)
  // Divide a list of factors by another, producing a third.
  let result = map(copy-sequence, numer);
  for (d in denom)
    let factor = cl-find(d.exp-lhs, result, key: exp-lhs, test: \=);
    if (factor)
      dec!(factor.exp-rhs, d.exp-rhs);
    else
      push!(list(#"^", d.exp-lhs, - d.exp-rhs), result);
    end if;
  end for;
  remove!(result, 0, test: method (x, y) x == y.exp-rhs; end method);
end method divide-factors;

define method free-of (exp, var)
  // True if expression has no occurrence of var.
  ~ find-anywhere(var, exp);
end method free-of;

define method find-anywhere (item, tree)
  // Does item occur anywhere in tree?  If so, return it.
  let _that = #f;
  if (item == tree)
    tree;
  elseif (not(instance?(tree, <list>)))
    #f;
  elseif (_that := find-anywhere(item, first(tree)))
    _that;
  elseif (_that := find-anywhere(item, tail(tree)))
    _that;
  end if;
end method find-anywhere;

define method integrate (exp, x)
  //  First try some trivial cases
  if (free-of(exp, x))
    apply(list, #"*", exp, #(#"x"));
    //  Int c dx = c*x
    elseif (starts-with(exp, #"+"))
    //  Int f + g  =
    apply(list, #"+", integrate(exp.exp-lhs, x),
          #(//    Int f + Int g
            #(#(#","), #"integrate", #(#"exp-rhs", #"exp"), #"x")));
  elseif (starts-with(exp, #"-"))
    select (length(exp-args(exp)))
      1
         => integrate(exp.exp-lhs, x);
      2
         => apply(list, #"-", integrate(exp.exp-lhs, x),
                  #(//  Int f - g  =
                    #(#(#","), #"integrate", #(#"exp-rhs", #"exp"), #"x")));
    end select;
    //  Int f - Int g
    //  Now move the constant factors to the left of the integral
    else
    let (const-factors, x-factors)
        = partition-if(method (factor) free-of(factor, x); end method,
                       factorize(exp));
    identity(// simplify
             apply(list, #"*", unfactorize(const-factors),
                   #(//  And try to integrate:
                     #(#(#","), #"cond", #(#(#"null", #"x-factors"), #"x"),
                       #(#(#"some",
                           #(#"function",
                             #(#"lambda", #(#"factor"),
                               #(#"deriv-divides", #"factor", #"x-factors",
                                 #"x"))),
                           #"x-factors")),
                       //  <other methods here>
                       #(#"t",
                         #(#"list", #(#"quote", #"int?"),
                           #(#"unfactorize", #"x-factors"), #"x"))))));
  end if;
end method integrate;

define method partition-if (pred, list)
  // Return 2 values: elements of list that satisfy pred,
  //   and elements that don't.
  let yes-list = #f;
  let no-list = #f;
  for (item in list)
    if (pred(item)) push!(item, yes-list); else push!(item, no-list); end if;
  end for;
  values(reverse!(yes-list), reverse!(no-list));
end method partition-if;

define method deriv-divides (factor, factors, x)
  assert(starts-with(factor, #"^"));
  let u = factor.exp-lhs;
  let n = factor.exp-rhs;
  let k = divide-factors(factors, factorize(list(#"*", factor, deriv(u, x))));
  if (free-of(k, x))
    //  Int k*u^n*du/dx dx = k*Int u^n du
    //                     = k*u^(n+1)/(n+1) for n/=1
    //                     = k*log(u) for n=1
    if (n = -1)
      list(#"*", unfactorize(k), list(#"log", u));
    else
      list(#"/", list(#"*", unfactorize(k), list(#"^", u, n + 1)), n + 1);
    end if;
  elseif (n = 1 & in-integral-table?(u))
    //  Int y'*f(y) dx = Int f(y) dy
    begin
      let k2
          = divide-factors(factors,
                           factorize(list(#"*", u, deriv(u.exp-lhs, x))));
      if (free-of(k2, x))
        list(#"*", integrate-from-table(u.exp-op, u.exp-lhs),
             unfactorize(k2));
      end if;
    end;
  end if;
end method deriv-divides;

define method deriv (y, x) simplify(list(#"d", y, x)); end method deriv;

define method integration-table (rules)
  for (i-rule in rules)
    let rule = infix->prefix(i-rule);
    symbol-get-property(rule.exp-lhs.exp-lhs.exp-op, #"int") := rule;
  end for;
end method integration-table;

define method in-integral-table? (exp)
  exp-p(exp) & symbol-get-property(exp.exp-op, #"int");
end method in-integral-table?;

define method integrate-from-table (op, arg)
  let rule = symbol-get-property(op, #"int");
  replace-in-tree(arg, rule.exp-lhs.exp-lhs.exp-lhs, rule.exp-rhs);
end method integrate-from-table;

set-simp-fn(#"int",
            method (exp)
              unfactorize(factorize(integrate(exp.exp-lhs, exp.exp-rhs)));
            end method);

