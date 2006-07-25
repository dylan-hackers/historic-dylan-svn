//  -*- Mode: Lisp; Syntax: Common-Lisp -*-
//  Code from Paradigms of AI Programming
//  Copyright (c) 1991 Peter Norvig
//  File cmacsyma.lisp: Canonical Form version of Macsyma.
//  Bug Fix by dst, Dave_Touretzky@CS.CMU.EDU
requires("patmatch", "eliza-pm");

//  rule and expression definitions from "student.lisp"
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

//  from original Macsyma:
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

pat-match-abbrev(#"x+", #(#"?+", #"x"));

pat-match-abbrev(#"y+", #(#"?+", #"y"));

// A list of rules, ordered by precedence.
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

#f;

// LTD: Can't handle complex deftypes.
#f;

// LTD: No setf macros.
#"main-var";

// LTD: No setf macros.
#"coef";

define method main-var (p) p[0]; end method main-var;

define method coef (p, i) p[i + 1]; end method coef;

define method degree (p) size(p) - 2; end method degree;

define method poly (x, #rest coefs)
  // Make a polynomial with main variable x 
  //   and coefficients in increasing order.
  apply(vector, x, coefs);
end method poly;

define method make-poly (x, degree)
  // Make the polynomial 0 + 0*x + 0*x^2 + ... 0*x^degree
  let p = make(<array>, dimensions: degree + 2);
  main-var(p) := x;
  p;
end method make-poly;

define method prefix->canon (x)
  // Convert a prefix Lisp expression to canonical form.
  //   Exs: (+ (^ x 2) (* 3 x)) => #(x 0 3 1)
  //        (- (* (- x 1) (+ x 1)) (- (^ x 2) 1)) => 0
  if (instance?(x, <number>))
    x;
  elseif (instance?(x, <symbol>))
    poly(x, 0, 1);
  elseif (exp-p(x) & symbol-get-property(x.exp-op, #"prefix->canon"))
    apply(symbol-get-property(x.exp-op, #"prefix->canon"),
          map(prefix->canon, exp-args(x)));
  else
    error("Not a polynomial: %S", x);
  end if;
end method prefix->canon;

for (item in #(#(#"+", #"poly+"), #(#"-", #"poly-"), #(#"*", #"poly*poly"),
               #(#"^", #"poly^n"), #(#"d", #"deriv-poly")))
  symbol-get-property(first(item), #"prefix->canon") := second(item);
end for;

define method poly+ (#rest args)
  // Unary or binary polynomial addition.
  select (length(args))
    1
       => first(args);
    2
       => poly+poly(first(args), second(args));
  end select;
end method poly+;

define method poly- (#rest args)
  // Unary or binary polynomial subtraction.
  select (length(args))
    0
       => 0;
    1
       => poly*poly(-1, first(args));
    2
       => poly+poly(first(args), poly*poly(-1, second(args)));
  end select;
end method poly-;

define method var= (x, y) x == y; end method var=;

define method var> (x, y) x > y; end method var>;

define method poly+poly (p, q)
  // Add two polynomials.
  normalize-poly(if (instance?(p, <number>))
                   k+poly(p, q);
                 elseif (instance?(q, <number>))
                   k+poly(q, p);
                 elseif (var=(main-var(p), main-var(q)))
                   poly+same(p, q);
                 elseif (var>(main-var(q), main-var(p)))
                   k+poly(q, p);
                 else
                   k+poly(p, q);
                 end if);
end method poly+poly;

define method k+poly (k, p)
  // Add a constant k to a polynomial p.
  if (k == 0)
    p;
    //  0 + p = p
    elseif (instance?(k, <number>) & instance?(p, <number>))
    k + p;
    //  Add numbers
    else
    begin
      let r = copy-poly(p);
      //  Add k to x^0 term of p
      coef(r, 0) := poly+poly(coef(r, 0), k);
      r;
    end;
  end if;
end method k+poly;

define method poly+same (p, q)
  // Add two polynomials with the same main variable.
  //  First assure that q is the higher degree polynomial
  if (degree(p) > degree(q))
    poly+same(q, p);
  else
    let r = copy-poly(q);
    for (i from 0 to degree(p))
      coef(r, i) := poly+poly(coef(r, i), coef(p, i));
    end for;
    r;
  end if;
end method poly+same;

define method copy-poly (p)
  // Make a copy a polynomial.
  copy-sequence(p);
end method copy-poly;

define method poly*poly (p, q)
  // Multiply two polynomials.
  normalize-poly(if (instance?(p, <number>))
                   k*poly(p, q);
                 elseif (instance?(q, <number>))
                   k*poly(q, p);
                 elseif (var=(main-var(p), main-var(q)))
                   poly*same(p, q);
                 elseif (var>(main-var(q), main-var(p)))
                   k*poly(q, p);
                 else
                   k*poly(p, q);
                 end if);
end method poly*poly;

define method k*poly (k, p)
  // Multiply a polynomial p by a constant factor k.
  if (k == 0)
    0;
    //  0 * p = 0
    elseif (k == 1)
    p;
    //  1 * p = p
    elseif (instance?(k, <number>) & instance?(p, <number>))
    k * p;
    //  Multiply numbers
    else
    //  Multiply each coefficient
    begin
      let r = make-poly(main-var(p), degree(p));
      //  Accumulate result in r;  r[i] = k*p[i]
      for (i from 0 to degree(p))
        coef(r, i) := poly*poly(k, coef(p, i));
      end for;
      r;
    end;
  end if;
end method k*poly;

define method poly*same (p, q)
  // Multiply two polynomials with the same variable.
  let r-degree = degree(p) + degree(q);
  let r = make-poly(main-var(p), r-degree);
  for (i from 0 to degree(p))
    if (~ (coef(p, i) == 0))
      for (j from 0 to degree(q))
        coef(r, i + j)
         := poly+poly(coef(r, i + j), poly*poly(coef(p, i), coef(q, j)));
      end for;
    end if;
  end for;
  r;
end method poly*same;

define method normalize-poly (p)
  // Alter a polynomial by dropping trailing zeros.
  if (instance?(p, <number>))
    p;
  else
    let p-degree = find-key(p, curry(\==, 0)) - 1;
    if (p-degree <= 0)
      normalize-poly(coef(p, 0));
    elseif (p-degree < degree(p))
      remove!(copy-subsequence(p, start: p-degree), 0);
    else
      p;
    end if;
  end if;
end method normalize-poly;

define method deriv-poly (p, x)
  // Return the derivative, dp/dx, of the polynomial p.
  //  If p is a number or a polynomial with main-var > x,
  //  then p is free of x, and the derivative is zero;
  //  otherwise do real work.
  //  But first, make sure X is a simple variable,
  //  of the form #(X 0 1).
  assert(instance?(x, <polynomial>) & degree(x) = 1 & coef(x, 0) == 0
          & coef(x, 1) == 1);
  if (instance?(p, <number>))
    0;
  elseif (var>(main-var(p), main-var(x)))
    0;
  elseif (var=(main-var(p), main-var(x)))
    //  d(a + bx + cx^2 + dx^3)/dx = b + 2cx + 3dx^2
    //  So, shift the sequence p over by 1, then
    //  put x back in, and multiply by the exponents
    begin
      let r = copy-sequence(p, 1);
      main-var(r) := main-var(x);
      for (i from 1 to degree(r))
        coef(r, i) := poly*poly(i + 1, coef(r, i));
      end for;
      normalize-poly(r);
    end;
  else
    //  Otherwise some coefficient may contain x.  Ex:
    //  d(z + 3x + 3zx^2 + z^2x^3)/dz
    //  = 1 +  0 +  3x^2 +  2zx^3
    //  So copy p, and differentiate the coefficients.
    begin
      let r = copy-poly(p);
      for (i from 0 to degree(p))
        coef(r, i) := deriv-poly(coef(r, i), x);
      end for;
      normalize-poly(r);
    end;
  end if;
end method deriv-poly;

define method prefix->infix (exp)
  // Translate prefix to infix expressions.
  //   Handles operators with any number of args.
  if (not(instance?(exp, <list>)))
    exp;
  else
    intersperse(exp.exp-op, map(prefix->infix, exp-args(exp)));
  end if;
end method prefix->infix;

define method intersperse (op, args)
  // Place op between each element of args.
  //   Ex: (intersperse '+ '(a b c)) => '(a + b + c)
  if (length=1(args))
    first(args);
  else
    tail(begin
           let _acc = make(<deque>);
           for (arg in args)
             push-last(_acc, op);
             push-last(_acc, arg);
           finally
             _acc;
           end for;
         end);
  end if;
end method intersperse;

define method canon->prefix (p)
  // Convert a canonical polynomial to a lisp expression.
  if (instance?(p, <number>))
    p;
  else
    args->prefix(#"+", 0,
                 begin
                   let _acc = make(<deque>);
                   for (i from degree(p) to 0)
                     push-last(_acc,
                               args->prefix(#"*",
                                            1,
                                            list(canon->prefix(coef(p, i)),
                                                 exponent->prefix(main-var(p),
                                                                  i))));
                   finally
                     _acc;
                   end for;
                 end);
  end if;
end method canon->prefix;

define method exponent->prefix (base, exponent)
  // Convert canonical base^exponent to prefix form.
  select (exponent)
    0
       => 1;
    1
       => base;
    otherwise
       => bq-list(#"^", base, exponent);
  end select;
end method exponent->prefix;

define method args->prefix (op, identity, args)
  // Convert arg1 op arg2 op ... to prefix form.
  let useful-args = remove(args, identity);
  if (empty?(useful-args))
    identity;
  elseif (op == #"*" & member?(0, args))
    0;
  elseif (length=1(args))
    first(useful-args);
  else
    pair(op,
         mappend(method (exp)
                   if (starts-with(exp, op))
                     exp-args(exp);
                   else
                     list(exp);
                   end if;
                 end method,
                 useful-args));
  end if;
end method args->prefix;

define method canon (infix-exp)
  // Canonicalize argument and convert it back to infix
  prefix->infix(canon->prefix(prefix->canon(infix->prefix(infix-exp))));
end method canon;

define method canon-simplifier ()
  // Read an expression, canonicalize it, and print the result.
  while (#t)
    print(#"canon>", *standard-output*);
    print(canon(// LTD: Function READ not yet implemented.
                read()),
          *standard-output*);
  end while;
end method canon-simplifier;

define method poly^n (p, n)
  // Raise polynomial p to the nth power, n>=0.
  //  Uses the binomial theorem
  check-type(n, integer(0, \*));
  if (n = 0)
    1;
  elseif (instance?(p, <integer>))
    p ^ n;
  else
    //  First: split the polynomial p = a + b, where
    //  a = k*x^d and b is the rest of p
    begin
      let a = make-poly(main-var(p), degree(p));
      let b = normalize-poly(copy-sequence(p, 0, size(p) - 1));
      let a^n = make(<array>, dimensions: n + 1);
      let b^n = make(<array>, dimensions: n + 1);
      let result = make-poly(main-var(p), degree(p) * n);
      coef(a, degree(p)) := coef(p, degree(p));
      //  Second: Compute powers of a^i and b^i for i up to n
      a^n[0] := 1;
      b^n[0] := 1;
      for (i from 1 to n)
        a^n[i] := poly*poly(a, a^n[i - 1]);
        b^n[i] := poly*poly(b, b^n[i - 1]);
      end for;
      let c = 1;
      //  c helps compute (n choose i) incrementally
      for (i from 0 to n)
        p-add-into!(result, c, poly*poly(a^n[i], b^n[n - i]));
        c := c * (n - i) / (i + 1);
      end for;
      normalize-poly(result);
    end;
  end if;
end method poly^n;

define method p-add-into! (result, c, p)
  // Destructively add c*p into result.
  if (instance?(p, <number>) | ~ var=(main-var(p), main-var(result)))
    coef(result, 0) := poly+poly(coef(result, 0), poly*poly(c, p));
  else
    for (i from 0 to degree(p))
      coef(result, i) := poly+poly(coef(result, i), poly*poly(c, coef(p, i)));
    end for;
  end if;
  result;
end method p-add-into!;

define method make-rat (numerator, denominator)
  // Build a rational: a quotient of two polynomials.
  if (instance?(denominator, <number>))
    k*poly(1 / denominator, numerator);
  else
    pair(numerator, denominator);
  end if;
end method make-rat;

define method rat-numerator (rat)
  // The numerator of a rational expression.
  select (rat by instance?)
    cons
       => head(rat);
    number
       => numerator(rat);
    #t
       => rat;
  end select;
end method rat-numerator;

define method rat-denominator (rat)
  // The denominator of a rational expression.
  select (rat by instance?)
    cons
       => tail(rat);
    number
       => denominator(rat);
    #t
       => 1;
  end select;
end method rat-denominator;

define method rat*rat (x, y)
  // Multiply rationals: a/b * c/d = a*c/b*d
  poly/poly(poly*poly(rat-numerator(x), rat-numerator(y)),
            poly*poly(rat-denominator(x), rat-denominator(y)));
end method rat*rat;

define method rat+rat (x, y)
  // Add rationals: a/b + c/d = (a*d + c*b)/b*d
  let a = rat-numerator(x);
  let b = rat-denominator(x);
  let c = rat-numerator(y);
  let d = rat-denominator(y);
  poly/poly(poly+poly(poly*poly(a, d), poly*poly(c, b)), poly*poly(b, d));
end method rat+rat;

define method rat/rat (x, y)
  // Divide rationals: a/b / c/d = a*d/b*c
  rat*rat(x, make-rat(rat-denominator(y), rat-numerator(y)));
end method rat/rat;

