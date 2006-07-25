//  -*- Mode:Common-Lisp;  Base:10 -*-
//  Written by: Richard J. Fateman
//  File: diffrat.lisp
//  (c) 1991 Richard J. Fateman
//  This is a derivative-divides integration program
//  written to use rational forms. Much of the work is
//  in peripheral issues, like computing derivatives of
//  random functions and testing to see if expressions are
//  free of variables.  This program depends on simplification
//  to find out if expressions divide out evenly, and will lose
//  (fail to integrate), if it is required to know identities
//  that are algebraic or transcendental (e.g. sin^2+cos^2 =1).
//  Also, this program produces ANTIDERIVATIVES, not really integrals.
//  That is, the domain of the answer may not be the same as the domain
//  of integration (unstated, but lurking in most applications).
// (provide 'diffrat)
"(in-package mma)";

// (require "poly")(require "simp1")(require "rat1")
define method int (e, x) diffdiv(e, x); end method int;

// testing program
define method d (e, x) outof-rat(ratdiff(into-rat(e), x)); end method d;

//  Some useful utility programs
//  pfreevar returns t if the poly p is free of the variable numbered v.
define method pfreevar (p, v)
  local method pf1 (x)
          if (coefp(x))
            #t;
          else
            freevar(revtab[x[0]], revtab[v]) & every?(pf1, x);
          end if;
        end method pf1;
  pf1(p);
end method pfreevar;

//  freevar returns t if  x is free of the variable or kernel v.
//  x and v are in list representation. Only explicit dependencies
//  are discovered here.
define method freevar (x, v)
  local method f1 (x)
          if (x == v)
            #f;
            //  here could check for "x depends on v indirectly."
            elseif (instance?(x, <pair>))
            every?(f1, tail(x));
            //  could check for poly or rat stuff
            else
            #t;
          end if;
        end method f1;
  f1(x);
end method freevar;

//  see if a rational form r is free of a variable, in list form, v. v may
//  be inside a kernel as, say (Log v) or a regular variable.
//  As a particular use, we know that if rfreevar (r v) is t, 
//  then r is a constant wrt v, and integrate(r,v) = r*v {+ a const}.
define method rfreevar (r, v)
  let v = look-up-var(v);
  let rf1 = method (x) pfreevar(head(x), v); end method;
  every?(rf1, rat-numerator(r)) & every?(rf1, rat-denominator(r));
end method rfreevar;

//  (ratderiv r v) computes the partial derivative
//  of the rational expression r wrt to the list-form expression v,
//  which is presumed to be either an indeterminate, or perhaps
//  a kernel, in which case it should be simplified. The answer is
//  in rational form.
//  In an attempt to do this efficiently in rational form we consider
//  two cases:
//  (a) v is an indeterminate and the only kernel in r that
//  involves v is exactly v.  This is the "fast" case, in the sense
//  that almost all the arithmetic can be done on polynomials.  It is
//  a subset of case b, and if we had only one program to write, we'd
//  have to write case b.
//  (b) r includes kernels that depend on v in other ways.  For example,
//  (Log v) or (Sin v). All the arithmetic must be done in rational form
//  if the derivative is rational (not polynomial) in v. (Log v) is like
//  that.
//  This may seem like a long way to do it, but let's try, anyway.
//  collectvars returns a list of all the variable (numbers) in a
//  rational form r.  E.g. (collectvars #(3 #(1 1 1) 0 1)) -> (1 3)
define method collectvars (r)
  local method cv2 (pr)
          //  cv2 is applied to each pair: poly . power
          cv3(head(pr));
        end method cv2,
        method cv3 (p)
          //  cv3 is applied to each polynomial
          if (coefp(p))
            #f;
          else
            cv := add!(p[0], cv);
            for (i = 1-(length(p)) then 1-(i), until i = 0)
              cv3(p[i]);
            finally
              #f;
            end for;
          end if;
        end method cv3;
  let list92543 = rat-numerator(r);
  begin do(cv2, list92543); list92543; end;
  let list92543 = rat-denominator(r);
  begin do(cv2, list92543); list92543; end;
  cv;
end method collectvars;

//  for each variable in a rational form, we want to compute (or remind
//  ourselves that we already know) its derivative wrt *var*
#f;

define method ratdiff (r, v)
  let setupderiv
      = method (h)
          //  variable is global, h is a number..
          //  we store derivative on a special derivative hash-table
          difftab[h] := into-rat(gendiff(revtab[h], *var*));
        end method;
  let c = collectvars(r);
  fluid-bind (*var* = v)
    fluid-bind (*r* = r)
      fluid-bind (*varnum* = vartab[v])
        let difftab = make(<table>, size: 2 + size(c));
        fluid-bind (*sign* = 1)
          let result = poly2rat(0, 1);
          //  set up derivatives for all kernels in this rational expression
          begin do(setupderiv, c); c; end;
          let list92543 = rat-numerator(r);
          begin do(ratdiff1, list92543); list92543; end;
          *sign* := -1;
          let list92543 = rat-denominator(r);
          begin do(ratdiff1, list92543); list92543; end;
          result;
        end fluid-bind;
      end fluid-bind;
    end fluid-bind;
  end fluid-bind;
end method ratdiff;

define method ratdiff1 (pr)
  let poly = head(pr);
  let pow = tail(pr) * *sign*;
  //  takes care of numerator/denominator
  if (coefp(poly))
    #f;
  elseif (pfreevar(poly, *varnum*))
    #f;
  else
    //  for each (poly . pow) do result= pow*r*poly'/poly + result
    result
     := rat+(result,
             rat*(*r*,
                  rat*(poly2rat(pow, 1),
                       rat*(ratdiff2(poly), poly2rat(poly, -1)))));
  end if;
end method ratdiff1;

//  ratdiff2 takes a polynomial and returns a rational form
//  which is its derivative
define method ratdiff2 (p)
  block (return-from-ratdiff2)
    if (coefp(p)) return-from-ratdiff2(poly2rat(0, 1)); end if;
    let dp = difftab[p[0]];
    let ans = poly2rat(0, 1);
    let x = vector(p[0], 0, 1);
    // mainvar as poly
    //  (format t "~%dp=~s,x=~s" dp x)
    if (fpe-coefzero-p(rat-numerator(dp)))
      //  main var of poly is independent of *var*
      for (i = 1-(length(p)) then 1-(i), until i = 0)
        ans := rat+(ans, rat*(poly2rat(x, i - 1), ratdiff2(p[i])));
      finally
        ans;
      end for;
    else
      //  main var of poly depends on *var*
      for (i = 1-(length(p)) then 1-(i), until i = 0)
        let _that = #f;
        if (_that := coefzerop(p[i]))
          _that;
          //  add zero term.. do nuthin.
          //  given c*p^n, set
          // ans := ans + n*c*p^(n-1)*dp + dc *p^n
          else
          ans
           := rat+(ans,
                   rat+(rat*(ratdiff2(p[i]), poly2rat(x, i - 1)),
                        rat*(poly2rat(i - 1, 1),
                             rat*(poly2rat(p[i], 1),
                                  rat*(poly2rat(x, i - 2), dp)))));
        end if;
      finally
        ans;
      end for;
    end if;
  end block;
end method ratdiff2;

//  set up a table of derivatives .. or here we are
//  using atom property lists.  Is that fair?
//  maybe a hash table would be better?
//  this computes derivative wrt 1st and only argument..
//  how can we store the derivative wrt nth argument?
symbol-get-property(#"exp", #"deriv") := #(#"exp", #"%");

//  or should it be (Power E %)???
symbol-get-property(#"log", #"deriv") := #(#"power", #"%", -1);

symbol-get-property(#"sin", #"deriv") := #(#"cos", #"%");

symbol-get-property(#"cos", #"deriv") := #(#"times", -1, #(#"sin", #"%"));

symbol-get-property(#"tan", #"deriv") := #(#"power", #(#"sec", #"%"), 2);

symbol-get-property(#"sec", #"deriv")
 := #(#"times", #(#"sec", #"%"), #(#"tan", #"%"));

// ... etc rest of Trig functions
symbol-get-property(#"sinh", #"deriv") := #(#"cosh", #"%");

// ... etc rest of Hyperbolic functions
symbol-get-property(#"arcsin", #"deriv")
 := #(#"power", #(#"plus", -1, #(#"power", #"%", 2)), -0.5);

symbol-get-property(#"arccos", #"deriv")
 := #(#"times", -1,
      #(#"power", #(#"plus", 1, #(#"times", -1, #(#"power", #"%", 2))), -0.5));

symbol-get-property(#"arctan", #"deriv")
 := #(#"power", #(#"plus", 1, #(#"power", #"%", 2)), -1);

symbol-get-property(#"arcsec", #"deriv")
 := #(#"times",
      #(#"power", #(#"plus", 1, #(#"times", -1, #(#"power", #"%", -2))),
        -0.5),
      #(#"power", #"x", -2));

// ... etc rest of ArcTrig functions
symbol-get-property(#"arcsinh", #"deriv")
 := #(#"power", #(#"plus", 1, #(#"power", #"%", 2)), -0.5);

symbol-get-property(#"arccosh", #"deriv")
 := #(#"times",
      #(#"power",
        #(#"times", #(#"plus", -1, #"%"),
          #(#"power", #(#"plus", 1, #"%"), -1)),
        -0.5),
      #(#"power", #(#"plus", 1, #"%"), -1));

symbol-get-property(#"arctanh", #"deriv")
 := #(#"power", #(#"plus", 1, #(#"times", -1, #(#"power", #"%", 2))), -1);

symbol-get-property(#"arcsech", #"deriv")
 := #(#"times", -1, #(#"power", #"%", -1),
      #(#"power",
        #(#"times", #(#"plus", 1, #(#"times", -1, #"%")),
          #(#"power", #(#"plus", 1, #"%"), -1)),
        -0.5),
      #(#"power", #(#"plus", 1, #"x"), -1));

// ... etc could add the rest of ArcHyperbolic functions ..Cot,Csc
//  odds and ends: Erf, ExpIntegralEi, Abs
symbol-get-property(#"erf", #"deriv")
 := #(#"times", 2,
      #(#"power",
        #(#"times", #(#"exp", #(#"power", #"%", 2)), #(#"power", #"pi", 0.5)),
        -1));

symbol-get-property(#"expintegralei", #"deriv")
 := #(#"times", #(#"exp", #"%"), #(#"power", #"%", -1));

//  line below is perhaps a problem when x=0..
symbol-get-property(#"abs", #"deriv")
 := #(#"times", #(#"abs", #"%"), #(#"power", #"x", -1));

//  integration properties
symbol-get-property(#"log", #"integ")
 := #(#"times", #"%", #(#"plus", -1, #(#"log", #"%")));

symbol-get-property(#"sin", #"integ") := #(#"times", -1, #(#"cos", #"%"));

symbol-get-property(#"cos", #"integ") := #(#"sin", #"%");

symbol-get-property(#"tan", #"integ")
 := #(#"times", -1, #(#"log", #(#"cos", #"%")));

symbol-get-property(#"sec", #"integ")
 := #(#"times", 2, #(#"arctanh", #(#"tan", #(#"times", 0.5, #"%"))));

symbol-get-property(#"cot", #"integ")
 := #(#"times", -1, #(#"log", #(#"sin", #"%")));

//  etc
symbol-get-property(#"arcsin", #"integ")
 := #(#"plus", #(#"times", #"%", #(#"arcsin", #"%")),
      #(#"power", #(#"plus", 1, #(#"times", -1, #(#"power", #"%", 2))), 0.5));

symbol-get-property(#"arccos", #"integ")
 := #(#"plus", #(#"times", #"%", #(#"arccos", #"%")),
      #(#"times", -1,
        #(#"power", #(#"plus", 1, #(#"times", -1, #(#"power", #"%", 2))),
          0.5)));

symbol-get-property(#"arctan", #"integ")
 := #(#"plus", #(#"times", #"%", #(#"arctan", #"%")),
      #(#"times", -0.5, #(#"log", #(#"plus", 1, #(#"power", #"%", 2)))));

//  etc
symbol-get-property(#"arctanh", #"integ")
 := #(#"plus", #(#"times", #"%", #(#"arctanh", #"%")),
      #(#"times", 0.5, #(#"log", #(#"plus", -1, #(#"power", #"%", 2)))));

symbol-get-property(#"exp", #"integ") := #(#"exp", #"%");

// 
// Here's some more odds and ends
symbol-get-property(#"expintegralei", #"integ")
 := #(#"plus", #(#"times", #"%", #(#"expintegralei", #"%")),
      #(#"times", -1, #(#"exp", #"%")));

// int[ erf(_X) ] := _X * erf(_X) + Pi^(-1/2) * exp(-_X^2)
symbol-get-property(#"erf", #"integ")
 := #(#"plus", #(#"times", #"%", #(#"erf", #"%")),
      #(#"power",
        #(#"times", #(#"exp", #(#"power", #"%", 2)), #(#"power", #"pi", 0.5)),
        -1));

symbol-get-property(#"abs", #"integ")
 := #(#"times", 0.5, #"%", #(#"abs", #"%"));

//  Well, we have to do some non-rational differentiation, and this
//  program is the one that does it. 
//  Restrictions that remain: it doesn't grok derivatives of
//  symbolic Derivatives.  That is, (gendiff '(((Derivative 1) f) x) 'x)
//  should mean something, namely (((Derivative 2) f) x)
define method gendiff (h, v)
  if (h == v)
    1;
  elseif (freevar(h, v))
    0;
  elseif (~ instance?(h, <pair>))
    error("Gendiff of %=", h);
  elseif (member?(head(h), #(#"plus", #"times")))
    outof-rat(ratdiff(into-rat(h), v));
  elseif (head(h) == #"power")
    if (instance?(third(h), <integer>))
      outof-rat(ratdiff(into-rat(h), v));
    else
      ged(h, v);
    end if;
    //  fake a derivative if necessary
    else
    begin
      let k = #f;
      k
       := if (not(instance?(head(h), <list>)))
            symbol-get-property(head(h), #"deriv");
          else
            #f;
          end if;
      //  some kind of operator head like (((Derivative.)))
      // 	      (format t "~% deriv=~s h=~s" k h)
      //  Note that Mathematica (tm) uses the notation
      //  equivalent to (((Derivative 1) f) x) for f'[x].
      //  This allows for the handling of
      //  f(u(x),v(x)) ...
      if (empty?(k))
        // unknown deriv. Use chain rule
        for (i = cdr(h) then cdr(i), dlist = #(1) then cons(0, dlist),
             ans = nil then //  initialize answer
                            //  all the other times through
             let (thispart(gendiff(car(i), v)))()
                 = if (eql(0, thispart))
                     ans;
                     cons(list(#"times", thispart,
                               uniq(cons(list(cons(#"derivative", dlist),
                                              car(h)),
                                         cdr(h)))),
                          ans);
                   end if;,
             until //  termination test
             empty?(i))
          #f;
        finally
          simp(pair(#"plus", ans));
        end for;
      else
        simp(uniq(list(#"times", replace-in-tree(second(h), #"%", k),
                       gendiff(second(h), v))));
      end if;
    end;
  end if;
end method gendiff;

//  generalexponentdiff
define method ged (e, x)
  let a = second(e);
  let b = third(e);
  simp(if (freevar(b, x))
         //  one form would be b*a^(b-1)*d(a,x)
         //  a better form would be b*a^b/a *d(a,x), using same kernels
         list(#"times", b, e, apply(list, #"power", a, #(-1)), gendiff(a, x));
       elseif (a == #"e")
         //  exponential
         list(#"times", e, gendiff(b, x));
       else
         list(#"times", e, gendiff(list(#"times", b, list(#"log", a)), x));
       end if);
end method ged;

//  This is the main derivative-divides integration program.
//  Integrate exp-in  wrt var, both in list form.  It returns
//  an answer in list form, as well.
define method diffdiv (exp-in, var)
  block (return-from-diffdiv)
    let exp = into-rat(exp-in);
    let factors
        = concatenate(rat-numerator(exp),
                      map(method (x) pair(head(x), - cdr(x)); end method,
                          rat-denominator(exp)));
    let constcoef = poly2rat(1, 1);
    let vnum = look-up-var(var);
    let den = #f;
    let ans = #f;
    let vfactors = #f;
    for (f = factors then cdr(f), until empty?(f))
      //  several possibilities for (car f)  which is a (factor . power)
      if (pfreevar(head(head(f)), look-up-var(var)))
        // in which case we multiply constcoef by (caar f)^(cdar f)
        if (tail(head(f)) > 0)
          rat-numerator(constcoef)
           := fpe-insert(head(head(f)), tail(head(f)),
                         rat-numerator(constcoef));
        else
          rat-denominator(constcoef)
           := fpe-insert(head(head(f)), - cdar(f),
                         rat-denominator(constcoef));
        end if;
      else
        vfactors := pair(head(f), vfactors);
      end if;
    finally
      #f;
    end for;
    // (format t "~%constcoef=~s, vfactors=~s" (outof-rat constcoef) vfactors)
    //  if all the factors are free of the variable of integration,
    //  quit now with the answer.
    if (empty?(vfactors))
      return-from-diffdiv(outof-rat(rat*(constcoef, into-rat(var))));
    end if;
    //  vfactors is a list of the (factor . power) pairs containing var.
    exp := rat/(exp, constcoef);
    //  Next, let's do some quick checks.
    //  if the integrand is just a polynomial, we can do it
    //  another way. Here's how...
    if (//  only the variable itself depends on var
        every?(method (h) vnum == h | freevar(revtab[h], var); end method,
               collectvars(exp))
         & //  and the denominator of exp is free of v entirely
        rfreevar((den
                   := make-rat(numerator: #(#(1 . 1)),
                               denominator: rat-denominator(exp))),
                 var))
      return-from-diffdiv(outof-rat(rat*(constcoef,
                                         rat*(den,
                                              pintegrate(fpe-expand(rat-numerator(exp)),
                                                         vnum)))));
    end if;
    //  ok, we do not have a polynomial in var.  We could check here for
    //  a simple rational function in var by the same kind of
    //  check on the denominator (is it a simple poly in var also?)
    //  But maybe derivative-divides will work on it, anyway..
    block (return)
      for (f = vfactors then cdr(f),
           until //  if we've exhausted vfactors unsuccessfully, return the "input".
                 //  this is perhaps not what is wanted -- we could provide, for
                 //  example, an error message, or we could call another routine.
           empty?(f))
        let y = head(head(f));
        let ypow = tail(head(f));
        let yprime = ratdiff(poly2rat(y, 1), var);
        // 	  (format t "~% y=~s, ypow=~s yprime=~s" y ypow yprime)
        // 	  (format t "~% y=~s, ypow=~s yprime=~s" (intol y) ypow (outof-rat yprime))
        ans := rat/(exp, rat*(poly2rat(y, ypow), yprime));
        // (format t "~%ans=~s" (outof-rat ans))
        if (rfreevar(ans, var))
          //  check if y*y' or y^n*y' divides
          //  AHA -- WE've GOT IT!
          if (~ (ypow == -1))
            return-from-diffdiv(//  const* (y^(p+1))/(p+1)
                                outof-rat(rat*(rat*(constcoef, ans),
                                               rat*(poly2rat(y, ypow + 1),
                                                    poly2rat(ypow + 1,
                                                             -1)))));
          elseif (ypow = -1)
            //  we have y^-1*y' -> log(y)
            return-from-diffdiv(//  const* log(y)
                                outof-rat(rat*(constcoef,
                                               rat*(ans,
                                                    into-rat(ulist(#"log",
                                                                   outof-rat(poly2rat(y,
                                                                                      1))))))));
          end if;
        else
          //  rfreevar test fails on y^n*y'.  Try extracting the head of
          //  y, that is, y=f(u), and look for f(u)*u'  (if we know
          //  an antiderivative for f, anyway.)
          if (~ (ypow = 1))
            #f;
          else
            begin
              let ylist = intol(y);
              let head = #f;
              let antideriv = #f;
              let arg = #f;
              if (instance?(ylist, <pair>)
                   & not(instance?((head := head(ylist)), <list>)))
                // usual case
                antideriv := symbol-get-property(head, #"integ");
                arg := second(ylist);
              elseif (nil);
              end if;
              // 	       (format t "~% y=~s, antideriv=~s yprime=~s" ylist antideriv		       (gendiff arg var))
              //  another case is (((Derivative list) fun) x1 ...)
              //  We haven't programmed that yet .. How to look up
              //  the antiderivative should not be toooo hard.
              if (not(instance?(ylist, <list>)))
                #f;
                //  if ylist is an atom, deriv is 0 or 1
                elseif (tail(tail(ylist)) | empty?(antideriv))
                //  we don't know antiderivative 
                //  or f has more than one argument
                #f;
                //  check the exact division situation:
                elseif (rfreevar(ans
                                  := rat/(exp,
                                          rat*(poly2rat(y, 1),
                                               into-rat(gendiff(arg, var)))),
                                 var))
                //  AHA -- WE'VE GOT IT!
                return-from-diffdiv(outof-rat(rat*(into-rat(replace-in-tree(second(ylist),
                                                                            #"%",
                                                                            antideriv)),
                                                   rat*(ans, constcoef))));
              end if;
            end;
          end if;
        end if;
      finally
        return(ulist(#"integrate", exp-in, var));
      end for;
    end block;
  end block;
end method diffdiv;

// Derivative divides cannot integrate polynomials, in general. This
//  program (Rint) can.  It leaves the answer in factored form, which
//  is sometimes neat, but sometimes distracting.
// The task: Integrate a polynomial over the integers. 
// The ploy is as follows:
//  We want to do it as much as possible as a polynomial over the integers,
//  so we can just do x^2 -> x^3/3 which requires rational numbers.
//  But we can accumulate denominators separately.  Then consider, for example,
//  integrating  9 + x + 3*x^2 + 7*x^3 - 8*x^4. 
//  Since the poly is of degree 4 set the denominator to 5!= 120.
// Let g(k) := 5!/(k+1).  That is g(4) =24, g(3)=30, ... g(0) =120.
// Then the answer is 1/120 times 
//    9*g(0)*x + g(1)*x^2 +3*g(2)*x^3+7*g(3)*x^4-8*g(4)*x^5.
//  Now note that we can factor out the common factor of x, so the answer is
//  (x/120)* (9*g(0) +g(1)*x +3*g(2)*x^2 +7*g(3)*x^3-8*g(4)*x^4).
//  or after removing common factors, 
//                                    2       3       4
// Out[24] = 1/20 x (180 + 10 x + 20 x  + 35 x  - 32 x )
//  What about  (1+x) + (1+x+x^2)*y , integrated wrt x?  Let the highest
//  degree of x ANYWHERE be the denominator.  The analysis still works.
//  rint makes the assumption that other kernels in the numerator
//  are free of the variable of integration.
define method rint (r, v)
  let den = make-rat(numerator: #(#(1 . 1)), denominator: rat-denominator(r));
  if (rfreevar(den, v))
    rat*(pintegrate(fpe-expand(rat-numerator(r)), look-up-var(v)), den);
  else
    error("\nDenominator %= is not free of %=", outof-rat(den), v);
  end if;
end method rint;

define method rint (r, v) outof-rat(rint(into-rat(r), v)); end method rint;

//  pintegrate takes a polynomial and a variable but returns a
//  RATIONAL form
define method pintegrate (p, v)
  local method g (k) maxfact / k; end method g,
        method factorial (k)
          if (k = 0) 1; else k * factorial((k - 1)); end if;
        end method factorial,
        //  set maxdegv to maximum degree that v occurs in p. return value nil.
        method maxdegree (p)
          let _that = #f;
          if (_that := coefp(p))
            _that;
          elseif (_that := var>(v, p[0]))
            _that;
          elseif (p[0] == v)
            maxdegv := max(maxdegv, size(p) - 2);
          else
            for (i = 1-(length(p)) then 1-(i), until i = 0)
              //  returns nil. size effect to maxdegv
              maxdegree(p[i]);
            end for;
          end if;
        end method maxdegree,
        method pd1 (p)
          if (coefp(p))
            p * maxfact;
          elseif (var>(v, p[0]))
            p;
          elseif (p[0] == v)
            r := make(<vector>, size: size(p));
            r[0] := v;
            for (i = 1-(length(p)) then 1-(i), until i = 0)
              r[i] := p*(g(i), p[i]);
            finally
              // 		     (format t "~%integrated p=~s to get r=~s" p r)
              r;
            end for;
          else
            r := copy-sequence(p);
            for (i = 1-(length(r)) then 1-(i), until i = 0)
              r[i] := pd1(r[i]);
            finally
              r;
            end for;
          end if;
        end method pd1;
  maxdegree(p);
  maxfact := factorial(maxdegv + 1);
  //  (format t "~s, ~s, ~s" (pd1 p) v maxfact)
  rat/(rat*(poly2rat(vector(v, 0, 1), 1),
            make-rat(numerator: make-fpe(pd1(p), 1),
                     denominator: list(#(1 . 1)))),
       poly2rat(maxfact, 1));
end method pintegrate;

//  Notes for further heuristics.
//   for symbolic n, make Int[x^n,x] into (x^(n+1)-1)/(n+1).
//  This has a different constant from the usual, but the nice property that
//  if n-> -1, the answer is Log[x]. (suggested by WK 12/90)
//  similar stuff possible for Cos[n*x]*Sin[m*n] formulas when n=+-m.
"eof";

