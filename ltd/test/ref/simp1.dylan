//  -*- Mode:Common-Lisp; Package:mma; Base:10 -*-
// 
//  Written by: Tak W. Yan
//  modified by Richard Fateman
//  Contains: procedures that convert between lisp prefix form 
//            and representation of rat's and also the simplifier itself
//  (c) Copyright 1990 Richard J. Fateman, Tak Yan
//  Basically three functions are provided.
//      into-rat:  take an expression parsed by parser, simplify it, and
//                 represent it as a "rat"
//      outof-rat: take a rat and translate it back to Mathematica (tm)-like
//                 language
//      simp:      simplify an expression by converting it back and forth
// (provide 'simp1)
#f;

//  export what needs to be exported
// (require "ucons1")(require "poly")(require "rat1")
// LTD: Function LOAD not yet implemented.
load("mma");

//  need the symbols like Plus
"(in-package mma)";

//  don't export. Make everyone do (in-package :mma).
//
nil(#f, #f);

define variable vartab = make(<table>, test: \=);

//  map from kernels to integers
define variable revtab = make(<table>, test: \==);

//  map from integers to kernels
//  reptab should use eq, if we believe that expressions will be
//  "unique" coming from the parser or other transformation programs.
//  (see what happens coming out of "outof-rat" for example.)
//
define variable reptab = make(<table>, test: \==);

//  map from expressions to rats
define variable disreptab = make(<table>, test: \==);

//  map from rats to expressions
//  look-up-var: look up a variable in the hash-table; if exists,
//     	        return the value of it; otherwise, increment varcount
//               and add variable to hash-table
define method look-up-var (x)
  let _that = #f;
  if (_that := vartab[x])
    _that;
  else
    revtab[varcount := varcount + 1] := x;
    vartab[x] := varcount;
  end if;
end method look-up-var;

//  special hack to make the variable x the ``MOST'' MAIN variable.
//  useful for integration variable, for example.
//  Do this before any other use of the symbol z.
define method make-main-var (z, #key place = 536870911)
  if (vartab[z])
    if (~ (z == revtab[place]))
      format-out("\n %= already is a variable with a different order;\n   existing expressions may be broken",
                 z);
    end if;
  elseif (nil);
  end if;
  if (revtab[place])
    format-out("\n %= was most-main, previously.", revtab[place]);
    make-main-var(revtab[place], place - 1);
  elseif (nil);
  end if;
  vartab[z] := place;
  revtab[place] := z;
end method make-main-var;

//  into the rat representation
//  into-rat: represent as a rat
define method into-rat (x)
  select (x by instance?)
    integer
       => coef2rat(x);
    ratio
       => make-rat(numerator: list(pair(numerator(x), 1)),
                   denominator: list(pair(denominator(x), 1)));
    rat
       => x;
    // already a rat form
    // 	    (array  whatever)
    #t
       => begin
            let _that = #f;
            if (_that := reptab[x])
              _that;
            elseif (_that := (reptab[x] := into-rat-1(x)))
              _that;
            end if;
          end;
  end select;
end method into-rat;

//  into-rat-1: actually do the work
define method into-rat-1 (x)
  let h = #f;
  if (instance?(x, <symbol>))
    var2rat(x);
  elseif (instance?(x, <pair>) & not(instance?((h := head(x)), <list>)))
    if (h == #"integer")
      coef2rat(second(x));
    elseif (h == #"times")
      reduce-rat(rat*, into-rat(second(x)), tail(tail(x)));
    elseif (h == #"plus")
      reduce-rat(rat+, into-rat(second(x)), tail(tail(x)));
    elseif (h == #"power")
      into-rat-2(x);
    else
      var2rat(umapcar(simp, x));
    end if;
  elseif (instance?(x, <pair>))
    var2rat(umapcar(simp, x));
    //  we could check for interval or bigfloat or ?? but then what?
    //  we give up on what this is. just wrap it up as a variable
    else
    var2rat(x);
  end if;
end method into-rat-1;

//  into-rat-2: handle the case of powering
define method into-rat-2 (x)
  let exp = into-rat(third(x));
  let exp-n = exp.rat-numerator;
  let exp-d = exp.rat-denominator;
  if (fpe-coef-p(exp-n) & fpe-coef-p(exp-d))
    if (fpe-coefone-p(exp-d))
      rat^(into-rat(second(x)), fpe-expand(exp-n));
    else
      rat^(var2rat(ulist(#"power", simp(second(x)),
                         simp(ulist(#"times", 1,
                                    ulist(#"power",
                                          fpe-expand(exp-d),
                                          -1))))),
           fpe-expand(exp-n));
    end if;
  else
    var2rat(ulist(#"power", simp(second(x)), outof-rat(exp)));
  end if;
end method into-rat-2;

//  var2rat: convert a variable into a rat
define method var2rat (x)
  make-rat(numerator: make-fpe(vector(look-up-var(x), 0, 1), 1),
           denominator: make-fpe(coefone(), 1));
end method var2rat;

//  coef2rat: convert a coef into a rat
define method coef2rat (x)
  make-rat(numerator: make-fpe(x, 1), denominator: make-fpe(coefone(), 1));
end method coef2rat;

//  reduce-rat: apply fn to r and the result of applying fn recursively to
//              the terms in l
define method reduce-rat (fn, r, l)
  if (empty?(l))
    r;
  else
    fn(r, reduce-rat(fn, into-rat(head(l)), tail(l)));
  end if;
end method reduce-rat;

//  out of the rat representation
//  outof-rat: translate back to Mathematica (tm)-like lists
define method outof-rat (r)
  let _that = #f;
  if (_that := disreptab[r])
    _that;
  elseif (_that
           := (disreptab[r]
                          := begin
                               let n = fintol(r.rat-numerator);
                               let d = fintol(r.rat-denominator);
                               let ans = outof-rat-1(n, d);
                               (reptab[ans] := r);
                               //  controversy here
                               ans;
                             end))
    _that;
  end if;
end method outof-rat;

//  outof-rat-1: take 2 fpe's, n and d, and translate 
//               into list form; n is numerator and d is denominator;
define method outof-rat-1 (n, d)
  if (1 = d)
    n;
    //  e.g. 3
    elseif (1 = n)
    if (instance?(d, <pair>) & head(d) == #"power")
      if (instance?(third(d), <number>))
        //  e.g. y^(-4)
        ulist(#"power", second(d), - caddr(d));
      else
        //  e.g. y^(-x)
        ulist(#"power", second(d), ulist(#"times", -1, third(d)));
      end if;
    elseif (instance?(d, <number>))
      d ^ -1;
      //  1/3
      else
      ulist(#"power", d, -1);
    end if;
    //  x^-1
    elseif (instance?(n, <number>) & instance?(d, <number>))
    n / d;
    //  e.g. 1/2
    elseif (instance?(d, <number>))
    ulist(#"times", outof-rat-1(1, d), n);
    //  e.g. 1/30(x^2+1)
    else
    ulist(#"times", n, outof-rat-1(1, d));
  end if;
end method outof-rat-1;

//  fintol: convert a fpe into list form; the fpe must be normal,
define method fintol (f)
  let c = head(head(f));
  //  constant term
  for (j = cdr(f) then cdr(j), lis = nil then nil, until empty?(j))
    //  the loop body:
    lis := pair(fintol2(head(head(j)), tail(head(j))), lis);
  finally
    if (empty?(lis))
      c;
    elseif (coefonep(c) & empty?(tail(lis)))
      head(lis);
    else
      if (coefonep(c))
        ucons(#"times", uniq(reverse!(lis)));
      else
        ucons(#"times", ucons(c, uniq(reverse!(lis))));
      end if;
    end if;
  end for;
end method fintol;

//  fintol2: break a pair in a fpe into list form
define method fintol2 (p, e)
  if (e == 1) intol(p); else ulist(#"power", intol(p), e); end if;
end method fintol2;

//  intol: convert a polynomial in vector form into list form
define method intol (p)
  if (instance?(p, <vector>))
    //  look up what the kernel is from revtab
    intol+(intol2(p,
                  element(revtab, p[0],
                                    default: // in case (svref p 0) is not 
                                             //  in hashtable, use it literally
                                    p[0])));
  else
    p;
  end if;
end method intol;

//  intol2: help intol
define method outof-rat-check (x)
  if (rat-p(x)) outof-rat(x); else intol(x); end if;
end method outof-rat-check;

define method intol2 (p, var)
  let res = #f;
  let term = #f;
  for (i = 1-(length(p)) then 1-(i), until i = 0)
    term := intol*(outof-rat-check(p[i]), intol^(i - 1, var));
    let _that = #f;
    if (_that := term == 0) _that; else res := ucons(term, res); end if;
  finally
    res;
  end for;
end method intol2;

//  intol^: handle the case for powering
define method intol^ (n, var)
  if (zero?(n)) 1; elseif (n = 1) var; else ulist(#"power", var, n); end if;
end method intol^;

//  intol+: handle +
define method intol+ (p)
  if (empty?(tail(p)))
    head(p);
  elseif (isplus(head(p)))
    uappend(head(p), tail(p));
  else
    ucons(#"plus", p);
  end if;
end method intol+;

//  intol*: handle *
define method intol* (a, b)
  if (a == 0)
    0;
  elseif (a == 1)
    b;
  elseif (b == 1)
    a;
  else
    ucons(#"times", uappend(intol*chk(a), intol*chk(b)));
  end if;
end method intol*;

//  into*chk: help into*
define method intol*chk (a)
  if (istimes(a)) tail(a); else ulist(a); end if;
end method intol*chk;

//  IsPlus: check if a is led by a 'Plus
define method isplus (a)
  instance?(a, <pair>) & head(a) == #"plus";
end method isplus;

//  IsTimes: check if a is led by 'Times
define method istimes (a)
  instance?(a, <pair>) & head(a) == #"times";
end method istimes;

//  simplify by converting back and forth
//  simp: simplify the expression x
//  assumes that all kernels that are non-identical are algebraically
//  independent. Clearly false for e.g. Sin[x], Cos[x], E^x, E^(2*x)
define method simp (x) outof-rat(into-rat(x)); end method simp;

//  This gives a list of kernels assumed to be independent.
//  If they are NOT, then the simplification may be incomplete.
//  In general, the search for the "simplest" set of kernels is
//  difficult, and leads to (for example) the Risch structure
//  theorem, Grobner basis decompositions, solution of equations
//  in closed form algebraically or otherwise.  Don't believe me?
//  what if the kernels are Rootof[x^2-3],Rootof[y^4-9], Log[x/2],
//  Log[x], Exp[x], Integral[Exp[x],x] ....
define method kernelsin (x)
  pair(#"list",
       map(method (x) revtab[x]; end method, collectvars(into-rat(x))));
end method kernelsin;

