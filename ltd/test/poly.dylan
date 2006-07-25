//  -*- Mode:Common-Lisp; Package:mma; Base:10 -*-
// 
//  Written by: Richard J. Fateman
//  File: poly.lisp
//  Contains: vector-based polynomial recursive representation arithmetic
//  (c) Copyright 1990 Richard J. Fateman, Peter Klier, Peter Norvig, Tak Yan
// (provide 'poly)
//  A polynomial is either a number (integer -- for now)
//  or a vector of length d+2 where d>0 is the degree of S
//  S=#(mainvar coeff coeff ... coeff).
//               ^                ^
//               |                |-coeff of mainvar^d : non-zero
//               |-coeff of mainvar^0  
//  e.g.
//  4 + 5*x + x^2  = #(mainvar 4 5 1), where mainvar is an integer
//  representing x in the system.
//  Coeffs can be polynomials in other variables.
//  Note: this is not the shortest code, but it should not be much slower
//  than the fastest code absolutely necessary to get the job done.
// (provide 'vector-math)
#f;

#f;

"(in-package mma)";

//  don't export: make everyone else do (in-package :mma).
// 
// 
//  coefp: is defined to return true (non-nil, anyway)
//         for an element of the coefficient domain. By default, we
//         use integers.  This could be changed in concert with other
//         programs to support other domains (see coef+).
//         Coefp is usually called to see if its argument is
//         (recursively) a polynomial in yet more variables, or is the
//         ``base case'' of a polynomial which is, in fact, a coefficient.
//         By defining it as "numberp" rather than "integerp" it works more
//         smoothly (no change needed for rationals or floats).
nil(#f, nil(), #(#(), #()));

//  mainvar:  extract the main variable from a polynomial
// LTD: No macros.
#"mainvar";

//  samevar:  see if two polynomials have the same main variable
// LTD: No macros.
#"samevar";

//  degree: extract the degree from a polynomial (degree in main variable)
// LTD: No macros.
#"degree";

//  lc:  extract the leading coefficient of a polynomial
// LTD: No macros.
#"lc";

//  constc: extract the constant coefficient of a polynomial
//  with respect to its main variable
// LTD: No macros.
#"constc";

//  monomialp: check if v is a monomial (like x, or 3x^2)
define method monomialp (v :: <simple-vector>)
  size(v)
   = find-key(copy-subsequence(v, start: 1),
              curry(\==, complement(method (x) (x = 0); end method)))
      + 1;
end method monomialp;

//  var>:  an ordering predicate on variables to determine which
//         is more "main". We could use strings or symbols for the variables
//         instead of numbers, and use alphabetic ordering for var>.
//         This seems simpler overall, since the variables might be more
//         complicated in applications (e.g. (cos z)).
//         Some easy encoding of names to numbers can be used (e.g. the
//         first symbol mentioned is 1, and then count up.)
// LTD: No macros.
#"var>";

//   The next set of macro definitions supplies the arithmetic
//   for coefficients.  In general, any domain that can support
//   the coefficient operations below is fair game for the package.
//   More advanced operations may place additional requirements
//   on the coefficient domain (E.g. they form an algebraic Ring or Field).
//   Add/subtract/multiply two coefficients, etc.
// LTD: No macros.
#"coef+";

// LTD: No macros.
#"coef-";

// LTD: No macros.
#"coef*";

// LTD: No macros.
#"coef>";

//  Dividing two coefficients is not so obvious.  If the coefficient
//  domain is the common-lisp rational numbers (an algebraic field) and
//  if y is non-zero, then (/ x y) is also in the same field.  If the
//  coefficient domain is the integers, then division may throw the
//  computation out of that domain.  (/ 1 2) is not an integer.
//  Here we assume the person or program using coef/ is knowledgeable
//  about its uses.  Actually, we go further than that here. We
//  don't use coef/ in this file at all.  In the places we use
//  division (in p/ ) we make the (perhaps unwarranted) assumption
//  that exact division over the integers is required. See p/.
// LTD: No macros.
#"coef/";

//   coef^ computer a coefficient to an integer power n
// LTD: No macros.
#"coef^";

//  some extra pieces just in case they're needed: remainder, negation, and
//  absolute value
// LTD: No macros.
#"coefrem";

define method coefneg (x) - x; end method coefneg;

define method coefabs (x) abs(x); end method coefabs;

//  Tests for zero and unity are important, as are constructors
// LTD: No macros.
#"coefzero";

//   zero in the coefficient domain
// LTD: No macros.
#"coefone";

//  unity in the coefficient domain
// LTD: No macros.
#"coefzerop";

// LTD: No macros.
#"coefonep";

// LTD: No macros.
#"coef-negative-p";

define method coefzerofunp (x) 0 == x; end method coefzerofunp;

//   to funcall, we need a function
//  This product function preserves both arguments and returns the product
//  of two polynomials.
//  p*: return the product of v1*v2
define method p* (v1, v2)
  if (coefp(v1))
    p*cv(v1, v2);
    //  call function to multiply coef*poly
    elseif (coefp(v2))
    p*cv(v2, v1);
  elseif (samevar(v1, v2))
    //  two polynomials in the same main variable
    //  do  n X m multiplications
    begin
      let ilim :: <integer> = #f;
      let jlim :: <integer> = #f;
      let index :: <integer> = #f;
      let ival = #f;
      let res = #f;
      begin ilim := size(v1) - 1; jlim := size(v2) - 1; end;
      res := make(<array>, dimensions: ilim + jlim);
      res[0] := mainvar(v1);
      for (i = 1 then 1+(i), until i > ilim)
        ival := v1[i];
        if (~ coefzerop(ival))
          //  shortcut 0 * anything = 0
          for (j = 1 then 1+(j), until j > jlim)
            index := i + j + -1;
            res[index] := p+vv-zero-check(res[index], p*(ival, v2[j]));
          end for;
        end if;
      finally
        res;
      end for;
    end;
  elseif (var>(mainvar(v1), mainvar(v2)))
    p*cv(v2, v1);
  else
    p*cv(v1, v2);
  end if;
end method p*;

//  p*cv: coefficient times polynomial vector;
//        preserves both inputs and although the result can
//        share substructure with the vector v, the top-level
//        vector is new. (true recursively)
define method p*cv (c, v)
  if (coefp(v))
    coef*(c, v);
  elseif (coefzerop(c))
    coefzero();
    //  0 * anything is 0	
    elseif (coefonep(c))
    v;
    //  1 * v = v. 
    //  run down the length of the vector, multiplying.
    //  p* is not destructive of its arguments either.
    else
    begin
      let v :: <simple-vector> = v;
      let len :: <integer> = size(v);
      let u :: <simple-vector> = #f;
      u := make(<array>, dimensions: len);
      mainvar(u) := mainvar(v);
      for (i = 1-(len) then 1-(i), until i = 0)
        u[i] := p*(c, v[i]);
      finally
        u;
      end for;
    end;
  end if;
end method p*cv;

//  pnegate: negate p
//  there are trivial other ways of doing this
//  like multiplying by -1. This takes less time
//  and space.
define method pnegate (v :: <simple-vector>)
  if (coefp(v))
    coefneg(v);
  else
    let u :: <simple-vector> = make(<vector>, size: size(v));
    u[0] := v[0];
    for (i = 1-(length(v)) then 1-(i), until i = 0)
      u[i] := pnegate(v[i]);
    finally
      u;
    end for;
  end if;
end method pnegate;

//  p+: sum two polynomials as vectors, non-destructively
define method p+ (v1, v2)
  if (coefp(v1))
    p+cv(v1, v2);
  elseif (coefp(v2))
    p+cv(v2, v1);
    //  reverse args
    elseif (samevar(v1, v2))
    //  same main var
    begin
      let lv1 :: <integer> = size(v1);
      let lv2 :: <integer> = size(v2);
      let v1 :: <simple-vector> = v1;
      let v2 :: <simple-vector> = v2;
      if (lv1 > lv2)
        p+into(v2, v1, lv2, lv1);
        //  v1 is longer
        else
        p+into(v1, v2, lv1, lv2);
      end if;
    end;
  elseif (var>(mainvar(v1), mainvar(v2)))
    p+cv(v2, v1);
  else
    p+cv(v1, v2);
  end if;
end method p+;

//  p-: compute difference of two polynomials as vectors, non-destructively
//  this could be done trivially by u+(-v) but this is
//  faster and uses less space.
define method p- (v1 :: <simple-vector>, v2 :: <simple-vector>)
  if (coefp(v1))
    p+cv(v1, pnegate(v2));
  elseif (coefp(v2))
    p+cv(pnegate(v2), v1);
  elseif (samevar(v1, v2))
    //  same main var
    //  aw, could do this in a pickier fashion...
    p+(v1, pnegate(v2));
  elseif (var>(mainvar(v1), mainvar(v2)))
    p+cv(pnegate(v2), v1);
  else
    p+cv(v1, pnegate(v2));
  end if;
end method p-;

//  p+cv: add coeff to vector, non-destructively
define method p+cv (c, v)
  if (coefp(v))
    coef+(c, v);
  else
    let v :: <simple-vector> = copy-sequence(v);
    v[1] := p+(c, v[1]);
    v;
  end if;
end method p+cv;

//  p+into: add terms from one polynomial v1 into another
define method p+into (v1, v2, shorter, longer)
  let res :: <simple-vector> = #f;
  res := make(<array>, dimensions: longer);
  res[0] := mainvar(v2);
  for (i = 1 then 1+(i), until i = shorter)
    res[i] := p+vv-zero-check(v1[i], v2[i]);
  end for;
  for (i = shorter then 1+(i), until i = longer)
    res[i] := v2[i];
  finally
    pnorm(res);
  end for;
end method p+into;

//  p+vv-zero-check: helper for p+into
define method p+vv-zero-check (place, form)
  if (coefzerop(place)) form; else p+(place, form); end if;
end method p+vv-zero-check;

//  pnorm converts a polynomial into a normal form in case it is
//  really zero or a constant or has trailing (high degree) zero
//  coeffs.  pnorm is destructive. pnorm is Not recursive except
//  in converting constant terms to manifest non-vectors.
//  Assume x is an arbitrary main-variable index:
//  #(x 5) -> 5.  #(x 5 4 3 0 0) -> #(x 5 4 3).  #(x 0 0) -> 0. 
//  #(x 0 1) -> #(x 0 1) [no change]
//  pnorm: return the normal form of x
define method pnorm (x)
  if (coefp(x))
    x;
  else
    let x :: <simple-vector> = x;
    let pos :: <integer> = #f;
    pos := find-key(x, curry(\==, complement(coefzerofunp)));
    if (pos = 0)
      coefzero();
      //  nothing left but the header: zero polynomial
      elseif (pos = 1)
      //  just the constant itself
      pnorm(x[1]);
      //  constant polynomial
      elseif (pos = size(x) - 1)
      x;
    else
      choose(complement(coefzerofunp), copy-subsequence(x, start: pos));
    end if;
  end if;
end method pnorm;

//  p^v: this may seem like a dumb way to compute powers, but it's not
//  Repeated multiplication is generally faster than squaring.  In this
//  representation, binomial expansion, a good bet for sparse representation,
//  is only sometimes advantageous, and then not by very much, usually.
define method p^ (x, n)
  //  x^n -  n integer, x polynomial
  if (instance?(n, <integer>))
    if (negative?(n))
      error("negative powers not allowed");
    elseif (zero?(n))
      1;
      //  x^0 = 1 (even if x = 0)
      elseif (n == 1)
      x;
      //  x^1 = x
      elseif (coefp(x))
      x ^ n;
    else
      p*(x, p^(x, n - 1));
    end if;
  else
    error("only integer powers allowed");
  end if;
end method p^;

//  p/: divide p by s; only exact divisions are performed, and
//             the result is q such that q*s = p; exact means that p, q,
//             and s all have integer coefficients.
//  This does NOT work over random coefficient domains that you might
//  construct.
//  Ordinarily a single value is returned unless there is an
//  error condition, in which case the first value is nil and the second
//  is a string which describes the problem.
//  Call p/-test if you want a "semi-predicate."
//  Look at the first (or only value). If it is non-nil, you've got
//  the exact quotient.
//  Call p/-and-barf if you want to signal an error in case
//  an exact division is not possible.
define method p/-and-barf (p, s)
  let (v, err) = block (p/) p/(p, s); end block;
  if (empty?(v)) error(err); else v; end if;
end method p/-and-barf;

define method p/-test (p, s) block (p/) p/(p, s); end block; end method p/-test;

define method p/ (p, s)
  if (coefzerop(s))
    p/(values(#f, "Division by zero"));
  elseif (coefonep(s))
    p;
  elseif (coefzerop(p))
    p;
  elseif (coefp(p))
    if (coefp(s))
      if (0 == modulo(p, s))
        p / s;
      else
        p/(values(#f, "Inexact integer division"));
      end if;
    else
      p/(values(#f, "Division by a polynomial of higher degree"));
    end if;
  elseif (coefp(s) | var>(mainvar(p), mainvar(s)))
    p/vc(p, s);
  elseif (var>(mainvar(s), mainvar(p)))
    p/(values(#f, "Division by a polynomial of higher degree"));
  else
    p/helper(p, s);
  end if;
end method p/;

define method p/vc (p, s)
  let q = copy-sequence(p);
  for (i = 1-(length(p)) then 1-(i), until i = 0)
    q[i] := p/(q[i], s);
  finally
    q;
  end for;
end method p/vc;

define method p/helper (p :: <simple-vector>, s :: <simple-vector>)
  let l1 = size(p);
  let l2 = size(s);
  if (l2 > l1)
    p/(values(#f, "Division by a polynomial of higher degree"));
  else
    begin
      let q = make(<array>, dimensions: 2 + l1 + - l2);
      let sneg = pnegate(s);
      let slc = lc(s);
      mainvar(q) := mainvar(p);
      block (return)
        for (i = l1 then length(p), until i < l2)
          q[1 + i + - l2] := p/(lc(p), slc);
          p := pnorm(p+(p, p*(copy-sequence(q, 0, 2 + i + - l2), sneg)));
          if (coefzerop(p)) return(pnorm(q)); end if;
          if (coefp(p)) p/(values(#f, "Quotient not exact")); end if;
        finally
          p/(values(#f, "Quotient not exact"));
        end for;
      end block;
    end;
  end if;
end method p/helper;

//  prem: return the pseudo remainder when p is divided by s
//  (You didn't learn this in high school.) Ref. Knuth, Art of Comptr. Prog.
//  vol. 2.  This turns out to be important for GCD computations.
define method prem (p, s)
  if (coefzerop(s))
    error("Division by zero");
  elseif (coefzerop(p))
    p;
  elseif (coefp(p))
    if (coefp(s)) coefrem(p, s); else p; end if;
  elseif (coefp(s) | var>(mainvar(p), mainvar(s)))
    coefzero();
  elseif (var>(mainvar(s), mainvar(p)))
    p;
  else
    prem2(p, s);
  end if;
end method prem;

//  prem2: return the pseudo remainder when p is divided by s; p and s
//         must have the same variable. This is the usual situation.
define method prem2 (p, s)
  if (size(s) > size(p))
    p;
  else
    let slc = lc(s);
    let l = size(p) - size(s);
    let temp = make(<array>, dimensions: 2 + l);
    mainvar(temp) := mainvar(p);
    block (return)
      for (k = l then m, m = nil then nil, until %f)
        choose(complement(identity), copy-subsequence(temp, start: 2 + k));
        //  fix bug in kcl delete-if, which is non-destructive
        //
        nil(nil(#f), nil(#f));
        p := pnorm(p+(p*(p, slc), p*cv(-1, p*(temp, s))));
        if (coefp(p) | var>(mainvar(s), mainvar(p)))
          return(if (k = 0)
                   p;
                   //  PATCH 12/8/94
                   else
                   p*(p, p^(slc, k));
                 end if);
          //  philliao
          elseif ((m := size(p) - size(s)) < 0)
          return(if (k = 0) p; else p*(p, p^(slc, k)); end if);
        end if;
        if (k - 1 > m) p := p*(p, p^(slc, k - 1 - m)); end if;
      end for;
    end block;
  end if;
end method prem2;

//  Subresultant polynomial gcd.
//  See Knuth vol 2 2nd ed p 410. or TOMS Sept. 1978.
//  pgcd2sr: return the gcd of u and v;
//
nil(#f, nil(#f),
    nil((nil(#f))(#f), (nil(#f))(#f),
        (nil(#f))(nil((nil(#f))(nil(#f, #f)), nil(nil(#f, #f)))),
        (nil(#f))(nil(#f, #f)), (nil(nil(#f), nil(#f)))(nil(#f, #f)),
        (nil(nil(#f), nil(#f)))(nil(#f, #f)), nil(nil(#f, #f))));

//  pgcd2sr: return the gcd of p and q, which have the same main variable
// #+ignore
define method pgcd2sr (p, q)
  //  set up so q is of same or lower degree
  if (size(q) > size(p))
    begin
      let g174861 = p;
      let g174860 = q;
      p := g174860;
      q := g174861;
      #f;
    end;
  elseif (nil);
  end if;
  let pcont = pcontentsr(p);
  let qcont = pcontentsr(q);
  let p = p/(p, pcont);
  let q = p/(q, qcont);
  let content = pgcdsr(pcont, qcont);
  block (return)
    for (g = 1 then lc(p), h = 1 then p/(p^(g, d), hpow),
         d = length(p) - length(q) then length(p) - length(q),
         hpow = 1 then if (eql(h, 1)) 1; p^(h, 1-(d)); end if, until %f)
      let g174862 = q;
      let g174863 = p/(prem(p, q), p*(g, p*(h, hpow)));
      p := g174862;
      q := g174863;
      #f;
      // 	(format  t "~%p=~s~%q=~s~% d=~s, h=~s" p q d h)
      if (coefzerop(q))
        //  poly remainder seq ended with zero
        // 	      (return (p* content p)) ;bug in version prior to 11/94
        return(p*(content, p/(p, pcontentsr(p))));
      end if;
      if (coefp(q) | var>(mainvar(p), mainvar(q)))
        //  the remainder sequence ended with non-zero, just
        // return the gcd of the contents.
        return(content);
      end if;
    end for;
  end block;
end method pgcd2sr;

//  this is a primitive prs, just like pgcd2sr but not as fast sometimes
define method pgcd2prim (u, v)
  if (size(v) > size(u))
    begin
      let g175001 = u;
      let g175000 = v;
      u := g175000;
      v := g175001;
      #f;
    end;
  elseif (nil);
  end if;
  let ucont = pcontentsr(u);
  let vcont = pcontentsr(v);
  block (return)
    for (c = p/(u, ucont) then d, d = p/(v, vcont) then ppartsr(prem(c, d)),
         until %f)
      if (coefzerop(d)) return(p*(pgcdsr(ucont, vcont), c)); end if;
      if (coefp(d) | var>(mainvar(c), mainvar(d)))
        return(pgcdsr(ucont, vcont));
      end if;
    end for;
  end block;
end method pgcd2prim;

//  pcontentsr: return the content of p (see Knuth, op. cit)
//  the content is the GCD of the coefficients of the top-level in the
//  polynomial p.
define method pcontentsr (p)
  if (coefzerop(p))
    p;
  elseif (coefp(p))
    coefone();
  else
    begin
      let len = size(p);
      for (i = 2 then 1+(i), g = svref(p, 1) then pgcdsr(g, svref(p, i)),
           until i = len | coefonep(g))
        #f;
      finally
        g;
      end for;
    end;
  end if;
end method pcontentsr;

//  pcontentxsr: return the gcd of u and the content of p
//  this is a handy time-saver
define method pcontentxsr (p, u)
  if (#t)
    for (i = 1-(length(p)) then 1-(i), g = u then pgcdsr(g, svref(p, i)),
         until coefonep(g) | i = 0)
      #f;
    finally
      g;
    end for;
  elseif (nil);
  end if;
end method pcontentxsr;

//  ppartsr: return the primitive part of p
define method ppartsr (p)
  if (coefzerop(p) | coefp(p)) p; else p/(p, pcontentsr(p)); end if;
end method ppartsr;

//  in some of the algorithms, the cofactors will be "free".  Here we're
//  just dividing the inputs by the gcd.
define method pgcd-cofac (u, v)
  let g = pgcdsr(u, v);
  values(g, p/(u, g), p/(v, g));
end method pgcd-cofac;

//  this program provides an ordering on polynomials, lexicographically.
//  it returns 'l (less than) 'e (equal)  or 'g (greater than).
//  this ordering is recursive by main variable.  There are other orderings,
//  like total degree ordering.
define method pcompare (u, v)
  if (u == v)
    #"e";
    //  quick check for equality
    //  if u and v are coefficients, then compare their numerical values.
    elseif (coefp(u))
    if (coefp(v))
      if (coef>(v, u)) #"l"; elseif (coef>(u, v)) #"g"; else #"e"; end if;
      // u is coefficient, v is not, so  u << v
      else
      #"l";
    end if;
    //  v is coefficient, u is not, so u >> v
    elseif (coefp(v))
    #"g";
    //  neither is a coefficient
    else
    begin
      let um = mainvar(u);
      let vm = mainvar(v);
      let ud = degree(u);
      let vd = degree(v);
      let ctest = #f;
      if (var>(um, vm))
        #"g";
        //  u has a more-main variable than v
        elseif (var>(vm, um))
        #"l";
        //  the reverse case
        elseif (ud > vd)
        #"g";
        //  u is higher degree  u >> v
        elseif (vd > ud)
        #"l";
        //  v is higher degree, so  v >> u
        //  Same main variable, same degree.
        //  Compare the coefficients, starting from the highest
        //  degree. The first one that differs determines
        //  which direction the comparison goes.
        else
        block (return)
          for (i = 1+(ud) then 1-(i), until i = 0)
            //  no differences found.  u = v
            ctest := pcompare(u[i], v[i]);
            if (ctest == #"e") #f; else return(ctest); end if;
          finally
            #"e";
          end for;
        end block;
      end if;
    end;
  end if;
end method pcompare;

//  compute the derivative wrt v, the encoding of a variable.
//  This assumes that there are no other dependencies on kernels, etc.
define method pderiv (p, v)
  local method pd1 (p)
          if (coefp(p))
            0;
          elseif (var>(v, p[0]))
            0;
          elseif (p[0] == v)
            r := make(<array>, dimensions: size(p) - 1);
            r[0] := v;
            for (i = 1-(length(p)) then 1-(i), until i = 1)
              r[i - 1] := p*(i - 1, p[i]);
            finally
              pnorm(r);
            end for;
          else
            r := copy-sequence(p);
            for (i = 1-(length(r)) then 1-(i), until i = 0)
              r[i] := pd1(r[i]);
            finally
              pnorm(r);
            end for;
          end if;
        end method pd1;
  pd1(p);
end method pderiv;

