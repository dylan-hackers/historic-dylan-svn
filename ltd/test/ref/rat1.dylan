//  -*- Mode:Common-Lisp; Package:RAT; Base:10 -*-
// 
//  Written by: Tak W. Yan, Richard J. Fateman
//  File: rat.lisp
//  Contains: rational function (in partially-factored form) arithmetic
// LTD: Function PROVIDE not yet implemented.
provide(#"rat1");

//  (c) Copyright 1990, 1991 Richard J. Fateman, Tak W. Yan
//  A fpe is a (possibly) factored polynomial expression implemented
//  as a list of pairs of polynomials and exponents. For example,
// 
//                              2  2         4
//                    12 (x + 1) (y  + y - 5)
// 
//  is represented as a list of three pairs, the first pair being the
//  the number 12 and the exponent 1, the second being the 
//  polynomial (x + 1) and the exponent 2, and the third being the
//  polynomial (y^2 + y - 5) and the exponent 4.
//  Currently, the first pair is always  an integer with exponent 1.
//  Although the polynomials could have coefficients in any ring
//  for some operations, the idea of gcd as used here assumes coefs form a
//  unique factorization domain, and therefore it is principally useful
//  to have integer coefficients.  (Rationals are not a UFD since
//   1/2*2 = 1/3*3 = 1 etc.)
//  Getting back to our representation --
//  There is a first pair that always looks like (integer . 1).
//  Subsequent pairs consist of
//  distinct polynomials with integer coefficients, and
//  exponents which are integers >= 1.
//  It is not required that polys  be squarefree, monic, irreducible
//  or restricted in some other ways.  Examples of possible pairs:
//   ( (x^2) . 2)  ;   ( (3 x^2 - 3 x) . 1)
//   actually, the polynomials will be in some better form, so it is
//  more accurate to depict these examples as 
//   ( #(1 0 0 1) . 2)  ;  ( #(1 0 -3 3) . 1)   where x <--> 1
//  The polynomials in an fpe, except for intermediate forms produced
//  during gcd computation, are maintained in lexicographically
//  increasing order. Note that two polynomials in fpe form 
//  may not be IDENTICAL even though they are mathematically
//  equivalent, because of different factorings.
//  For example,  ((1 . 1) ( (x^2+2x+1) . 1))  and ((1 . 1) ((x+1) . 2))
//  are equivalent but not identical.  If they were added together,
//  the result would be  (if we refrained from computing
//  gcds)
//   ((1 . 1) ((2x^2+4*x+2) . 1))
//  Another equivalent answer (more effort to compute .. we don't do
//  this...) would get
//   ((2 . 1) ((x+1) . 2))
//  On the other hand, if we were adding ((1 . 1) ((x+1) . 2)) and
//  ((1 . 1) ((x+1) . 3)), the common factor of x+1 would be obvious
//  and we would get  ((1 . 1) ((x+1) . 2) ((x+2) . 1))
#f;

// LTD: Function REQUIRE not yet implemented.
require(#"poly");

//  need macro defs
"(in-package mma)";

//  export what other modules may need
// 
//  default is to compute the gcd, but only between factors of
//  the numerator with factors of the denominator.
nil(#f, #());

define variable *expand* = #f;

//  other possible settings:
//   cdi: just find the obvious common factors by identity. That is
//   u and v have a common divisor iff u = v.
//  cdd: common divisor by division.  That is, u and v have a
//  common divisor if one divides the other exactly.
//  current implementation uses gcd as default.
//  NOT IMPLEMENTED techniques:
//  supergcd: all gcds of factors in numerator OR denominator are
//  considered for pair-wise gcds.  That is, (x^2-1)*(x+1) --> (x+1)^2*(x-1)
//  square-free: all factors are run through a square-free factorization
//  program. (they are tagged as square-free to save redundant calculations).
//  factors: all are factored over the integers. (also tagged)
//  make-fpe: make a "normal" fpe p^e from a polynomial p, exponent e>=0
//  examples:
//   (make-fpe poly 4) -->  (( 1 . 1) (poly 4)) ;;usual case
//   (make-fpe 0 1)    -->  (( 0   . 1))
//    by virtue of using fpe-insert with monomial flag = t, if
//    make-fpe is given something like p = 3x^2y^3, e=2, it will
//    produce ((9 . 1) ( #(x 0 1) . 4) (#(y 0 1) . 6)).
//    {actually, you won't see x and y, but some number-encoding of them.}
define method make-fpe (p, e)
  if (coefp(p))
    list(pair(coef^(p, e), 1));
  else
    fpe-insert(p, e, #(#(1 . 1)), #t);
  end if;
end method make-fpe;

define method poly2rat (p, e)
  if (e >= 0)
    make-rat(numerator: make-fpe(p, e), denominator: list(#(1 . 1)));
  else
    make-good-rat(list(#(1 . 1)), make-fpe(p, - e));
  end if;
end method poly2rat;

//  fpe-copy: return a copy of the fpe u
define method fpe-copy (u)
  // LTD: Function COPY-TREE not yet implemented.
  copy-tree(u);
end method fpe-copy;

//  fpe-expand: expand the fpe into a fully-expanded form; the result
//              is returned as a polynomial
define method fpe-expand (u)
  fluid-bind (*expand* = #t)
    for (ul = u then cdr(ul), p = 1 then 1, until empty?(ul))
      p := p*(p, p^(head(head(ul)), tail(head(ul))));
    finally
      p;
    end for;
  end fluid-bind;
end method fpe-expand;

//  A fpe is "normal" if its first factor is an integer and it has
//  no other integer factors. Also, the coefficient of all
//  monomial factors should be 1. (reason: they look odd otherwise...
//  consider 3*x^2*4*y^2  instead of 12*x^2*y^2)
//  The factors are unique, and
//  sorted. All coefficients in the domain
//  are represented as c^1, including coefzero and coefone.
//  fpe-norm: normalize a fpe
//  this program could be reprogrammed to do much less consing.
define method fpe-norm (u)
  local method fn1 (ul)
          //  aux function to sort terms.
          if (empty?(ul))
            #f;
          elseif (coefp(head(head(ul))))
            //  stick coefs up front
            const := coef*(const, coef^(head(head(ul)), tail(head(ul))));
            fn1(tail(ul));
          else
            fn1-insert(head(ul), fn1(tail(ul)));
          end if;
        end method fn1,
        method fn1-insert (h, ul)
          if (empty?(ul))
            //  Since factor (car h) is nowhere to be seen further in
            //  this fpe, make the pair of this factor and its power
            //  appear at the end of the list.
            pair(h, ul);
          elseif (#"e" == (ctest := pcompare(head(h), head(head(ul)))))
            //  We found this factor. Increment the exponent
            pair(pair(head(h), tail(h) + tail(head(ul))), tail(ul));
            //  Otherwise we keep looking for the factor.
            elseif (#"l" == ctest)
            //  this poly is less in ordering that (caar ul),
            //  so place it right here.
            pair(h, ul);
            //  This keep searching
            else
            pair(head(ul), fn1-insert(h, tail(ul)));
            ul;
          end if;
        end method fn1-insert;
  //  end of local fns
  u := fn1(u);
  pair(pair(const, 1), u);
end method fpe-norm;

define method fpe-coef-p (u) empty?(tail(u)); end method fpe-coef-p;

define method fpe-coefone-p (u)
  empty?(tail(u)) & coefonep(head(head(u)));
end method fpe-coefone-p;

define method fpe-coefzero-p (u)
  coefzerop(head(head(u)));
end method fpe-coefzero-p;

//  fpe-insert: insert a pair of polynomial and exponent into the fpe u.
//              This checks to see whether the polynomial already exists in
//              the fpe; if yes, increment the exponent; otherwise
//              adds the pair to the fpe. This will not change u.
//  in effect, this multiplies u in fpe form, by poly^u. 
//  compare to fpe-*, which multiplies 2 polys in fpe form.
//  if monom is t, then maybe poly is a monomial. Check it
//  and insert it in pieces if appropriate.
define method fpe-insert (poly, exp, u, #key monom = #f)
  local method fi1 (ul)
          //  this routine recurses down the list of factors
          if (empty?(ul))
            //  Since this factor is nowhere to be seen in
            //  this fpe, insert the new factor and its power
            //  at the end of the list.
            list(pair(poly, exp));
          elseif (#"e" == (ctest := pcompare(poly, head(head(ul)))))
            //  We found this factor. Increment the exponent.
            pair(pair(head(head(ul)), exp + tail(head(ul))), tail(ul));
            //  Otherwise we keep looking for the factor.
            elseif (#"l" == ctest)
            //  this poly is less in ordering than (caar ul),
            //  so place it right here.
            pair(pair(poly, exp), ul);
            //  This keep searching
            else
            pair(head(ul), fi1(tail(ul)));
          end if;
        end method fi1;
  if (coefp(poly))
    //  inserting an integer factor: modify the first element
    if (coefonep(poly))
      u;
    elseif (coefzerop(poly))
      make-fpe(0, 1);
    else
      pair(pair(coef*(head(head(u)), coef^(poly, exp)), 1), tail(u));
    end if;
  elseif (exp = 0)
    u;
    // multiplying u by z^0 gives u
    elseif (monom & monomialp(poly)
             & (degree(poly) > 1 | ~ coefonep(lc(poly))))
    //  the poly is something like (3*x^2) ^4
    //  insert 81  (i.e. 3^4) into  1*(x)^8.
    //  should work recursively for (3*y^2*x^2)^4
    fpe-insert(lc(poly), exp,
               fpe-insert(vector(poly[0], 0, 1), degree(poly) * exp, u),
               monom);
    //  we can, more generally, not insist on monomial,
    //  but only 0 x^1 term.  That is,
    //  (a*x^5+b*x^2) --> x^2*(a*x^3+b).  or
    //  (x^5+x^2)^3 --> x^6*(x^3+1)^3. etc.
    //  note that 0 constant term is not useful because
    //   x is encoded as (vector n 0 1) and has 0 const term
    elseif (monom & ~ *expand* & coefzerop(constc(poly)) & size(poly) > 3
             & coefzerop(poly[2]))
    for (i = 2 then 1+(i), until empty?(coefzerop(poly[i])))
      #f;
    finally
      // there's a non-0
      fpe-insert(vector(poly[0], 0, 1), exp * (i - 1),
                 //  the degree of the factor
                 fpe-insert(polyshift(poly, i - 1), exp, u));
    end for;
  else
    fi1(u);
  end if;
end method fpe-insert;

define method polyshift (p, i)
  let z = copy-sequence(p, i);
  z[0] := p[0];
  z;
end method polyshift;

//  fpe-*  multiplies two fpe's u and v. To be canonical,
// identical polynomials will appear only once in the result.
//  Both u and v are preserved.  No wasted conses.
define method fpe-* (u, v)
  local method fm1 (ul, vl)
          if (empty?(ul))
            vl;
          elseif (empty?(vl))
            ul;
          elseif (#"e" == (ctest := pcompare(head(head(ul)), head(head(vl)))))
            pair(pair(head(head(ul)), tail(head(ul)) + tail(head(vl))),
                 fm1(tail(ul), tail(vl)));
          elseif (#"l" == ctest)
            //  (caar ul) is less in ordering that (caar vl),
            //  so place it right here.
            pair(head(ul), fm1(tail(ul), vl));
            //  Otherwise (car vl) goes here
            else
            pair(head(vl), fm1(ul, tail(vl)));
          end if;
        end method fm1;
  if (empty?(tail(u)) & coefonep(head(head(u))))
    v;
    //  u is coefone
    elseif (empty?(tail(v)) & coefonep(head(head(v))))
    u;
    //  v is coefone
    else
    pair(pair(coef*(head(head(u)), head(head(v))), 1), //  mult consts.
         fm1(tail(u), tail(v)));
  end if;
end method fpe-*;

define method fpe-^ (a, n)
  // powering an fpe is "easy"
  //  presumably n is a positive integer, although if it is some
  //  other number, this program won't break.
  local method fp^1 (a)
          if (empty?(a))
            #f;
          else
            pair(pair(head(head(a)), n * tail(head(a))), fp^1(tail(a)));
          end if;
        end method fp^1;
  pair(pair(head(head(a)) ^ n, 1), fp^1(tail(a)));
end method fpe-^;

define method fpe-negative-p (u)
  coef-negative-p(head(head(u)));
end method fpe-negative-p;

define method fpe-negate (u)
  pair(pair(coefneg(head(head(u))), tail(head(u))), tail(u));
end method fpe-negate;

//  These procedures are concerned with finding the gcd and cd of fpe's.
//  They are independent of the implementation of fpe's.
//  fpe-gcd-cofac: 
//  returns 3 values: g= the gcd of u and v;
//                    ubyg = u/g;
//                    vbyg = v/g.
//  u and v are unchanged, though any of the outputs may share structure.
//  all values are fpe in normal form.
define method fpe-gcd-cofac (u, v)
  begin u := concatenate(u, #f); v := concatenate(v, #f); end;
  let g = #(#(1 . 1));
  //   g= gcd = 1, initially
  for (i = u then cdr(i),
       until //  for each polynomial in u do the following
             //  until we run off the end of u. Then return the triple 
             //  <gcd, u/gcd, v/gcd>.
       empty?(i))
    if (~ coefonep(head(head(i))))
      //  do this only if (caar i) is not 1
      for (j = v then cdr(j),
           until //  for each polynomial in v do the following
           empty?(j))
        if (~ coefonep(head(head(j))))
          let up = head(head(i));
          let vp = head(head(j));
          let ue = tail(head(i));
          let ve = tail(head(j));
          let (gp, uq, vq)
              = // gcd, up/gp, vp/gp
              pgcdswitch-cofac(up, vp);
          let _that = #f;
          if (_that := coefonep(gp))
            _that;
            //  no non-trivial divisor! just advance i,j.
            else
            //  ok, we've found a common factor gp
            begin
              let ge = min(ue, ve);
              //  gcd is really gp^ge
              //  add the factor to the ones already found.
              //  incidentally, this factor may already be
              //  in g because of some earlier discovered gcd.
              g := fpe-insert(gp, ge, g, #t);
              //  g = g*gp^ge
              //  blot out the two factors in u and v.
              //  we may have to put some remnants at the end, though.
              //  splice in what's left of powers of up and powers
              //  of vp, if any, and then consider up/gp and vp/gp
              //  possible missed factorization here.. if we compute
              //  gcd of x^2-1 and (x^2*y^2-y^2)^3 we get a 
              //  co-factor of y^2.  This is a monomial. It would be
              //  better to put y^6 on the factor list, rather than
              //  (y^2)^3.  Could check in fpe-norm for monomials,
              //  but this would add to expense.
              if (ge = ue)
                head(i) := pair(uq, ue);
                up := uq;
              else
                //  that is, ge < ue
                //  first decrement the exponent on this factor
                //  to make it reflect the number of times
                //  we've divided out gp
                head(i) := pair(gp, ue - ge);
                up := gp;
                if (coefonep(uq))
                  #f;
                else
                  concatenate!(i, list(pair(uq, ue)));
                end if;
              end if;
              if (ge = ve)
                head(j) := pair(vq, ve);
                vp := vq;
              else
                //  that is, ge < ve
                head(j) := pair(gp, ve - ge);
                vp := gp;
                if (coefonep(vq))
                  #f;
                else
                  concatenate!(j, list(pair(vq, ve)));
                end if;
              end if;
            end;
          end if;
        end if;
      end for;
    end if;
  finally
    values(g, fpe-norm(u), fpe-norm(v));
  end for;
end method fpe-gcd-cofac;

define method pgcdswitch-cofac (u, v)
  select (*rat-gcd-level*)
    #"gcd"
       => // compute the gcd. Which algorithm depends perhaps
          //  on other switches in polynomial package.
          /pgcd-cofac(u, v);
    #"cdi"
       => //  look for identical common factors only
          //  equalp checks element by element in arrays
          /if (u = v) values(u, 1, 1); else values(1, u, v); end if;
    #"cdd"
       => let r = #f;
           if (r := p/-test(u, v))
             values(v, r, 1);
           elseif (r := p/-test(v, u))
             values(u, 1, r);
           else
             values(1, u, v);
           end if;
    #"t"
       => //  use gcd as default
           pgcd-cofac(u, v);
  end select;
end method pgcdswitch-cofac;

//  A rat is a rational polynomial implemented as a structure
//  consisting of two fpe's: the numerator and the denominator.
define class <rat> (<object>)
  slot rat-numerator, init-keyword: #"rat-numerator";
  slot rat-denominator, init-keyword: #"rat-denominator";
end class <rat>;

//  A "good" rat is one whose denominator is always positive.
//  If the rat as a whole is negative, the negative sign
//  is in the numerator.
//  make-good-rat
define method make-good-rat (n, d)
  if (fpe-negative-p(d))
    d := fpe-negate(d);
    n := fpe-negate(n);
  elseif (nil);
  end if;
  make-rat(numerator: n, denominator: d);
end method make-good-rat;

//  The following procedures perform simple arithmetic on rats.
//  The returned rats from these procedures will be "normal,"
//  provided that the arguments are "normal." A rat is said to
//  be "normal" if
//        1) it is in reduced form (no common factors between numerator
//           and denominator).
//        2) each polynomial in a fpe that makes up the numerator
//           or the denominator appears only once within that rat.
//        3) the fpe's are themselves normal: leading coefficient followed
//           by terms sorted etc.
//  rat*: non-destructively multiply two rats r1 and r2
//        reference - W. S.  Brown's paper
define method rat* (r1, r2)
  block (return-from-rat*)
    let a = r1.rat-numerator;
    let b = r1.rat-denominator;
    let c = r2.rat-numerator;
    let d = r2.rat-denominator;
    let g = #f;
    let num = #f;
    let den = #f;
    //  we assume that gcd(a,b)=1, and also that gcd(c,d)=1.
    //  This may not be, strictly speaking, true, depending on *rat-gcd-level*.
    if (fpe-coefzero-p(a) | fpe-coefzero-p(c))
      return-from-rat*(poly2rat(0, 1));
    end if;
    let (#rest _num, _a, _c) = fpe-gcd-cofac(a, c);
    num := _num;
    a := _a;
    c := _c;
    let (#rest _den, _b, _d) = fpe-gcd-cofac(b, d);
    den := _den;
    b := _b;
    d := _d;
    let (#rest _g, _a, _d) = fpe-gcd-cofac(a, d);
    g := _g;
    a := _a;
    d := _d;
    let (#rest _g, _b, _c) = fpe-gcd-cofac(b, c);
    g := _g;
    b := _b;
    c := _c;
    //  put back the factors that were removed
    a := fpe-*(a, fpe-^(num, 2));
    b := fpe-*(b, fpe-^(den, 2));
    //  finally put the pieces together
    make-good-rat(fpe-*(a, c), fpe-*(b, d));
  end block;
end method rat*;

//  division. almost same as *.
define method rat/ (r1, r2)
  block (return-from-rat/)
    let a = r1.rat-numerator;
    let b = r1.rat-denominator;
    let d = r2.rat-numerator;
    let c = r2.rat-denominator;
    let g = #f;
    let num = #f;
    let den = #f;
    if (fpe-coefzero-p(a)) return-from-rat/(poly2rat(0, 1)); end if;
    if (fpe-coefzero-p(d)) error("Rational division by zero"); end if;
    let (#rest _num, _a, _c) = fpe-gcd-cofac(a, c);
    num := _num;
    a := _a;
    c := _c;
    let (#rest _den, _b, _d) = fpe-gcd-cofac(b, d);
    den := _den;
    b := _b;
    d := _d;
    let (#rest _g, _a, _d) = fpe-gcd-cofac(a, d);
    g := _g;
    a := _a;
    d := _d;
    let (#rest _g, _b, _c) = fpe-gcd-cofac(b, c);
    g := _g;
    b := _b;
    c := _c;
    a := fpe-*(a, fpe-^(num, 2));
    b := fpe-*(b, fpe-^(den, 2));
    make-good-rat(fpe-*(a, c), fpe-*(b, d));
  end block;
end method rat/;

//  rat+: non-destructively add two rats r1 and r2
define method rat+ (r1, r2)
  block (return-from-rat+)
    let a = r1.rat-numerator;
    let b = r1.rat-denominator;
    let c = r2.rat-numerator;
    let d = r2.rat-denominator;
    let n = make-fpe(coefone(), 1);
    let num = #f;
    let den = #f;
    let g = #f;
    if (fpe-coefzero-p(a)) return-from-rat+(r2); end if;
    if (fpe-coefzero-p(c)) return-from-rat+(r1); end if;
    let (#rest _num, _a, _c) = fpe-gcd-cofac(a, c);
    num := _num;
    a := _a;
    c := _c;
    let (#rest _den, _b, _d) = fpe-gcd-cofac(b, d);
    den := _den;
    b := _b;
    d := _d;
    //  now r1+r2 = (num/den)* (a/b+c/d) and gcd(b,d)=1.
    //  n=ad+bc
    n
     := pnorm(p+(fpe-expand(fpe-*(a, d)), //   n is a polynomial
                 fpe-expand(fpe-*(b, c))));
    if (coefzerop(n))
      return-from-rat+(make-rat(numerator: #(#(0 . 1)),
                                denominator: #(#(1 . 1))));
    end if;
    n := make-fpe(n, 1);
    //  now n is an fpe.
    den := fpe-*(den, fpe-*(b, d));
    let (#rest _g, _n, _den) = fpe-gcd-cofac(n, den);
    g := _g;
    n := _n;
    den := _den;
    num := fpe-*(num, n);
    //  set num to num *(ad + bc)
    make-good-rat(num, den);
  end block;
end method rat+;

//  rat^: raise r to the power e
define method rat^ (r, e)
  if (e = 0)
    make-rat(numerator: #(#(1 . 1)), denominator: #(#(1 . 1)));
  elseif (e > 0)
    make-rat(numerator: fpe-^(r.rat-numerator, e),
             denominator: fpe-^(r.rat-denominator, e));
  else
    if (fpe-coefzero-p(r.rat-numerator))
      error("Rational division by zero");
    end if;
    make-good-rat(// check sign of denom..
                  fpe-^(r.rat-denominator, - e), fpe-^(r.rat-numerator, - e));
  end if;
end method rat^;

