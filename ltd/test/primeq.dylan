//  -*- Mode: LISP; Syntax: Common-lisp; Package: USER -*-
//  Modular Arithmetic
//  References
//    Robert Solovay and Volker Strassen,
//      "A Fast Monte-Carlo Test for Primality,"
//      SIAM Journal on Computing, 1977, pp 84-85.
//    R. L. Rivest, A. Shamir, and L Adleman
//      "A Method for Obtaining Digital Signatures and Public-Key Cryptosystems"
//      Communications of the ACM,
//      Vol 21, Number 2, February 1978, pages 120-126
//   (c) Copyright Gerald Roylance 1983, 1984, 1985, 1986
//       All Rights Reserved.
//   This file may be distributed noncommercially provided
//   that this notice is not removed.
//  modified slightly by RJF  2/5/91
//  Greatest Common Divisor Algorithms
//  find A and B such that
//     A * X + B * Y = Z
//  initial equations:
//     1 * X + 0 * Y = X
//     0 * X + 1 * Y = Y
define method gcd-extended (x, y)
  for (a1 = 1 then a2, b1 = 0 then b2, z1 = x then z2,
       a2 = 0 then a1 - d * a2, b2 = 1 then b1 - d * b2,
       z2 = y then z1 - d * z2, d = 0 then 0, until zero?(z2))
    d := floor/(z1, z2);
  finally
    values(a1, x, b1, y, z1);
  end for;
end method gcd-extended;

//  find b such that a * b is congruent to 1  (modulo modulus)
define method modinv (a, modulus)
  let (a1, x, b1, y, z1) = gcd-extended(a, modulus);
  if (z1 = 1)
    a1;
  else
    error("MODINV:  no inverse: modulus not prime");
  end if;
end method modinv;

//  (EXPT A N) MOD M
define method modpower (number, expon, modulus)
  for (exp = expon then floor(exp, 2),
       //  speedier to break into
       //   2**24 bit chunks?
       sqr(number, mod(sqr * sqr, modulus)) = nil then nil,
       ans = 1 then 1, until zero?(exp))
    if (odd?(exp)) ans := modulo(ans * sqr, modulus); end if;
  finally
    ans;
  end for;
end method modpower;

//  Generate a random bignum that is L bits long
//  generate random substrings of say 20 bits and
//  paste them together to make a longer random string
//  of course, this is a crock
define method random-big-1 (length)
  for (l = length then l - k,
       //  number of bits to make
       k(0) = nil then nil,
       //  number of bits to make this pass
       bits(0) = nil then nil,
       until //  rand bits so far
       l <= 0)
    k := min(l, 20);
    bits
     := bits * ash(1, k)
         + //  shift left k bits
        random-uniform(to: ash(1, k));
  finally
    bits;
  end for;
end method random-big-1;

//  Jacobi-Symbol
//  The hairy exponent stuff here is just a hack to look
//  at the lsbs of the bignums.  It has been hacked here
//  to make it moderately fast without bumming it beyond
//  recognition.
//  the Jacobi-Symbol is always +1, -1, or 0
//  (-1)**exp the easy way....
//
// LTD: No macros.
#"jacobi-expt-1";

//  version from Sussman's notes
//
define method jacobisymbol (p, q)
  let pp :: <integer> = modulo(p, 16);
  let qq :: <integer> = modulo(q, 16);
  //   P or Q where it matters
  if (p = 0)
    0;
    //  not in GJS notes
    elseif (p = 1)
    1;
  elseif (odd?(pp))
    jacobisymbol(modulo(q, p), p)
     * jacobi-expt-1(truncate/(((pp - 1) * (qq - 1)), 4));
    // was /
    else
    jacobisymbol(floor/(p, 2), q)
     * jacobi-expt-1(truncate/((qq * qq - 1), 8));
  end if;
end method jacobisymbol;

//  was /
//  Prime Number Test:  Solovay-Strassen
//  Solovay-Strassen Prime Test
//    if n is prime, then J(a,n) is congruent mod n to a**((n-1)/2)
//
define method prime-test-1 (a, n)
  gcd(a, n) = 1
   & modulo(jacobi-symbol(a, n) - modpower(a, floor/((n - 1), 2), n), n) = 0;
end method prime-test-1;

//  checks if n is prime.  Returns nil if not prime. True if (probably) prime.
//    probability of a mistake = (expt 2 (- trials))
//      choosing TRIALS=30 should be enough
//
define method primeq (n, #key trials = 30)
  n := abs(n);
  if (n < 2)
    #f;
  elseif (n = 2)
    #"true";
  elseif (n = 3)
    #"true";
  elseif (n > 100
           & //  cheap test
          ~ (1 = gcd(n, 223092870)))
    #f;
  else
    block (return)
      for (i = 0 then 1+(i), a = random(n) then random(n), until i > trials)
        if (zero?(a))
          //  this test is no good
          i := i - 1;
        elseif (~ prime-test-1(a, n))
          return(#f);
        end if;
      finally
        #"true";
      end for;
    end block;
  end if;
end method primeq;

