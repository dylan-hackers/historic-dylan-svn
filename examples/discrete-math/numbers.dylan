module: numbers
author: Hannes Mehnert <hannes@mehnert.org>

/*
define abstract class <number> (<object>);

define abstract class <complex> (<number>);

define abstract class <real> (<complex>);

define abstract class <rational> (<real>)
define class <ratio> (<rational>)
define class <single-float> (<rational>)
define class <double-float> (<rational>)
define class <extended-float? (<rational>)

define abstract class <integer> (<rational>)
define class <single-integer> (<integer>)
define class <double-integer> (<integer>)
define class <extended-integer> (<integer>)

define class <natural-integer> (<integer>)

define class <ring> (<object? group?>)
define class <finite-field> (<ring>)

*/
// euclids algorithm
// buchmann einfuehrung in die kryptographie: p.13
define method gcd (a :: <integer>, b :: <integer>)
  => (result :: <integer>)
  let tmp :: <integer> = 0; 
  let a :: <integer> = abs(a);
  let b :: <integer> = abs(b);
  while (b ~= 0)
    tmp := modulo(a, b);
    a := b;
    b := tmp;
  end while;
  a;
end method gcd;

/*
// handbook of applied cryptographie: 2.89, p.64
define method lcm (a :: <integer>, b :: <integer>)
 => (result :: <integer>)
  a * b / gcd(a, b)
end method lcm;
*/

define constant <vector-of-integers> = limited(<simple-vector>, of: <integer>);

// extended euclids algorithm
// input: a, b
// output: gcd = x * a + y * b 
// handbook of applied cryptography: algorithm 2.107, p.67
define method extended-euclid (a :: <integer>, b :: <integer>)
  => (gcd :: <integer>, x :: <integer>, y :: <integer>)
  //validate input
  unless (positive?(a) & positive?(b))
    error("Error: Both arguments have to be > 0!");
  end unless;
  unless (a >= b)
    error("Error: a >= b!");
  end unless;
  let x :: <integer> = 0;
  let y :: <integer> = 0;
  let gcd :: <integer> = 0;
  let tmpa :: <integer> = a;
  let tmpb :: <integer> = b;
  //2
  let xs :: <vector-of-integers> = make(<vector-of-integers>, size: 2);
  xs[0] := 0; xs[1] := 1;
  let ys :: <vector-of-integers> = make(<vector-of-integers>, size: 2);
  ys[0] := 1; ys[1] := 0;
  while (tmpb > 0)
    //3.1
    let (quotient, remainder) = truncate/(tmpa, tmpb);
    x := xs[1] - quotient * xs[0];
    y := ys[1] - quotient * ys[0];
    //3.2
    tmpa := tmpb;
    tmpb := remainder;
    xs[1] := xs[0];
    xs[0] := x;
    ys[1] := ys[0];
    ys[0] := y;
  end while;
  //4
  x := xs[1];
  y := ys[1];
  gcd := tmpa;
  unless (gcd = x * a + y * b)
    error("Error: gcd != x * a + y * b\n%d != %d * %d + %d * %d\n",
           gcd, x, a, y, b);
  end unless;
  values(gcd, x, y);
end method extended-euclid;

//handbook of applied cryptography: 14.4, p.593
// input: integer, base
// output: list (an..a0): an * base^n + a(n-1) * base ^ (n-1) + ... + a0 
define method base-n-representation (integer :: <integer>, base :: <integer>)
 => (res :: <list>)
  //integer >= 0
  if (negative?(integer))
    error("Error: Trying to represent negative integer %d", integer);
  end if;
  //base > 0
  unless (positive?(base))
    error("Error: Trying to represent integer in base %d < 2!", base);
  end unless;
  local method collect (value :: <integer>, digits :: <list>)
   => (digits :: <list>)
    let (quotient :: <integer>, remainder :: <integer>)
      = floor/(value, base);
    let digits = pair(remainder, digits);
    if (zero?(quotient))
      digits;
    else
      collect(quotient, digits)
    end if;
  end method collect;
  collect(integer, #());
end method base-n-representation;

//handbook of applied cryptography, 2.149, p.73
define method jacobi (a :: <integer>, n :: <integer>)
 => (jacobi-symbol :: <integer>)
  if (negative?(a))
    error("Only defined for a >= 0.");
  end if;
  unless (a < n)
    error("Only defined for a < n.");
  end unless;
  unless (odd?(n))
    error("n must be odd!");
  end unless;
  //1
  if (a = 0)
    0;
  //2
  elseif (a = 1)
    1;
  else
    //3
    let e :: <integer> = 0;
    let a1 :: <integer> = a;
    while (even?(a1))
      e := e + 1;
      a1 := ash(a1, -1);
    end while;
    //4
    let s :: <integer> = 0;
    if (even?(e))
      s := 1;
    else
      let tmp = modulo(n, 8);
      case
        tmp = 1 => s := 1;
        tmp = 7 => s := 1;
        tmp = 3 => s := -1;
        tmp = 5 => s := -1;
      end case;
    end if;
    //5
    if (modulo(n, 4) = 3 & modulo(a1, 4) = 3)
      s := -s;
    end if;
    //6
    let n1 :: <integer> = modulo(n, a1);
    //7
    if (a1 = 1)
      s;
    else
      s * jacobi(n1, a1);
    end if;
  end if;
end method jacobi;

// chinese remainder theorem
// phi(n)
// mult, order, generator for ring
// factorize
// isprime (miller-rabin)
// prime generator
// rng
// wooping
