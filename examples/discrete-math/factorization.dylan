module: numbers
author: Hannes Mehnert <hannes@mehnert.org>


define method trial-division
 (x :: <integer>, upper-limit :: <integer>)
 => (factors :: <list>)
//  if (upper-limit = #f)
//    upper-limit = sqrt(x);
//  end if;

  let primes = generate-primes(upper-limit);

  let res :: <list> = #();
  let i :: <integer> = 0;
  while (i < primes.size & x > 1)
    let (quo, rem) = truncate/(x, primes[i]);
    if (rem = 0)
      res := pair(primes[i], res);
      x := quo;
    else
      i := i + 1;
    end if;
  end while;
  res;
end method trial-division;

define method generate-primes (upper-limit :: <integer>)
 => (primes :: <list>)
  let primes :: <list> = make(<list>, size: upper-limit);

  //initialize primes with 2,3,4,5,6,7,8.9,10,11,..,uppper-limit.
  primes[0] := 0;
  for (i from 1 below primes.size)
    primes[i] := i + 1;
  end for;

  //filter all multiples of primes
  for (i from 0 below truncate/(primes.size, 2))
    if (primes[i] ~= 0)
      for (j from i + primes[i] below primes.size by primes[i])
        primes[j] := 0;
      end for;
    end if;
  end for;

  //remove all zeros
  tail(remove-duplicates(primes));
end method generate-primes;

//handbook applied cryptography: 3.9 p.91
define method pollard-rho (n :: <integer>)
  => (d :: false-or(<integer>))
  //1
  let a :: <integer> = 2;
  let b :: <integer> = 2;
  let d :: <integer> = 1;
  //2
  while (d = 1 & d < n)
    //2.1
    a := modulo(a * a + 1, n);
    b := modulo(b * b + 1, n);
    b := modulo(b * b + 1, n);
    
    //2.2
    d := gcd(a - b, n);

    //2.3: integrated in while
    format-out("%d %d %d\n", a, b, d);
  end while;
  //2.4
  if (d = n)
    #f;
  else
    d;
  end if;
end method pollard-rho;