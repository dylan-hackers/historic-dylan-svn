module: secret-splitting
use-libraries: common-dylan, io, random
use-modules: common-dylan, format-out, random
author: Andreas Bogk <andreas@andreas.org>

define class <finite-field> (<number>)
  slot value :: <integer>, init-keyword: value:;
  slot modulus :: <integer>, init-keyword: modulus:;
end class <finite-field>;

define method same-field?(x :: <finite-field>, y :: <finite-field>)
 => (same? :: <boolean>)
  x.modulus = y.modulus;
end method same-field?;

define method \+(x :: <finite-field>, y :: <finite-field>)
 => (sum :: <finite-field>)
  if(same-field?(x, y))
    make(<finite-field>, 
         value: remainder(x.value + y.value, x.modulus),
         modulus: x.modulus)
  else
    error("Trying to add finite fields with different moduli.");
  end if;
end method;

define method \+(x :: <finite-field>, y :: <integer>)
 => (sum :: <finite-field>)
  make(<finite-field>, 
       value: remainder(x.value + y, x.modulus),
       modulus: x.modulus)
end method;

define method \+(x :: <integer>, y :: <finite-field>)
 => (sum :: <finite-field>)
  make(<finite-field>, 
       value: remainder(x + y.value, y.modulus),
       modulus: y.modulus)
end method;

define method \-(x :: <finite-field>, y :: <finite-field>)
 => (sum :: <finite-field>)
  if(same-field?(x, y))
    make(<finite-field>, 
         value: remainder(x.value - y.value, x.modulus),
         modulus: x.modulus)
  else
    error("Trying to subtract finite fields with different moduli.");
  end if;
end method;

define method \*(x :: <finite-field>, y :: <finite-field>)
 => (sum :: <finite-field>)
  if(same-field?(x, y))
    make(<finite-field>, 
         value: remainder(x.value * y.value, x.modulus),
         modulus: x.modulus)
  else
    error("Trying to multiply finite fields with different moduli.");
  end if;
end method;

define method \*(x :: <finite-field>, y :: <integer>)
 => (sum :: <finite-field>)
  make(<finite-field>, 
       value: remainder(x.value * y, x.modulus),
       modulus: x.modulus)
end method;

define method \*(x :: <integer>, y :: <finite-field>)
 => (sum :: <finite-field>)
  make(<finite-field>, 
       value: remainder(x * y.value, y.modulus),
       modulus: y.modulus)
end method;

define class <polynomial> (<object>)
  slot coefficients :: <collection>, 
    required-init-keyword: coefficients:;
end class <polynomial>;

define method order(p :: <polynomial>)
  p.coefficients.size
end method order;

define method evaluate(p :: <polynomial>, x :: <number>)
  for(i from 0 below p.order, 
      x* = 1 then x* * x,
      sum = 0 then sum + p.coefficients[i] * x*)
  finally
    sum
  end for;
end method evaluate;

define method choose-prime-larger-than(x :: <integer>)
 => (p :: <integer>)
  43;
end method choose-prime-larger-than;

define method shamir-setup(secret :: <integer>, 
                           number-of-users :: <integer>,
                           minimum-shares :: <integer>)
 => (shares :: <collection>)
  let p = choose-prime-larger-than(max(secret, number-of-users));
  local method ff(x :: <integer>)
          make(<finite-field>, value: x, modulus: p);
        end method ff;

  let a = map(method (x) ff(random(p)) end, 
              make(<range>, from: 1, to: number-of-users));

  a[0] := ff(secret);
  map(curry(evaluate, make(<polynomial>, coefficients: a)), 
      make(<range>, from: 0, to: number-of-users - 1));
end method shamir-setup;
 
define method shamir-decode(shares :: <collection>)
 => (secret :: <integer>)
  local method c(i)
          for(j from 0 below shares.size,
              prod = 1 then
                if(i = j) prod else
                  prod * (shares[j] / (shares[j] - shares[i]))
                end if)
          finally prod end for;
        end method c;

  for(i from 0 below shares.size,
      y in shares,
      secret = 0 then secret + c(i) * y)
  finally
    secret
  end for;
end method shamir-decode;

let shares = shamir-setup(23, 5, 3);
format-out("Shares: %=\n", shares);
let secret = shamir-decode(vector(shares[0],
                                  shares[1],
                                  shares[2]));
format-out("Secret: %=\n", secret);
