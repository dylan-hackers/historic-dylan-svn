module: dylan-user
author: Hannes Mehnert <hannes@mehnert.org>

define library numbers
  use common-dylan;
  use dylan;
  use io;
  export numbers;
end library;

define module numbers
  use common-dylan, exclude: { gcd };
  use format-out;
  use extensions, exclude: { value };
  export
    extended-euclid,
    base-n-representation,
    gcd,
    inverse,
    jacobi,
    <finite-field>,
    value,
    value-setter,
    <polynomial>,
    coefficients,
    coefficients-setter,
    degree,
    evaluate,
    trial-division,
    pollard-rho;
end module;
