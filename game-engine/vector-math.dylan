module: vector-math

//define constant <3D-vector> = limited(<vector>, of: <float>, size: 3);
//define constant <3D-point>  = limited(<vector>, of: <float>, size: 4);

define constant <3D-rotation> = <vector>;  // 4 elements, first 3 are axis, 4th is rotation amount
define constant <3D-vector> = <vector>;
define constant <3D-point>  = <vector>;
define constant 3d-vector   = vector;
define constant 3d-point    = vector;
define constant 3d-rotation = vector;

// General vector math

// what's the general definition of the cross product in an
// n-dimensional room?
define inline method cross-product
    (u :: <vector>, v :: <vector>)
 => (cross-product :: <vector>)
  let result = make(type-for-copy(u), size: u.size);
  local method cp(i, j) u[i] * v[j] - v[i] * u[j] end;
  result[0] := cp(1, 2);
  result[1] := cp(2, 0);
  result[2] := cp(0, 1);
  result;
end method cross-product;

define inline method \+(u :: <vector>, v :: <vector>)
 => (sum :: <vector>)
  map(\+, u, v)
end method;

define inline method \-(u :: <vector>, v :: <vector>)
 => (difference :: <vector>)
  map(\-, u, v)
end method;

define inline method negate(u :: <vector>)
 => (negation :: <vector>)
  u * -1
end method;

define inline method \*(u :: <vector>, v :: <number>)
 => (product :: <vector>)
  map(rcurry(\*, v), u)
end method;

define inline method \*(u :: <number>, v :: <vector>)
 => (product :: <vector>)
  map(curry(\*, u), v)
end method;

define inline method \*(u :: <vector>, v :: <vector>)
 => (dot-product :: <number>)
  reduce1(\+, map(\*, u, v))
end method;

define inline method \/(u :: <vector>, v :: <number>)
 => (product :: <vector>)
  map(rcurry(\/, v), u)
end method;

define inline method \/(u :: <number>, v :: <vector>)
 => (product :: <vector>)
  map(curry(\/, u), v)
end method;

define inline method magnitude(v :: <vector>)
 => (length :: <number>)
  sqrt(v * v)
end method magnitude;

define inline method normalize(v :: <vector>)
 => (normalized-vector :: <vector>)
  v / magnitude(v)
end method normalize;

// useful geometric operations

define inline function proj(p :: <vector>, q :: <vector>)
  => (projection-of-p-on-q :: <vector>)
  ((p * q) / (q * q)) * q
end function proj;

define inline function perp(p :: <vector>, q :: <vector>)
  => (component-of-p-perpendicular-to-q :: <vector>)
  p - proj(p, q)
end function perp;

define method gram-schmidt-orthogonalization(basis :: <vector>)
 => (orthoginalized-basis :: <vector>)
  let new-basis = make(type-for-copy(basis), size: basis.size);

  new-basis[0] := basis[0];

  for(i from 1 below basis.size)
    new-basis[i] := basis[i] 
      - reduce1(\+, map(curry(proj, basis[i]), 
                        subsequence(basis, end: i - 1)));
  end for;
  new-basis
end method gram-schmidt-orthogonalization;
