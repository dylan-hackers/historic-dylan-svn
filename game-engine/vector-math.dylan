module: vector-math

define class <algebraic-vector> (<number>, <vector>)
  slot data;
end class <algebraic-vector>;

define inline method element(vector :: <algebraic-vector>, index) 
 => (element :: <number>)
  vector.data[index]
end method element;

define inline method element-setter
    (value, vector :: <algebraic-vector>, index) => (<number>)
  vector.data[index] := value;
end method element-setter;

define inline method size (vector :: <algebraic-vector>) 
 => (size :: false-or(<integer>));
  vector.data.size
end method size;

//define constant <3D-vector> = limited(<vector>, of: <float>, size: 3);
//define constant <3D-point>  = limited(<vector>, of: <float>, size: 4);

define constant <3D-rotation> = <simple-object-vector>;  // 4 elements, first 3 are axis, 4th is rotation amount
define constant <color> = <simple-object-vector>; // r,g,b
define constant <3D-vector> = <simple-object-vector>;
define constant <3D-point>  = <simple-object-vector>;
define constant 3d-vector   = vector;
define constant 3d-point    = vector;
define constant 3d-rotation = vector;
define constant color = vector;

// General vector math

// what's the general definition of the cross product in an
// n-dimensional room?
define inline method cross-product
    (u :: <simple-object-vector>, v :: <simple-object-vector>)
 => (cross-product :: <simple-object-vector>)
  let result = make(type-for-copy(u), size: u.size);
  local method cp(i, j) u[i] * v[j] - v[i] * u[j] end;
  result[0] := cp(1, 2);
  result[1] := cp(2, 0);
  result[2] := cp(0, 1);
  result;
end method cross-product;

define inline method \+(u :: <simple-object-vector>, v :: <simple-object-vector>)
 => (sum :: <simple-object-vector>)
  map(\+, u, v)
end method;

define inline method \-(u :: <simple-object-vector>, v :: <simple-object-vector>)
 => (difference :: <simple-object-vector>)
  map(\-, u, v)
end method;

define inline method negate(u :: <simple-object-vector>)
 => (negation :: <simple-object-vector>)
  u * -1
end method;

define inline method \*(u :: <simple-object-vector>, v :: <number>)
 => (product :: <simple-object-vector>)
  map(rcurry(\*, v), u)
end method;

define inline method \*(u :: <number>, v :: <simple-object-vector>)
 => (product :: <simple-object-vector>)
  map(curry(\*, u), v)
end method;

define inline method \*(u :: <simple-object-vector>, v :: <simple-object-vector>)
 => (dot-product :: <number>)
  reduce1(\+, map(\*, u, v))
end method;

define inline method \*(m :: <matrix>, v :: <simple-object-vector>)
 => (product :: <simple-object-vector>)
  let d = dimensions(m);
  let result = make(type-for-copy(v), size: v.size, fill: 0.0);
  for(column from 0 below d[0])
    for(row from 0 below d[1])
      result[row] := result[row] + m[column, row] * v[column];
    end for;
  end for;
  result;
end method;

define inline method \/(u :: <simple-object-vector>, v :: <number>)
 => (product :: <simple-object-vector>)
  map(rcurry(\/, v), u)
end method;

define inline method \/(u :: <number>, v :: <simple-object-vector>)
 => (product :: <simple-object-vector>)
  map(curry(\/, u), v)
end method;

define inline method magnitude(v :: <simple-object-vector>)
 => (length :: <number>)
  sqrt(v * v)
end method magnitude;

define inline method normalize(v :: <simple-object-vector>)
 => (normalized-vector :: <simple-object-vector>)
  v / magnitude(v)
end method normalize;

// useful geometric operations

define inline function proj(p :: <simple-object-vector>, q :: <simple-object-vector>)
  => (projection-of-p-on-q :: <simple-object-vector>)
  ((p * q) / (q * q)) * q
end function proj;

define inline function perp(p :: <simple-object-vector>, q :: <simple-object-vector>)
  => (component-of-p-perpendicular-to-q :: <simple-object-vector>)
  p - proj(p, q)
end function perp;

define method gram-schmidt-orthogonalization(basis :: <simple-object-vector>)
 => (orthoginalized-basis :: <simple-object-vector>)
  let new-basis = make(type-for-copy(basis), size: basis.size);

  new-basis[0] := basis[0];

  for(i from 1 below basis.size)
    new-basis[i] := basis[i] 
      - reduce1(\+, map(curry(proj, basis[i]), 
                        subsequence(basis, end: i - 1)));
  end for;
  new-basis
end method gram-schmidt-orthogonalization;

define method matrix-from-column-vectors(#rest col-vectors) => mat :: <matrix>;
    matrix(apply(map, vector, col-vectors));
end method matrix-from-column-vectors;

define method uniform-scale(s :: <float>, #key dimensions = #[3, 3])
  let mat = make(<matrix>, dimensions: dimensions, fill: 0.0);
  for(i from 0 below dimensions[0])
    mat[i][i] := s;
  end for;
end method uniform-scale;

define method nonuniform-scale(#rest scales)
  let mat = make(<matrix>, dimensions: vector(scales.size, scales.size));
  for(i from 0 below scales.size)
    mat[i][i] := scales[i];
  end for;
end method nonuniform-scale;

define method rotate-x(angle :: <float>) => (mat :: <matrix>);
  let s = sin(angle);
  let c = cos(angle);
  matrix(vector(1.0, 0.0, 0.0),
         vector(0.0,   c,  -s),
         vector(0.0,   s,   c));
end method rotate-x;

define method rotate-y(angle :: <float>) => (mat :: <matrix>);
  let s = sin(angle);
  let c = cos(angle);
  matrix(vector(  c, 0.0,   s),
         vector(0.0, 1.0, 0.0),
         vector( -s, 0.0,   c));
end method rotate-y;

define method rotate-z(angle :: <float>) => (mat :: <matrix>);
  let s = sin(angle);
  let c = cos(angle);
  matrix(vector(  c,  -s, 0.0),
         vector(  s,   c, 0.0),
         vector(0.0, 0.0, 1.0));
end method rotate-z;

define method rotate(v :: <3d-vector>, angle :: <float>) => (mat :: <matrix>);
  let s = sin(angle);
  let c = cos(angle);
  let c* = 1.0 - c;
  matrix(vector(v[0] * v[0] * c* + c, 
                v[0] * v[1] * c* - v[2] * s, 
                v[0] * v[2] * c* + v[1] * s),
         vector(v[0] * v[1] * c* + v[2] * s,
                v[1] * v[1] * c* + c,
                v[1] * v[2] * c* - v[0] * s),
         vector(v[0] * v[2] * c* - v[1] * s,
                v[1] * v[2] * c* + v[0] * s,
                v[2] * v[2] * c* + c));
end method rotate;

