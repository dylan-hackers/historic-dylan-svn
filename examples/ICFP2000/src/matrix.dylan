module: icfp2000
synopsis: utility functions missing from the matrix library
authors: Andreas Bogk, Jeff Dubrule, Bruce Hoult
copyright: this program may be freely used by anyone, for any purpose

//
// Matrices are zero-based and addressed as matrix[row, column]
//
// We assumes matrices to be 4x4, and vectors to have size 4

define constant make-matrix   = curry(make, <matrix>, dimensions: #[4,4]);
define constant make-identity = curry(identity-matrix, dimensions: #[4,4]);

define constant $origin = #[0.0, 0.0, 0.0, 1.0];

define method \* (mat :: <matrix>, vector :: <vector>)
 => mult-vector :: <vector>;
  as(<simple-object-vector>, transpose(mat * transpose(matrix(vector))));
end method;

define method \* (vector :: <vector>, mat :: <matrix>)
 => mult-vector :: <vector>;
  as(<simple-object-vector>, matrix(vector) * mat);
end method;

define method \* (v1 :: <vector>, v2 :: <vector>)
 => (dot-product :: <number>);
  reduce1(\+, map(\*, v1, v2));
end method;

define method \* (v :: <vector>, n :: <number>)
 => (scalar-product :: <vector>);
  map(rcurry(\*, n), v);
end method;

define method \* (n :: <number>, v :: <vector>)
 => (scalar-product :: <vector>);
  map(curry(\*, n), v);
end method;

define method negative(v :: <vector>)
 => (negation :: <vector>);
  map(curry(\*, -1), v);
end method;

define method \+ (v1 :: <vector>, v2 :: <vector>)
 => (sum :: <vector>);
  map(\+, v1, v2);
end method;

define method \- (v1 :: <vector>, v2 :: <vector>)
 => (difference :: <vector>);
  map(\-, homogenize(v1), homogenize(v2));
end method;

define method print-object(mat :: <matrix>, stream :: <stream>)
 => ();
  for(i from 0 below mat.dimensions[0])
    format(stream, "[");
    for(j from 0 below mat.dimensions[1])
      format(stream, "[%=]", mat[i, j]);
    end for;
    format(stream, "]\n");
  end for;
end method print-object;

define method homogenize(v :: <vector>)
 => (result :: <vector>);
  map(rcurry(\/, v[v.size - 1]), v);
end method homogenize;

define method magnitude(v :: <vector>)
 => (length :: <number>);
  sqrt(reduce(\+, 0.0, map(\*, v, v)));
end method magnitude;

define method normalize(v :: <vector>)
  => (result :: <vector>);
  map(rcurry(\/, magnitude(v)), v);
end method normalize;
  