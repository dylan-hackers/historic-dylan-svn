module: icfp2000
synopsis: utility functions missing from the matrix library
authors: Andreas Bogk, Jeff Dubrule, Bruce Hoult
copyright: this program may be freely used by anyone, for any purpose


define constant make-matrix   = curry(make, <matrix>, dimensions: #[4,4]);
define constant make-identity = curry(identity-matrix, dimensions: #[4,4]);


define method \* (mat :: <matrix>, vector :: <vector>)
// => mult-vector :: <vector>;
  as(<simple-object-vector>, transpose(mat * transpose(matrix(vector))));
end method;

define method \* (vector :: <vector>, mat :: <matrix>)
// => mult-vector :: <vector>;
  (matrix(vector) * mat).components;
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
