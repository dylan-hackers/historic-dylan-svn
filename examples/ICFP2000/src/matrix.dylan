module: icfp2000

define constant make-matrix = curry(make, <matrix>, dimensions: #[4,4]);

define method \* (mat :: <matrix>, vector :: <vector>)
 => mult-vector :: <vector>;
  \*(mat, transpose(matrix(vector)));
end method;

define method \* (vector :: <vector>, mat :: <matrix>)
 => mult-vector :: <vector>;
  \*(matrix(vector), mat);
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
