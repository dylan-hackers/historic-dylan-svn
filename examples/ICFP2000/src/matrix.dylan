module: icfp2000
synopsis: utility functions missing from the matrix library
authors: Andreas Bogk, Jeff Dubrule, Bruce Hoult
copyright: this program may be freely used by anyone, for any purpose

//
// Matrices are zero-based and addressed as matrix[row, column]
//
// We assumes matrices to be 4x4, and vectors to have size 4

define class <vector3D> (<object>)
  slot x :: <fp>, required-init-keyword: x:;
  slot y :: <fp>, required-init-keyword: y:;
  slot z :: <fp>, required-init-keyword: z:;
  slot w :: <fp>, required-init-keyword: w:;
end;

define sealed domain make(singleton(<vector3D>));
define sealed domain initialize(<vector3D>);

define method vector3D(x :: <fp>, y :: <fp>, z :: <fp>, w :: <fp>) => (vec :: <vector3D>);
  make(<vector3D>, x: x, y: y, z: z, w: w);
end;


define class <transform> (<object>)
  slot v00 :: <fp>, init-value: 0.0;
  slot v01 :: <fp>, init-value: 0.0;
  slot v02 :: <fp>, init-value: 0.0;
  slot v03 :: <fp>, init-value: 0.0;
  slot v10 :: <fp>, init-value: 0.0;
  slot v11 :: <fp>, init-value: 0.0;
  slot v12 :: <fp>, init-value: 0.0;
  slot v13 :: <fp>, init-value: 0.0;
  slot v20 :: <fp>, init-value: 0.0;
  slot v21 :: <fp>, init-value: 0.0;
  slot v22 :: <fp>, init-value: 0.0;
  slot v23 :: <fp>, init-value: 0.0;
  slot v30 :: <fp>, init-value: 0.0;
  slot v31 :: <fp>, init-value: 0.0;
  slot v32 :: <fp>, init-value: 0.0;
  slot v33 :: <fp>, init-value: 0.0;
end;

define sealed domain make(singleton(<transform>));
define sealed domain initialize(<transform>);


define method make-matrix() => (mat :: <transform>);
  make(<transform>);
end;


define method make-identity() => (mat :: <transform>);
  let m = make(<transform>);
  m.v00 := 1.0;
  m.v11 := 1.0;
  m.v22 := 1.0;
  m.v33 := 1.0;
  m;
end;

define constant $origin :: <vector3D> = vector3D(0.0, 0.0, 0.0, 1.0);


define method as(class == <vector3D>, v :: <vector>) => res :: <vector3D>;
  vector3D(as(<fp>, v[0]), as(<fp>, v[1]), as(<fp>, v[2]), as(<fp>, v[3]));
end;

define method as(class == <vector>, v :: <vector3D>) => res :: <vector>;
  vector(v.x, v.y, v.z, v.w);
end;


define method as(class == <transform>, m :: <matrix>) => res :: <transform>;
  let t = make(<transform>);
  t.v00 := as(<fp>, m[0,0]);
  t.v01 := as(<fp>, m[0,1]);
  t.v02 := as(<fp>, m[0,2]);
  t.v03 := as(<fp>, m[0,3]);
  t.v10 := as(<fp>, m[1,0]);
  t.v11 := as(<fp>, m[1,1]);
  t.v12 := as(<fp>, m[1,2]);
  t.v13 := as(<fp>, m[1,3]);
  t.v20 := as(<fp>, m[2,0]);
  t.v21 := as(<fp>, m[2,1]);
  t.v22 := as(<fp>, m[2,2]);
  t.v23 := as(<fp>, m[2,3]);
  t.v30 := as(<fp>, m[3,0]);
  t.v31 := as(<fp>, m[3,1]);
  t.v32 := as(<fp>, m[3,2]);
  t.v33 := as(<fp>, m[3,3]);
  t;
end;


define method as(class == <matrix>, t :: <transform>) => res :: <matrix>;
  let m = make(<matrix>, dimensions: #[4,4]);
  m[0,0] := t.v00;  m[0,1] := t.v01;  m[0,2] := t.v02;  m[0,3] := t.v03;
  m[1,0] := t.v10;  m[1,1] := t.v11;  m[1,2] := t.v12;  m[1,3] := t.v13;
  m[2,0] := t.v20;  m[2,1] := t.v21;  m[2,2] := t.v22;  m[2,3] := t.v23;
  m[3,0] := t.v30;  m[3,1] := t.v31;  m[3,2] := t.v32;  m[3,3] := t.v33;
  m;
end;

define method test-matrix-vs-transform()
  let m = make(<matrix>, dimensions: #[4,4]);
  for (i from 0 below 4)
    for (j from 0 below 4)
      m[i,j] := 10 * i + j * j + 1.0;
    end;
  end;

  let t = as(<transform>, m);

  let v = #[3.0, 2.0, 0.0, 10.0];
  let v3d = vector3D(3.0, 2.0, 0.0, 10.0);

  format-out("m = %=\n", m);
  format-out("t = %=\n", t);
  format-out("v = %=\n", v);
  format-out("v3d = %=\n\n", v3d);


  let a = v.homogenize;
  let b = v.homogenize;

  format-out("a = %=\n", a);
  format-out("b = %=\n", b);


  let a = v.magnitude;
  let b = v.magnitude;

  format-out("a = %=\n", a);
  format-out("b = %=\n", b);


  let a = v.normalize;
  let b = v.normalize;

  format-out("a = %=\n", a);
  format-out("b = %=\n", b);

end;


define method \* (m :: <transform>, v :: <vector3D>)
 => mult-vector :: <vector3D>;
  vector3D(m.v00 * v.x + m.v01 * v.y + m.v02 * v.z + m.v03 * v.w,
	   m.v10 * v.x + m.v11 * v.y + m.v12 * v.z + m.v13 * v.w,
	   m.v20 * v.x + m.v21 * v.y + m.v22 * v.z + m.v23 * v.w,
	   m.v30 * v.x + m.v31 * v.y + m.v32 * v.z + m.v33 * v.w);
end method;

define method \* (mat :: <matrix>, vector :: <vector>)
 => mult-vector :: <vector>;
  as(<simple-object-vector>, transpose(mat * transpose(matrix(vector))));
end method;


define method \* (v :: <vector3D>, m :: <transform>)
 => mult-vector :: <vector3D>;
  vector3D(m.v00 * v.x + m.v10 * v.y + m.v20 * v.z + m.v30 * v.w,
	   m.v01 * v.x + m.v11 * v.y + m.v21 * v.z + m.v31 * v.w, 
	   m.v02 * v.x + m.v12 * v.y + m.v22 * v.z + m.v32 * v.w, 
	   m.v03 * v.x + m.v13 * v.y + m.v23 * v.z + m.v33 * v.w);
end method;

define method \* (vector :: <vector>, mat :: <matrix>)
 => mult-vector :: <vector>;
  as(<simple-object-vector>, matrix(vector) * mat);
end method;


define method \* (v1 :: <vector3D>, v2 :: <vector3D>)
 => (dot-product :: <fp>);
  v1.x * v2.x + v1.y * v2.y + v1.z * v2.z + v1.w * v2.w;
end method;

define method \* (v1 :: <vector>, v2 :: <vector>)
 => (dot-product :: <number>);
  reduce1(\+, map(\*, v1, v2));
end method;


define method \* (v :: <vector3D>, n :: <fp>)
 => (scalar-product :: <vector3D>);
  vector3D(v.x * n, v.y * n, v.z * n, v.w * n);
end method;

define method \* (v :: <vector>, n :: <number>)
 => (scalar-product :: <vector>);
  map(rcurry(\*, n), v);
end method;


define method \* (n :: <fp>, v :: <vector3D>)
 => (scalar-product :: <vector3D>);
  vector3D(v.x * n, v.y * n, v.z * n, v.w * n);
end method;

define method \* (n :: <number>, v :: <vector>)
 => (scalar-product :: <vector>);
  map(curry(\*, n), v);
end method;


define method negative(v :: <vector3D>)
 => (negation :: <vector3D>);
  vector(-v.x, -v.y, -v.z, -v.w);
end method;

define method negative(v :: <vector>)
 => (negation :: <vector>);
  map(curry(\*, -1), v);
end method;


define method \+ (v1 :: <vector3D>, v2 :: <vector3D>)
 => (sum :: <vector3D>);
  vector(v1.x + v2.x, v1.y + v2.y, v1.z + v2.z, v1.w + v2.w);
end method;

define method \+ (v1 :: <vector>, v2 :: <vector>)
 => (sum :: <vector>);
  map(\+, v1, v2);
end method;


define method \- (v1 :: <vector3D>, v2 :: <vector3D>)
 => (difference :: <vector3D>);
  let w1 = 1.0 / v1.w;
  let w2 = 1.0 / v2.w;
  vector(v1.x * w1 - v2.x * w2,
	 v1.y * w1 - v2.y * w2,
	 v1.z * w1 - v2.z * w2,
	 0.0);
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
      format(stream, "%=, ", mat[i, j]);
    end for;
    format(stream, "]\n");
  end for;
end method print-object;

define method print-object(mat :: <transform>, stream :: <stream>)
 => ();
  print-object(as(<matrix>, mat), stream);
end method print-object;

define method print-object(v :: <vector3D>, stream :: <stream>)
 => ();
  print-object(as(<vector>, v), stream);
end method print-object;


define method homogenize(v :: <vector3D>)
 => (result :: <vector3D>);
  let w = 1.0 / v.w;
  vector3D(v.x * w, v.y * w, v.z * w, 1.0);
end method homogenize;

define method homogenize(v :: <vector>)
 => (result :: <vector>);
  map(rcurry(\/, v[v.size - 1]), v);
end method homogenize;


define method magnitude(v :: <vector3D>)
 => (length :: <fp>);
  let (a, b, c, d) = values(v.x, v.y, v.z, v.w);
  sqrt(a * a + b * b + c * c + d * d);
end method magnitude;

define method magnitude(v :: <vector>)
 => (length :: <number>);
  sqrt(reduce(\+, 0.0, map(\*, v, v)));
end method magnitude;


define method normalize(v :: <vector3D>)
  => (result :: <vector3D>);
  let r = 1.0 / v.magnitude;
  vector3D(v.x * r, v.y * r, v.z * r, v.w * r);
end method normalize;
  
define method normalize(v :: <vector>)
  => (result :: <vector>);
  map(rcurry(\/, magnitude(v)), v);
end method normalize;
  
