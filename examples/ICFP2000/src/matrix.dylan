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

define method \* (m :: <transform>, v :: <vector3D>)
 => mult-vector :: <vector3D>;
  vector3D(m.v00 * v.x + m.v01 * v.y + m.v02 * v.z + m.v03 * v.w,
	   m.v10 * v.x + m.v11 * v.y + m.v12 * v.z + m.v13 * v.w,
	   m.v20 * v.x + m.v21 * v.y + m.v22 * v.z + m.v23 * v.w,
	   m.v30 * v.x + m.v31 * v.y + m.v32 * v.z + m.v33 * v.w);
end method;

define method \* (v :: <vector3D>, m :: <transform>)
 => mult-vector :: <vector3D>;
  vector3D(m.v00 * v.x + m.v10 * v.y + m.v20 * v.z + m.v30 * v.w,
	   m.v01 * v.x + m.v11 * v.y + m.v21 * v.z + m.v31 * v.w, 
	   m.v02 * v.x + m.v12 * v.y + m.v22 * v.z + m.v32 * v.w, 
	   m.v03 * v.x + m.v13 * v.y + m.v23 * v.z + m.v33 * v.w);
end method;

define method \* (v1 :: <vector3D>, v2 :: <vector3D>)
 => (dot-product :: <fp>);
  v1.x * v2.x + v1.y * v2.y + v1.z * v2.z + v1.w * v2.w;
end method;

define method \* (v :: <vector3D>, n :: <fp>)
 => (scalar-product :: <vector3D>);
  vector3D(v.x * n, v.y * n, v.z * n, v.w * n);
end method;


define method \* (n :: <fp>, v :: <vector3D>)
 => (scalar-product :: <vector3D>);
  vector3D(v.x * n, v.y * n, v.z * n, v.w * n);
end method;

define method \* (a :: <transform>, b :: <transform>)
 => (product :: <transform>);
  let r = make(<transform>);
  r.v00 := a.v00 * b.v00 + a.v01 * b.v10 + a.v02 * b.v20 + a.v03 * b.v30;
  r.v01 := a.v00 * b.v01 + a.v01 * b.v11 + a.v02 * b.v21 + a.v03 * b.v31;
  r.v02 := a.v00 * b.v02 + a.v01 * b.v12 + a.v02 * b.v22 + a.v03 * b.v32;
  r.v03 := a.v00 * b.v03 + a.v01 * b.v13 + a.v02 * b.v23 + a.v03 * b.v33;
  r.v10 := a.v10 * b.v00 + a.v11 * b.v10 + a.v12 * b.v20 + a.v13 * b.v30;
  r.v11 := a.v10 * b.v01 + a.v11 * b.v11 + a.v12 * b.v21 + a.v13 * b.v31;
  r.v12 := a.v10 * b.v02 + a.v11 * b.v12 + a.v12 * b.v22 + a.v13 * b.v32;
  r.v13 := a.v10 * b.v03 + a.v11 * b.v13 + a.v12 * b.v23 + a.v13 * b.v33;
  r.v20 := a.v20 * b.v00 + a.v21 * b.v10 + a.v22 * b.v20 + a.v23 * b.v30;
  r.v21 := a.v20 * b.v01 + a.v21 * b.v11 + a.v22 * b.v21 + a.v23 * b.v31;
  r.v22 := a.v20 * b.v02 + a.v21 * b.v12 + a.v22 * b.v22 + a.v23 * b.v32;
  r.v23 := a.v20 * b.v03 + a.v21 * b.v13 + a.v22 * b.v23 + a.v23 * b.v33;
  r.v30 := a.v30 * b.v00 + a.v31 * b.v10 + a.v32 * b.v20 + a.v33 * b.v30;
  r.v31 := a.v30 * b.v01 + a.v31 * b.v11 + a.v32 * b.v21 + a.v33 * b.v31;
  r.v32 := a.v30 * b.v02 + a.v31 * b.v12 + a.v32 * b.v22 + a.v33 * b.v32;
  r.v33 := a.v30 * b.v03 + a.v31 * b.v13 + a.v32 * b.v23 + a.v33 * b.v33;
  r;
end;


define method negative(v :: <vector3D>)
 => (negation :: <vector3D>);
  vector3D(-v.x, -v.y, -v.z, -v.w);
end method;

define method \+ (v1 :: <vector3D>, v2 :: <vector3D>)
 => (sum :: <vector3D>);
  vector3D(v1.x + v2.x, v1.y + v2.y, v1.z + v2.z, v1.w + v2.w);
end method;

define method \- (v1 :: <vector3D>, v2 :: <vector3D>)
 => (difference :: <vector3D>);
  let w1 = 1.0 / v1.w;
  let w2 = 1.0 / v2.w;
  vector3D(v1.x * w1 - v2.x * w2,
	   v1.y * w1 - v2.y * w2,
	   v1.z * w1 - v2.z * w2,
	   0.0);
end method;

define method homogenize(v :: <vector3D>)
 => (result :: <vector3D>);
  let w = 1.0 / v.w;
  vector3D(v.x * w, v.y * w, v.z * w, 1.0);
end method homogenize;

define method magnitude(v :: <vector3D>)
 => (length :: <fp>);
  let (a, b, c, d) = values(v.x, v.y, v.z, v.w);
  sqrt(a * a + b * b + c * c + d * d);
end method magnitude;

define method normalize(v :: <vector3D>)
  => (result :: <vector3D>);
  let r = 1.0 / v.magnitude;
  vector3D(v.x * r, v.y * r, v.z * r, v.w * r);
end method normalize;

define method print-object(v :: <vector3D>, stream :: <stream>)
 => ();
  print-object(vector(v.x, v.y, v.z, v.w), stream);
end method print-object;
