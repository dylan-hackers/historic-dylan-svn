module: our-matrix
synopsis: utility functions missing from the matrix library
authors: Andreas Bogk, Jeff Dubrule, Bruce Hoult
copyright: this program may be freely used by anyone, for any purpose

//
// Matrices are zero-based and addressed as matrix[row, column]
//
// We assumes matrices to be 4x4, and vectors to have size 4

define class <3D-vector> (<object>)
  slot x :: <fp>, required-init-keyword: x:;
  slot y :: <fp>, required-init-keyword: y:;
  slot z :: <fp>, required-init-keyword: z:;
end;

define sealed domain make(singleton(<3D-vector>));
define sealed domain initialize(<3D-vector>);


define inline method vector3D(x :: <fp>, y :: <fp>, z :: <fp>) => (vec :: <3D-vector>);
  make(<3D-vector>, x: x, y: y, z: z);
end;

define inline sealed method \* (v1 :: <3D-vector>, v2 :: <3D-vector>)
 => (dot-product :: <fp>);
  v1.x * v2.x + v1.y * v2.y + v1.z * v2.z;
end method;

define inline sealed method \* (v :: <3D-vector>, n :: <fp>)
 => (scalar-product :: <3D-vector>);
  vector3D(v.x * n, v.y * n, v.z * n);
end method;

define inline sealed method \* (n :: <fp>, v :: <3D-vector>)
 => (scalar-product :: <3D-vector>);
  vector3D(v.x * n, v.y * n, v.z * n);
end method;

define sealed method \* (a :: <transform>, b :: <transform>)
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


define inline sealed method negative(v :: <3D-vector>)
 => (negation :: <3D-vector>);
  vector3D(-v.x, -v.y, -v.z);
end method;

define inline sealed method \+ (v1 :: <3D-vector>, v2 :: <3D-vector>)
 => (sum :: <3D-vector>);
  vector3D(v1.x + v2.x, v1.y + v2.y, v1.z + v2.z);
end method;

define inline sealed method \- (v1 :: <3D-vector>, v2 :: <3D-vector>)
 => (difference :: <3D-vector>);
  vector3D(v1.x - v2.x, v1.y - v2.y, v1.z - v2.z);
end method;

define inline sealed method magnitude(v :: <3D-vector>)
 => (length :: <fp>);
  let (a, b, c) = values(v.x, v.y, v.z);
  sqrt(a * a + b * b + c * c);
end method magnitude;

define inline sealed method normalize(v :: <3D-vector>)
  => (result :: <3D-vector>);
  let mag = magnitude(v);
  vector3D(v.x / mag, v.y / mag, v.z / mag);
end method normalize;

/* Point stuff */

define class <3D-point> (<object>)
  slot x :: <fp>, required-init-keyword: x:;
  slot y :: <fp>, required-init-keyword: y:;
  slot z :: <fp>, required-init-keyword: z:;
  slot w :: <fp>, required-init-keyword: w:;
end;


define sealed domain make(singleton(<3D-point>));
define sealed domain initialize(<3D-point>);

define inline method point3D(x :: <fp>, y :: <fp>, z :: <fp>, w :: <fp>) => (vec :: <3D-point>);
  make(<3D-point>, x: x, y: y, z: z, w: w);
end;


define constant $origin :: <3D-point> = point3D(0.0, 0.0, 0.0, 1.0);

define inline sealed method homogenize(p :: <3D-point>)
 => (result :: <3D-point>);
  let recip-w = 1.0 / p.w;
  point3D(p.x * recip-w, p.y * recip-w, p.z * recip-w, 1.0);
end method homogenize;

define inline sealed method \- (p1 :: <3D-point>, p2 :: <3D-point>)
 => (difference :: <3D-vector>);
  let w1 = p1.w;
  let w2 = p2.w;
  if (w1 = 1.0 & w2 = 1.0)
    vector3D(p1.x - p2.x, 
	     p1.y - p2.y, 
	     p1.z - p2.z);
  else
    vector3D(p1.x / w1 - p2.x / w2, 
	     p1.y / w1 - p2.y / w2, 
	     p1.z / w1 - p2.z / w2);
  end if;
end method;

define inline sealed method \+ (p :: <3D-point>, v :: <3D-vector>)
 => (sum :: <3D-point>);
  let w1 = p.w;
  if (w1 = 1.0)
    point3D(p.x + v.x, p.y + v.y, p.z + v.z, 1.0);
  else
    point3D(p.x / w1 + v.x, p.y / w1 + v.y, p.z / w1 + v.z, 1.0);
  end if;
end method;

define inline sealed method \- (p :: <3D-point>, v :: <3D-vector>)
 => (difference :: <3D-point>);
  let w1 = p.w;
  if (w1 = 1.0)
    point3D(p.x - v.x, p.y - v.y, p.z - v.z, 1.0);
  else
    point3D(p.x / w1 - v.x, p.y / w1 - v.y, p.z / w1 - v.z, 1.0);
  end if;
end method;

/* Transformation matrix stuff */

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


define inline method make-matrix() => (mat :: <transform>);
  make(<transform>);
end;


define inline method make-identity() => (mat :: <transform>);
  let m = make(<transform>);
  m.v00 := 1.0;
  m.v11 := 1.0;
  m.v22 := 1.0;
  m.v33 := 1.0;
  m;
end;

define inline sealed method \* (m :: <transform>, v :: <3D-point>)
 => mult-point :: <3D-point>;
  point3D(m.v00 * v.x + m.v01 * v.y + m.v02 * v.z + m.v03 * v.w,
	   m.v10 * v.x + m.v11 * v.y + m.v12 * v.z + m.v13 * v.w,
	   m.v20 * v.x + m.v21 * v.y + m.v22 * v.z + m.v23 * v.w,
	   m.v30 * v.x + m.v31 * v.y + m.v32 * v.z + m.v33 * v.w);
end method;

define inline sealed method \* (m :: <transform>, v :: <3D-vector>)
 => mult-vector :: <3D-vector>;
  vector3D(m.v00 * v.x + m.v01 * v.y + m.v02 * v.z,
	 m.v10 * v.x + m.v11 * v.y + m.v12 * v.z,
	 m.v20 * v.x + m.v21 * v.y + m.v22 * v.z);
end method;
