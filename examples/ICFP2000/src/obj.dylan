module: ray-tracer
synopsis: Object model
authors: Andreas Bogk, Jeff Dubrule, Bruce Hoult
copyright: this program may be freely used by anyone, for any purpose

define abstract class <obj> (<object>)
  slot transform :: <transform> = make-identity();
  slot inverse-transform :: <transform> = make-identity();
end class <obj>;


define abstract class <primitive> (<obj>)
  slot surface-interpreter-entry, init-keyword: #"surface-function";
end class <primitive>;

define class <sphere> (<primitive>)
end class <sphere>;

define class <plane> (<primitive>)
end class <plane>;

define class <cube> (<primitive>)
end class <cube>;

define class <cone> (<primitive>)
end class <cone>;

define class <cylinder> (<primitive>)
end class <cylinder>;

define abstract class <csg-object> (<obj>)
  slot objects :: <collection>, init-keyword: of:;
end class <csg-object>;

define class <csg-union> (<csg-object>)
end class <csg-union>;

define sealed domain initialize(<obj>);
define sealed domain make(singleton(<sphere>));
define sealed domain make(singleton(<plane>));
define sealed domain make(singleton(<cube>));
define sealed domain make(singleton(<cone>));
define sealed domain make(singleton(<cylinder>));

/* --------------------- Methods ---------------------------- */

// Copying:
define method copy(o :: <obj>)
 => (new-o :: <obj>)
  let new-o = make(object-class(o));
  new-o.transform := o.transform;
  new-o.transform := o.inverse-transform;

  new-o;
end method copy;

define method copy(o :: <primitive>)
 => (new-o :: <primitive>)
  let new-o = next-method();
  new-o.surface-interpreter-entry := o.surface-interpreter-entry;

  new-o;
end method copy;

define method copy(o :: <csg-object>)
 => (new-o :: <csg-object>)
  let new-o = next-method();
  new-o.objects := o.objects;

  new-o;
end method copy;

// Translation:
define variable *translation-matrix* = make-identity();

define method translate(o :: <obj>, x, y, z) => (same-obj :: <obj>)
  *translation-matrix*.v03 := x;
  *translation-matrix*.v13 := y;
  *translation-matrix*.v23 := z;

  let new-object = copy(o);

  new-object.transform := *translation-matrix* * o.transform;

  *translation-matrix*.v03 := -x;
  *translation-matrix*.v13 := -y;
  *translation-matrix*.v23 := -z;
  new-object.inverse-transform :=  o.inverse-transform * *translation-matrix*;
  new-object;
end method translate;

// Rotation:
define variable *x-rotation-matrix* = make-identity();
define variable *last-x-rotation-theta* = 0.0;

define method x-rotate(o :: <obj>, theta) => (same-obj :: <obj>)
  if (theta ~= *last-x-rotation-theta*)
    let (s, c) = values(sin(theta), cos(theta));
    *x-rotation-matrix*.v11 := c;
    *x-rotation-matrix*.v12 := -s;
    *x-rotation-matrix*.v21 := s;
    *x-rotation-matrix*.v22 := c;
  end if;
  let new-object = copy(o);

  new-object.transform := *x-rotation-matrix* * o.transform;

  // cos(-theta) = cos(theta), sin(-theta) = -sin(theta)
  *x-rotation-matrix*.v12 := -*x-rotation-matrix*.v12;
  *x-rotation-matrix*.v21 := -*x-rotation-matrix*.v21;
  new-object.inverse-transform := *x-rotation-matrix* * o.inverse-transform;

  *last-x-rotation-theta* := -theta;
  new-object;  
end method x-rotate;

define variable *y-rotation-matrix* = make-identity();
define variable *last-y-rotation-theta* = 0.0;

define method y-rotate(o :: <obj>, theta) => (same-obj :: <obj>)
  if (theta ~= *last-y-rotation-theta*)
    let (s, c) = values(sin(theta), cos(theta));
    *y-rotation-matrix*.v00 := c;
    *y-rotation-matrix*.v02 := s;
    *y-rotation-matrix*.v20 := -s;
    *y-rotation-matrix*.v22 := c;
  end if;
  let new-object = copy(o);
  new-object.transform := *y-rotation-matrix* * o.transform;

  *y-rotation-matrix*.v02 := -*y-rotation-matrix*.v02;
  *y-rotation-matrix*.v20 := -*y-rotation-matrix*.v20;
  new-object.inverse-transform := *y-rotation-matrix* * o.inverse-transform;

  *last-y-rotation-theta* := -theta;
  new-object;
end method y-rotate;

define variable *z-rotation-matrix* = make-identity();
define variable *last-z-rotation-theta* = 0.0;

define method z-rotate(o :: <obj>, theta) => (same-obj :: <obj>)
  if (theta ~= *last-z-rotation-theta*)
    let (s, c) = values(sin(theta), cos(theta));
    *z-rotation-matrix*.v00 := c;
    *z-rotation-matrix*.v01 := -s;
    *z-rotation-matrix*.v10 := s;
    *z-rotation-matrix*.v11 := c;
  end if;
  let new-object = copy(o);

  new-object.transform :=  *z-rotation-matrix* * o.transform;

  *z-rotation-matrix*.v01 := -*z-rotation-matrix*.v01;
  *z-rotation-matrix*.v10 := -*z-rotation-matrix*.v10;
  new-object.inverse-transform := *z-rotation-matrix* * o.inverse-transform;

  *last-z-rotation-theta* := -theta;
  new-object;  
end method z-rotate;

// Scaling: 

define variable *scaling-matrix* = make-identity();

define method scale(o :: <obj>, x, y, z) => (same-obj :: <obj>)
  *scaling-matrix*.v00 := x;
  *scaling-matrix*.v11 := y;
  *scaling-matrix*.v22 := z;

  let new-object = copy(o);

  new-object.transform := o.transform * *scaling-matrix*;

  *scaling-matrix*.v00 := 1.0/x;
  *scaling-matrix*.v11 := 1.0/y;
  *scaling-matrix*.v22 := 1.0/z;

  new-object.inverse-transform := *scaling-matrix* * o.inverse-transform;
  new-object;  
end method scale;

define variable *uniform-scaling-matrix* = make-identity();

define method uniform-scale(o :: <obj>, factor) => (same-obj :: <obj>)
  *uniform-scaling-matrix*.v00 := factor;
  *uniform-scaling-matrix*.v11 := factor;
  *uniform-scaling-matrix*.v22 := factor;

  let new-object = copy(o);

  new-object.transform := *uniform-scaling-matrix* * o.transform;

  *uniform-scaling-matrix*.v00 := 1.0/factor;
  *uniform-scaling-matrix*.v11 := 1.0/factor;
  *uniform-scaling-matrix*.v22 := 1.0/factor;
  new-object.inverse-transform := o.inverse-transform * *uniform-scaling-matrix*;

  new-object;  
end method uniform-scale;
