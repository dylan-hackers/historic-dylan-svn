module: icfp2000
synopsis: Object model
authors: Andreas Bogk, Jeff Dubrule, Bruce Hoult
copyright: this program may be freely used by anyone, for any purpose

define class <obj> (<object>)
  slot transform :: <matrix> = identity-matrix(dimensions: #[4,4]);
  slot model :: type-union(<primitive>, <collection>);
end class <obj>;

define constant <primitive> = type-union(<sphere>);

define class <sphere> (<object>)
end class <sphere>;

/* --------------------- Methods ---------------------------- */

// Translation:
define variable *translation-matrix* = identity-matrix(dimensions:
							 #[4,4]);

define method translate!(o :: <obj>, x, y, z) => (same-obj :: <obj>)
  *translation-matrix*[0, 3] := x;
  *translation-matrix*[1, 3] := y;
  *translation-matrix*[2, 3] := z;

  o.transform := *translation-matrix* * o.transform;
  o;
end method translate!;

// Rotation:
define variable *x-rotation-matrix* = identity-matrix(dimensions: #[4,4]);
define variable *last-x-rotation-theta* = 0.0;

define method x-rotate!(o :: <obj>, theta) => (same-obj :: <obj>)
  if (theta ~= *last-x-rotation-theta*)
    *x-rotation-matrix*[1, 1] := cos(theta);
    *x-rotation-matrix*[1, 2] := -sin(theta);
    *x-rotation-matrix*[2, 1] := sin(theta);
    *x-rotation-matrix*[2, 2] := cos(theta);
    *last-x-rotation-theta* := theta;
  end if;
  o.transform := *x-rotation-matrix* * o.transform;
  o;  
end method x-rotate!;

define variable *y-rotation-matrix* = identity-matrix(dimensions: #[4,4]);
define variable *last-y-rotation-theta* = 0.0;

define method y-rotate!(o :: <obj>, theta) => (same-obj :: <obj>)
  if (theta ~= *last-y-rotation-theta*)
    *y-rotation-matrix*[0, 0] := cos(theta);
    *y-rotation-matrix*[0, 2] := sin(theta);
    *y-rotation-matrix*[2, 0] := -sin(theta);
    *y-rotation-matrix*[2, 2] := cos(theta);
    *last-y-rotation-theta* := theta;
  end if;
  o.transform := *y-rotation-matrix* * o.transform;
  o;  
end method y-rotate!;

define variable *z-rotation-matrix* = identity-matrix(dimensions: #[4,4]);
define variable *last-z-rotation-theta* = 0.0;

define method z-rotate!(o :: <obj>, theta) => (same-obj :: <obj>)
  if (theta ~= *last-z-rotation-theta*)
    *z-rotation-matrix*[0, 0] := cos(theta);
    *z-rotation-matrix*[0, 1] := -sin(theta);
    *z-rotation-matrix*[1, 0] := sin(theta);
    *z-rotation-matrix*[1, 1] := cos(theta);
    *last-z-rotation-theta* := theta;
  end if;
  o.transform := *z-rotation-matrix* * o.transform;
  o;  
end method z-rotate!;

// Scaling: 

define variable *scaling-matrix* = identity-matrix(dimensions: #[4,4]);

define method scale!(o :: <obj>, x, y, z) => (same-obj :: <obj>)
  *scaling-matrix*[0, 0] := x;
  *scaling-matrix*[1, 1] := y;
  *scaling-matrix*[2, 2] := z;

  o.transform := *scaling-matrix* * o.transform;
  o;  
end method scale!;


define variable *uniform-scaling-matrix* = identity-matrix(dimensions: #[4,4]);

define method uniform-scale!(o :: <obj>, factor) => (same-obj :: <obj>)
  *uniform-scaling-matrix*[0, 0] := factor;
  *uniform-scaling-matrix*[1, 1] := factor;
  *uniform-scaling-matrix*[2, 2] := factor;

  o.transform := *uniform-scaling-matrix* * o.transform;
  o;
end method uniform-scale!;

/* 
// Copying methods:
define method translated-copy(o :: <obj>, x, y, z) 
 => (new_obj ::  <obj>)
  
  let new_obj = shallow-copy(o);

  translate!(o, x, y, z);
end method translated-copy;

*/
