module: icfp2000
synopsis: Object model
authors: Andreas Bogk, Jeff Dubrule, Bruce Hoult
copyright: this program may be freely used by anyone, for any purpose

define abstract class <obj> (<object>)
  slot transform :: <matrix> = identity-matrix(dimensions: #[4,4]);
  slot inverse-transform :: <matrix> = identity-matrix(dimensions: #[4,4]);
end class <obj>;

define abstract class <primitive> (<obj>)
  slot surface-interpreter-entry;
end class <primitive>;

define sealed class <sphere> (<primitive>)
end class <sphere>;

define sealed class <plane> (<primitive>)
end class <plane>;

define sealed class <cube> (<primitive>)
end class <cube>;

define sealed class <cone> (<primitive>)
end class <cone>;

define abstract class <csg-object> (<obj>)
  slot objects :: <collection>, init-keyword: of:;
end class <csg-object>;

define class <csg-union> (<csg-object>)
end class <csg-union>;


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
define variable *translation-matrix* = identity-matrix(dimensions:
							 #[4,4]);

define method translate!(o :: <obj>, x, y, z) => (same-obj :: <obj>)
  *translation-matrix*[0, 3] := x;
  *translation-matrix*[1, 3] := y;
  *translation-matrix*[2, 3] := z;

  o.transform := o.transform * *translation-matrix* ;

  *translation-matrix*[0, 3] := -x;
  *translation-matrix*[1, 3] := -y;
  *translation-matrix*[2, 3] := -z;
  o.inverse-transform := *translation-matrix* * o.inverse-transform;
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
  end if;
  o.transform := o.transform * *x-rotation-matrix* ;

  // cos(-theta) = cos(theta), sin(-theta) = -sin(theta)
  *x-rotation-matrix*[1, 2] := -*x-rotation-matrix*[1, 2];
  *x-rotation-matrix*[2, 1] := -*x-rotation-matrix*[2, 1];
  o.inverse-transform := *x-rotation-matrix* * o.inverse-transform;

  *last-x-rotation-theta* := -theta;
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
  end if;
  o.transform := o.transform * *y-rotation-matrix*;

  *y-rotation-matrix*[0, 2] := -*y-rotation-matrix*[0, 2];
  *y-rotation-matrix*[2, 0] := -*y-rotation-matrix*[2, 0];
  o.inverse-transform := *y-rotation-matrix* * o.inverse-transform;

  *last-y-rotation-theta* := -theta;
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
  end if;
  o.transform := o.transform * *z-rotation-matrix* ;

  *z-rotation-matrix*[0, 1] := -*z-rotation-matrix*[0, 1];
  *z-rotation-matrix*[1, 0] := -*z-rotation-matrix*[1, 0];
  o.inverse-transform := *z-rotation-matrix* * o.inverse-transform;

  *last-z-rotation-theta* := -theta;
  o;  
end method z-rotate!;

// Scaling: 

define variable *scaling-matrix* = identity-matrix(dimensions: #[4,4]);

define method scale!(o :: <obj>, x, y, z) => (same-obj :: <obj>)
  *scaling-matrix*[0, 0] := x;
  *scaling-matrix*[1, 1] := y;
  *scaling-matrix*[2, 2] := z;

  o.transform := o.transform * *scaling-matrix*;

  *scaling-matrix*[0, 0] := 1.0/x;
  *scaling-matrix*[1, 1] := 1.0/y;
  *scaling-matrix*[2, 2] := 1.0/z;

  o.inverse-transform := *scaling-matrix* * o.inverse-transform;
  o;  
end method scale!;


define variable *uniform-scaling-matrix* = identity-matrix(dimensions: #[4,4]);

define method uniform-scale!(o :: <obj>, factor) => (same-obj :: <obj>)
  *uniform-scaling-matrix*[0, 0] := factor;
  *uniform-scaling-matrix*[1, 1] := factor;
  *uniform-scaling-matrix*[2, 2] := factor;

  o.transform := o.transform * *uniform-scaling-matrix* ;

  *uniform-scaling-matrix*[0, 0] := 1.0/factor;
  *uniform-scaling-matrix*[1, 1] := 1.0/factor;
  *uniform-scaling-matrix*[2, 2] := 1.0/factor;

  o.inverse-transform := o.inverse-transform * *uniform-scaling-matrix*;
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

define method translate(o :: <obj>, x, y, z) => (same-obj :: <obj>)
  *translation-matrix*[0, 3] := x;
  *translation-matrix*[1, 3] := y;
  *translation-matrix*[2, 3] := z;

  let new-object = copy(o);

  new-object.transform := *translation-matrix* * o.transform;

  *translation-matrix*[0, 3] := -x;
  *translation-matrix*[1, 3] := -y;
  *translation-matrix*[2, 3] := -z;
  new-object.inverse-transform :=  o.inverse-transform * *translation-matrix*;
  new-object;
end method translate;

define method x-rotate(o :: <obj>, theta) => (same-obj :: <obj>)
  if (theta ~= *last-x-rotation-theta*)
    *x-rotation-matrix*[1, 1] := cos(theta);
    *x-rotation-matrix*[1, 2] := -sin(theta);
    *x-rotation-matrix*[2, 1] := sin(theta);
    *x-rotation-matrix*[2, 2] := cos(theta);
  end if;
  let new-object = copy(o);

  new-object.transform := *x-rotation-matrix* * o.transform;

  // cos(-theta) = cos(theta), sin(-theta) = -sin(theta)
  *x-rotation-matrix*[1, 2] := -*x-rotation-matrix*[1, 2];
  *x-rotation-matrix*[2, 1] := -*x-rotation-matrix*[2, 1];
  new-object.inverse-transform := o.inverse-transform * *x-rotation-matrix*;

  *last-x-rotation-theta* := -theta;
  new-object;  
end method x-rotate;

define method uniform-scale(o :: <obj>, factor) => (same-obj :: <obj>)
  *uniform-scaling-matrix*[0, 0] := factor;
  *uniform-scaling-matrix*[1, 1] := factor;
  *uniform-scaling-matrix*[2, 2] := factor;

  let new-object = copy(o);

  new-object.transform := *uniform-scaling-matrix* * o.transform;

  *uniform-scaling-matrix*[0, 0] := 1.0/factor;
  *uniform-scaling-matrix*[1, 1] := 1.0/factor;
  *uniform-scaling-matrix*[2, 2] := 1.0/factor;
  new-object.inverse-transform := o.inverse-transform * *uniform-scaling-matrix*;

  new-object;  
end method uniform-scale;

