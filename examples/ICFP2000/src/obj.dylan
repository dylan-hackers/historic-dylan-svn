module: icfp2000
synopsis: Object model
authors: Andreas Bogk, Jeff Dubrule, Bruce Hoult
copyright: this program may be freely used by anyone, for any purpose

define class <obj> (<object>)
  slot transform :: <matrix> = make-identity(); // this is broken
  slot model :: type-union(<primitive>, <collection>);
end class <obj>;

define constant <primitive> = type-union(<sphere>);

define class <sphere> (<object>)
  slot radius;
end class <sphere>;

/* --------------------- Methods ---------------------------- */

// Translation:
define variable *translation-matrix* = identity-matrix(dimensions: #[4,4]);

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



// Copying methods:
define method translated-copy(o :: <obj>, x, y, z) 
 => (new_obj ::  <obj>)
  
  let new_obj = shallow-copy(o);

  translate!(o, x, y, z);
end method translated-copy;

