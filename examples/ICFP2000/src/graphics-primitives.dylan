language: infix-dylan
module: GML-compiler
file: graphics-primitives.dylan
author: Gabor Greif, mailto: gabor@mac.com


define macro graphics-primitive-definer

  { define graphics-primitive ?:name() end }
  =>
  { define method compile-one(token == ?#"name", more-tokens :: <list>) => (closure :: <function>, remaining :: <list>);
      let (cont, remaining) = more-tokens.compile-GML;
      "GML: graphics primitive '" ?"name" "' not supported".error;
    end; }

  { define graphics-primitive ?:name() ?:body end }
  =>
  { define method compile-one(token == ?#"name", more-tokens :: <list>) => (closure :: <function>, remaining :: <list>);
      let (cont, remaining) = more-tokens.compile-GML;
      values(method(stack :: <list>, env :: <function>) => new-stack :: <list>;
               cont(pair(?body, stack), env)
             end method,
             remaining)
    end method; }

  { define graphics-primitive ?:name(?vars) ?:body end }
  =>
  { define method compile-one(token == ?#"name", more-tokens :: <list>) => (closure :: <function>, remaining :: <list>);
      let (cont, remaining) = more-tokens.compile-GML;
      values(method(stack :: <pair>, env :: <function>) => new-stack :: <list>;
               ?vars;
               cont(pair(?body, stack), env)
             end method,
             remaining)
    end method; }

  { define graphics-primitive ?:name(?vars) => (); ?:body end }
  =>
  { define method compile-one(token == ?#"name", more-tokens :: <list>) => (closure :: <function>, remaining :: <list>);
      let (cont, remaining) = more-tokens.compile-GML;
      values(method(stack :: <pair>, env :: <function>) => new-stack :: <list>;
               ?vars;
               ?body;
               cont(stack, env)
             end method,
             remaining)
    end method; }

  vars:
  { ?:variable } => { let (?variable, stack :: <list>) = values(stack.head, stack.tail) }
  { ... => ?:variable } => { let (?variable, stack :: <pair>) = values(stack.head, stack.tail); ... }

end macro graphics-primitive-definer;


/*

USAGE:  the parameters should be written in the order as defined in the task.pdf

define graphics-primitive slump(s :: <integer> => f :: <float> => v :: <vector>)
  (f + s + 1) * v.first
end graphics-primitive slump;

*/

define graphics-primitive render(     amb :: <point> 
				   => lights :: <vector>
				   => obj :: <obj>
				   => depth :: <integer>
				   => fov :: <fp>
				   => wid :: <integer>
				   => ht :: <integer>
				   => file :: <string>) 
 => ();
  let amb-color = make(<color>, red: amb.point-x, green: amb.point-y, blue: amb.point-z);

  let fov-radians = fov * $pi / 180.0;

  render-image(obj, depth, file, amb-color, lights, wid,
	       ht, fov-radians);
end graphics-primitive render;

// Object creation: 
define graphics-primitive sphere(surf :: <function>)
  make(<sphere>, surface-function: surf);
end graphics-primitive sphere;

define graphics-primitive plane(surf :: <function>)
   make(<plane>, surface-function: surf);
end graphics-primitive plane;

define graphics-primitive cone() error("No cones yet!") end;
define graphics-primitive cube() error("No cubes yet!") end;
define graphics-primitive cylinder() error("No cylinders yet!") end;

// Transformations: 
define graphics-primitive rotatex(o :: <obj> => theta :: <fp>)
  x-rotate(o, theta);
end graphics-primitive rotatex;

define graphics-primitive rotatey(o :: <obj>  => theta :: <fp>)
  y-rotate(o, theta);
end graphics-primitive rotatey;

define graphics-primitive rotatez(o :: <obj> => theta :: <fp>)
  z-rotate(o, theta);
end graphics-primitive rotatez;

define graphics-primitive translate(o :: <obj> => x :: <fp> => y :: <fp> => z :: <fp>)
  translate(o, x, y, z);
end;

define graphics-primitive scale(o :: <obj> => x :: <fp> => y :: <fp> => z :: <fp>) 
  scale(o, x, y, z);
end;

define graphics-primitive uscale(o :: <obj> => factor :: <fp>)
  uniform-scale(o, factor);
end;

// Lighting:
define graphics-primitive light(dir :: <point> => color :: <point>) 
  make(<star>, direction: vector3D(dir.point-x, dir.point-y, dir.point-z), 
               color: make(<color>, red: color.point-x, green: color.point-y, blue: color.point-z));
end;

define graphics-primitive pointlight() error("No pointlights yet") end;
define graphics-primitive spotlight() error("No spotlights yet") end;

// CSG:
define graphics-primitive union(o1 :: <obj> => o2 :: <obj>) 
  make(<csg-union>, of: vector(o1, o2));
end;

define graphics-primitive difference() error("You write it!") end;
define graphics-primitive intersect() error("You write it!")  end;

