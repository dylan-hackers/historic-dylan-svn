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
  x-rotate(o, theta * $pi / 180.0);
end graphics-primitive rotatex;

define graphics-primitive rotatey(o :: <obj>  => theta :: <fp>)
  y-rotate(o, theta * $pi / 180.0);
end graphics-primitive rotatey;

define graphics-primitive rotatez(o :: <obj> => theta :: <fp>)
  z-rotate(o, theta * $pi / 180.0);
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


/*
define method optimizable-two(token1 :: <fp>, token2 == #"uscale", more :: <list>, suppress-closure == #f, #key orig :: <pair>)
                                => (tokens :: <list>, closure);

  debug-print("optimizing uscale");
  
end;

*/

define macro unary-optimization-definer
  { define unary-optimization ?:name(?type:expression, ?operator:expression) end }
  =>
  { define unary-optimization ?name(?type, ?type, ?operator) end; }

  { define unary-optimization ?:name(?front:expression, ?back:expression, ?operator:expression) end }
  =>
  { define method optimizable-two(right :: ?back, token2 == ?#"name", more-tokens :: <pair>, suppress-closure == #f, #key orig :: <pair>) => (remaining :: <list>, closure);
      let (cont, remaining) = more-tokens.optimize-compile-GML;
      values( remaining,
              method(stack :: <pair>, env :: <function>) => new-stack :: <list>;
              let (left :: ?front, rest :: <list>) = values(stack.head, stack.tail);
              cont(pair(?operator(left, right), rest), env)
              end method)
    end method;
    
    define method optimizable-two(right :: ?back, token2 == ?#"name", more-tokens :: <list>, suppress-closure == #f, #key orig :: <pair>) => (remaining :: <list>, closure);
      let (cont, remaining) = more-tokens.optimize-compile-GML;
      values( remaining,
              method(stack :: <pair>, env :: <function>) => new-stack :: <list>;
              let (left :: ?front, rest :: <list>) = values(stack.head, stack.tail);
              cont(pair(?operator(left, right), rest), env)
              end method)
    end method; }
end macro unary-optimization-definer;

define unary-optimization uscale(<obj>, <fp>, uniform-scale) end;


// Lighting:
define graphics-primitive light(dir :: <point> => color :: <point>) 
  make(<star>, direction: vector3D(dir.point-x, dir.point-y, dir.point-z), 
       color: make(<color>, red: color.point-x, green: color.point-y, blue: color.point-z));
end;

define graphics-primitive pointlight(loc :: <point> => color ::
				       <point>)
  make(<firefly>, location: point3D(loc.point-x, loc.point-y, loc.point-z, 1.0),
       color: make(<color>, red: color.point-x, green: color.point-y, blue: color.point-z));
       
end;

define graphics-primitive spotlight(loc :: <point> => at :: <point> =>
				      color :: <point> => cutoff ::
				      <fp> => exp :: <fp>)

  let pos :: <3D-point> = point3D(loc.point-x, loc.point-y,
				  loc.point-z, 1.0);
  let dir :: <3D-vector> = pos - point3D(at.point-x, at.point-y,
				  at.point-z, 1.0);

  make(<flashlight>, location: pos, direction: dir, cutoff: cutoff *
	 $pi / 180.0, exponent: exp,
       color: make(<color>, red: color.point-x, green: color.point-y, blue: color.point-z));
end;

// CSG:
define graphics-primitive union(o1 :: <obj> => o2 :: <obj>) 
  make(<csg-union>, of: vector(o1, o2));
end;

define graphics-primitive difference() error("You write it!") end;
define graphics-primitive intersect() error("You write it!")  end;

