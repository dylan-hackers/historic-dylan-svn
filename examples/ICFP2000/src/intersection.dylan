module: icfp2000
synopsis: Ray-intersection code
authors: Andreas Bogk, Jeff Dubrule, Bruce Hoult
copyright: this program may be freely used by anyone, for any purpose

define class <ray> (<object>)
  slot ray-position;
  slot ray-direction;
end class <ray>;

define method initialize(ray :: <ray>, #next next-method, #key position: pos, direction: dir, #all-keys)
 => ()
  ray.ray-position := homogenize(pos);
  ray.ray-direction := normalize(dir);
end method initialize;

define method transform-with-matrix(ray :: <ray>, matrix :: <matrix>)
  make(<ray>, position: matrix * ray.ray-position, direction: matrix * ray.ray-direction);
end method transform-with-matrix;

define class <surface> (<object>)
  slot color, init-keyword: color:, init-value: make(<color>, red: 1.0, green: 1.0, blue: 1.0);
  slot diffusion-coefficient, init-keyword: diffusion:, init-value: 1.0;
  slot specular-coefficient, init-keyword: specular:, init-value: 0.0;
  slot phong-coefficient, init-keyword: phong:, init-value: 0.0;
end class <surface>;

define method make-surface-closure(surface-id, u, v, interpreter-entry)
  let surface = #f;

  local method return-color()
    if(surface)
      surface;
    else
      let (color, diffusion, specular, phong) = interpreter-entry(surface-id, u, v);
      surface := make(<surface>, color: color, diffusion: diffusion, specular: specular, phong: phong);
      surface;
    end if;
  end method return-color;
  return-color;
end method make-surface-closure;

define method clamp(x)
  if(x < 0.0)
    0.0;
  else 
    if(x > 1.0)
      1.0;
    else
      x;
    end if;
  end if;
end method clamp;

define method silly-texture(surface-id, u, v)
  values(make(<color>, red: clamp(u), green: clamp(v), blue: 0.0), 1.0, 0.2, 0.2);
end method silly-texture;

define method intersection-before(o :: <obj>, ray, distance, #key shadow-test: shadow-test?)
 => (point, normal, surface-function)

  // First transform the pos/ray/distance into the coordinates of o,
  // using the inverse of o.transform.

  let local-ray = transform-with-matrix(ray, o.inverse-transform);

  let (our-point, our-normal, surface-method) =
    intersection-before(o.model, local-ray, distance, shadow-test: shadow-test?);

  // Now, transform the point & normal back into the caller's coordinates
  if (shadow-test?)
    our-point;
  elseif (our-point)
    let point = o.transform * our-point;
    let normal = o.transform * our-normal;

    values(point, normal, surface-method);
  else
    values(#f, #f, #f);
  end if;
end method intersection-before;

define method intersection-before(m :: <sphere>, ray, distance, #key
				    shadow-test: shadow-test?)
 => (point, normal, surface-method)


  block (easy-out)
    if (magnitude(ray.ray-position - $origin) < 1.0)
      // We started inside the sphere
      if (shadow-test?)
	easy-out(#t);
      else
	// XXX Intersection between ray and inside of sphere
      end if;
    elseif (magnitude(ray.ray-position - $origin) - 1.0 > distance)
      easy-out(#f);  // We're out of range.
    else
      let t_ca = -ray.ray-position * ray.ray-direction;
      if (t_ca < 0.0)
	// Pointing away from sphere, no intersection
	easy-out(#f);
      end if;
    
      let foo = ray.ray-position - $origin;
      let l_oc_2 = foo * foo;
      
      if (abs(l_oc_2 - (t_ca * t_ca)) > 1.0)
	easy-out(#f);
      elseif (shadow-test?)
	easy-out(#t);
      else
	let u = 0.5; // XXX
	let v = 0.5;
	values(#[ 1.0, 1.0, 1.0, 1.0 ], #[1.0, 1.0, 1.0, 1.0], make-surface-closure(0, u, v, m.surface-interpreter-entry));
      end if;
    end if;
  end block;
end method intersection-before;
    
define method intersection-before(m :: <plane>, ray, distance, #key shadow-test: shadow-test?)
 => (point, normal, surface-method)

  if (ray.ray-position[1] < 0.0 & ray.ray-direction[1] < 0.0)
    #f;
  elseif (ray.ray-position[1] > 0.0 & ray.ray-direction[1] > 0.0)
    #f;
  elseif (abs(ray.ray-position[1]) > distance)
    #f; // Out of range, even going straight towards the plane...
  elseif (shadow-test?)
    #t;
  else
    let t = -ray.ray-position[1]/ray.ray-direction[1];
    if (abs(t) > distance)
      #f;
    else
      let point = ray.ray-direction * t + $origin;
      let u = clamp(point[0]);
      let v = clamp(point[2]);
      values(point, 
	     #[ 0.0, 1.0, 0.0, 0.0 ],
	     make-surface-closure(0, u, v, m.surface-interpreter-entry));
    end if;
  end if;
end method intersection-before;