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

define method intersection-before(o :: <obj>, ray, distance, #key shadow-test: shadow-test?)
 => (point, normal, surface-function, new-distance)

  // First transform the pos/ray/distance into the coordinates of o,
  // using the inverse of o.transform.

  let local-ray = transform-with-matrix(ray, o.inverse-transform);

  let (our-point, our-normal, surface-method, new-distance) =
    real-intersection-before(o, local-ray, distance, shadow-test: shadow-test?);

  // Now, transform the point & normal back into the caller's coordinates
  if (shadow-test?)
    our-point;
  elseif (our-point)
    let point = o.transform * our-point;
    let normal = normalize(o.transform * our-normal);

    values(point, normal, surface-method, new-distance);
  else
    values(#f, #f, #f, #f);
  end if;
end method intersection-before;

define method real-intersection-before(m :: <sphere>, ray, distance, #key
				    shadow-test: shadow-test?)
 => (point, normal, surface-method, new-distance)


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
      let d_2 = l_oc_2 - (t_ca * t_ca);
      
      if (abs(d_2) > 1.0)
	easy-out(#f);
      elseif (shadow-test?)
	easy-out(#t);
      else
	let point = ray.ray-direction * (t_ca - sqrt(1.0 - d_2)) 
	  + ray.ray-position;
/*	format-out("t_ca = %=, d_2 = %=\n", t_ca, d_2);
	format-out("  ray.ray-position = %=, ray-ray-direction = %=\n", ray.ray-position, ray.ray-direction);
	format-out("  point = %=\n", point);
	force-output(*standard-output*); */
	let u = clamp(atan2(point[2], point[0]));
	let v = clamp((point[1] + 1.0) / 2.0);
	values(point, point - $origin, make-surface-closure(0, u, v, m.surface-interpreter-entry));
      end if;
    end if;
  end block;
end method real-intersection-before;
    
define method real-intersection-before(m :: <plane>, ray, distance, #key shadow-test: shadow-test?)
 => (point, normal, surface-method, new-distance)

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
      let point = ray.ray-direction * t + ray.ray-position;
      let u = clamp(point[0]);
      let v = clamp(point[2]);
      values(point, 
	     #[ 0.0, 1.0, 0.0, 0.0 ],
	     make-surface-closure(0, u, v, m.surface-interpreter-entry), t);
    end if;
  end if;
end method real-intersection-before;

define method real-intersection-before(m :: <csg-union>, ray, distance, #key shadow-test: shadow-test?)
 => (point, normal, surface-method, new-distance)

  block(easy-out)
    let (point1, normal1, surface1, distance1) = intersection-before(m.objects[0], ray, distance, shadow-test: shadow-test?);
    if(point1 & shadow-test?)
      easy-out(values(#t, #f, #f, #f));
    end if;
    let (point2, normal2, surface2, distance2) = intersection-before(m.objects[0], ray, distance, shadow-test: shadow-test?);
    if(point2 & shadow-test?)
      easy-out(values(#t, #f, #f, #f));
    end if;
    if(point1 & point2)
      if(distance1 < distance2)
	values(point1, normal1, surface1, distance1);
      else
	values(point2, normal2, surface2, distance2);
      end if;
    else 
      if(point1)
	values(point1, normal1, surface1, distance1);
      else
	if(point2)
	  values(point2, normal2, surface2, distance2);
	else
	  #f;
	end if;
      end if;
    end if;
  end block;
end method real-intersection-before;
