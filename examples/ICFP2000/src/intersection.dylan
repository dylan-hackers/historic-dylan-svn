module: ray-tracer
synopsis: Ray-intersection code
authors: Andreas Bogk, Jeff Dubrule, Bruce Hoult
copyright: this program may be freely used by anyone, for any purpose

define sealed class <ray> (<object>)
  slot ray-position :: <3D-point>;
  slot ray-direction :: <3D-vector>;
end class <ray>;

define method initialize(ray :: <ray>, #next next-method, #key
			   position: pos :: <3D-point>, 
			   direction: dir :: <3D-vector>, #all-keys)
 => ()
  next-method();
  ray.ray-position := homogenize(pos);
  ray.ray-direction := normalize(dir);
end method initialize;

define method transform-with-matrix(ray :: <ray>, matrix :: <transform>)
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
    // XXX: can we transform new-distance?  We probably already
    // computed it...
    new-distance := magnitude(ray.ray-position - point);

    values(point, normal, surface-method, new-distance);
  else
    values(#f, #f, #f, #f);
  end if;
end method intersection-before;

define method real-intersection-before(m :: <sphere>, ray, distance, #key
				    shadow-test: shadow-test?)
 => (point, normal, surface-method, new-distance)

  block (easy-out)
    let ray-to-center = ray.ray-position - $origin;

    if (magnitude(ray-to-center) < 1.0)
      // We started inside the sphere
      if (shadow-test?)
	easy-out(#t);
      else
	// XXX Intersection between ray and inside of sphere
      end if;
    elseif (magnitude(ray-to-center) - 1.0 > distance)
      easy-out(#f);  // We're out of range.
    else
      let t_ca = -ray-to-center * ray.ray-direction;
      if (t_ca < 0.0)
	// Pointing away from sphere, no intersection
	easy-out(#f);
      end if;
    
      let l_oc_2 = ray-to-center * ray-to-center;
      let d_2 = l_oc_2 - (t_ca * t_ca);
      
      if (abs(d_2) > 1.0)
	easy-out(#f);
      elseif (shadow-test?)
	easy-out(#t);
      else
	let point = ray.ray-position + ray.ray-direction * (t_ca - sqrt(1.0 - d_2));
/*	format-out("t_ca = %=, d_2 = %=\n", t_ca, d_2);
	format-out("  ray.ray-position = %=, ray-ray-direction = %=\n", ray.ray-position, ray.ray-direction);
	format-out("  point = %=\n", point);
	force-output(*standard-output*); */
	let u = clamp(atan2(point.z, point.x));
	let v = clamp((point.y + 1.0) / 2.0);
	values(point, 
	       point - $origin, 
	       make-surface-closure(0, u, v, m.surface-interpreter-entry), 
	       t_ca - sqrt(1.0 - d_2));
      end if;
    end if;
  end block;
end method real-intersection-before;
    
define method real-intersection-before(m :: <plane>, ray, distance, #key shadow-test: shadow-test?)
 => (point, normal, surface-method, new-distance)

  if (ray.ray-position.y < 0.0 & ray.ray-direction.y < 0.0)
    #f;
  elseif (ray.ray-position.y > 0.0 & ray.ray-direction.y > 0.0)
    #f;
  elseif (shadow-test?)
    #t;
  else
    let t = -ray.ray-position.y / ray.ray-direction.y;
    if (abs(t) > distance)
      #f;
    else
      let point = ray.ray-position + ray.ray-direction * t;
      let u = clamp(point.x);
      let v = clamp(point.z);
      values(point, 
	     vector3D(0.0, if (ray.ray-position.y < 0.0) 
			     -1.0 
			   else
			     1.0 
			   end if, 0.0),
	     make-surface-closure(0, u, v, m.surface-interpreter-entry), t);
    end if;
  end if;
end method real-intersection-before;

define method plane-intersection(norm :: <3D-vector>, d :: <fp>, ray :: <ray>)
 => hit :: false-or(<3D-point>);
  let t = -(norm * (ray.ray-position - $origin) + d) / (norm * ray.ray-direction);
  
  if (t <= 0.0)
    #f;
  else
    homogenize(ray.ray-position + ray.ray-direction * t);
  end if;
end method plane-intersection;

define constant $cube-planes =  vector(vector(vector3D( 0.0,  0.0, -1.0), 0.0), // Front
				       vector(vector3D( 0.0,  0.0,  1.0), -1.0), // Back
				       vector(vector3D(-1.0,  0.0,  0.0), 0.0), // Left
				       vector(vector3D( 1.0,  0.0,  0.0), -1.0), // Right
				       vector(vector3D( 0.0,  1.0,  0.0), -1.0), // Top
				       vector(vector3D( 0.0, -1.0,  0.0), 0.0)); // Bottom

define constant $u-methods = vector(x, x, z, z, y, y);
define constant $v-methods = vector(y, y, y, y, z, z);

define method real-intersection-before(m :: <cube>, ray, distance, #key shadow-test: shadow-test?)
 => (point, normal, surface-method, new-distance)
  block (outta-here)
    for (p in $cube-planes, which-one from 0)
      let hit = plane-intersection(p[0], p[1], ray);
      if (hit & hit.x <= 1.0 & hit.y <= 1.0 & hit.z <= 1.0
	    & hit.x >= 0.0 & hit.y >= 0.0 & hit.z >= 0.0)
	outta-here(hit, p[0], make-surface-closure(which-one, 
						   $u-methods[which-one](hit), 
						   $v-methods[which-one](hit), 
						   m.surface-interpreter-entry));
      end if;
    end for;
  end block;
end method real-intersection-before;


define method real-intersection-before(m :: <cone>, ray, distance, #key shadow-test: shadow-test?)
 => (point, normal, surface-method, new-distance)

  
end method real-intersection-before;

define method real-intersection-before(m :: <cylinder>, ray, distance, #key shadow-test: shadow-test?)
 => (point, normal, surface-method, new-distance)

  
  
end method real-intersection-before;

// CSG:
define method real-intersection-before(m :: <csg-union>, ray, distance, #key shadow-test: shadow-test?)
 => (point, normal, surface-method, new-distance)

  block(easy-out)
    let closest = distance;
    let (best-point, best-normal, best-surface) = #f;
    for (o in m.objects)
      let (p, n, surf, dist) = intersection-before(o, ray, closest, 
						   shadow-test:
						     shadow-test?);
      if (p)
	if (shadow-test?)
	  easy-out(#t, #f, #f, #f);
	else
	  best-point := p;
	  best-normal := n;
	  best-surface := surf;
	  closest := dist;
	end if;
      end if;
    end for;
    values(best-point, best-normal, best-surface, closest);
  end block;
end method real-intersection-before;
