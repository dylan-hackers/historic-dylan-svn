module: ray-tracer
synopsis: Ray-intersection code
authors: Andreas Bogk, Jeff Dubrule, Bruce Hoult
copyright: this program may be freely used by anyone, for any purpose

define class <ray> (<object>)
  slot ray-position :: <3D-point>, required-init-keyword: position:;
  slot ray-direction :: <3D-vector>, required-init-keyword: direction:;
end class <ray>;

define sealed domain make(singleton(<ray>));
define sealed domain initialize(<ray>);

define method initialize(ray :: <ray>, #key
			   position: pos :: <3D-point>, 
			   direction: dir :: <3D-vector>, #all-keys)
 => ();
  ray.ray-position := homogenize(pos);
  ray.ray-direction := normalize(dir);
end method initialize;

define method transform-with-matrix(ray :: <ray>, matrix :: <transform>) => (new-ray :: <ray>);
//  let ray-pos :: <3D-point> = ray.ray-position;
//  let ray-dir :: <3D-vector> = ray.ray-direction;
  make(<ray>, position: matrix * ray.ray-position, direction: matrix * ray.ray-direction);
end method transform-with-matrix;

define generic real-intersection-before(o :: <obj>, ray :: <ray>, distance :: <fp>,
					shadow-test? :: <boolean>)
 => (point, normal, surface-function, new-distance);

define sealed domain real-intersection-before(<obj>, <ray>, <fp>, <boolean>);


define method intersection-before(o :: <obj>, ray :: <ray>, distance :: <fp>,
				  shadow-test? :: <boolean>)
 => (point, normal, surface-function, new-distance)

  // First transform the pos/ray/distance into the coordinates of o,
  // using the inverse of o.transform.

  let local-ray :: <ray> = transform-with-matrix(ray, o.inverse-transform);

  let (our-point, our-normal, surface, new-distance) =
    real-intersection-before(o, local-ray, distance, shadow-test?);

  // Now, transform the point & normal back into the caller's coordinates
  if (shadow-test?)
    our-point;
  elseif (our-point)
    let trans :: <transform> = o.transform;
    let our-point :: <3D-point> = our-point;
    let our-normal :: <3D-vector> = our-normal;

    let point :: <3D-point> = trans * our-point;
    let normal :: <3D-vector> = normalize(trans * our-normal); 
    // XXX: can we transform new-distance?  We probably already
    // computed it...
    new-distance := magnitude(ray.ray-position - point);

    values(point, normal, surface, new-distance);
  else
    values(#f, #f, #f, #f);
  end if;
end method intersection-before;


define method real-intersection-before(m :: <sphere>, ray :: <ray>, distance :: <fp>,
				       shadow-test? :: <boolean>)
 => (point, normal, surface, new-distance)

  block (easy-out)
    let ray-to-center :: <3D-vector> = ray.ray-position - $origin;
    let mag :: <fp> = magnitude(ray-to-center);

    if (mag < 1.0)
      // We started inside the sphere
      if (shadow-test?)
	easy-out(#t);
      else
	// XXX Intersection between ray and inside of sphere
      end if;
    elseif (mag - 1.0 > distance)
      easy-out(#f);  // We're out of range.
    else
      let t_ca :: <fp> = -ray-to-center * ray.ray-direction;
      if (t_ca < 0.0)
	// Pointing away from sphere, no intersection
	easy-out(#f);
      end if;
    
      let l_oc_2 :: <fp> = ray-to-center * ray-to-center;
      let d_2 :: <fp> = l_oc_2 - (t_ca * t_ca);
      
      if (abs(d_2) > 1.0)
	easy-out(#f);
      elseif (shadow-test?)
	easy-out(#t);
      else
	let point :: <3D-point> = ray.ray-position + ray.ray-direction * (t_ca - sqrt(1.0 - d_2));
/*	format-out("t_ca = %=, d_2 = %=\n", t_ca, d_2);
	format-out("  ray.ray-position = %=, ray-ray-direction = %=\n",
		   ray.ray-position, ray.ray-direction);
	format-out("  point = %=\n", point);
	force-output(*standard-output*); */
	let u :: <fp> = clamp(atan2(point.z, point.x));
	let v :: <fp> = clamp((point.y + 1.0) / 2.0);
	values(point, 
	       point - $origin, 
	       make-surface-closure(0, u, v, m), 
	       t_ca - sqrt(1.0 - d_2));
      end if;
    end if;
  end block;
end method real-intersection-before;
    
define method real-intersection-before(m :: <plane>, ray :: <ray>, distance :: <fp>,
				       shadow-test? :: <boolean>)
 => (point, normal, surface, new-distance)

  let pos-y :: <fp> = ray.ray-position.y;
  let dir-y :: <fp> = ray.ray-direction.y;

  if (pos-y < 0.0 & dir-y < 0.0)
    #f;
  elseif (pos-y > 0.0 & dir-y > 0.0)
    #f;
  elseif (shadow-test?)
    #t;
  else
    let t :: <fp> = -pos-y / dir-y;
    if (abs(t) > distance)
      #f;
    else
      let point :: <3D-point> = ray.ray-position + ray.ray-direction * t;
      let u = clamp(point.x);
      let v = clamp(point.z);
      values(point, 
	     vector3D(0.0,
		      if (ray.ray-position.y < 0.0) 
			-1.0 
		      else
			1.0 
		      end if, 0.0),
	     make-surface-closure(0, u, v, m), t);
    end if;
  end if;
end method real-intersection-before;

define method plane-intersection(norm :: <3D-vector>, displacement :: <fp>, ray :: <ray>)
 => (hit, distance);
  let t = -(norm * (ray.ray-position - $origin) + displacement) / (norm * ray.ray-direction);
  
  if (t <= 0.0)
    #f;
  else
    values(homogenize(ray.ray-position + ray.ray-direction * t), t);
  end if;
end method plane-intersection;


define class <hit-info> (<object>)
  slot dist :: <fp>, required-init-keyword: dist:;
  slot hit :: <3D-point>, required-init-keyword: hit:;
  slot norm :: <3D-vector>, required-init-keyword: norm:;
  slot surface, required-init-keyword: surface:;
end;

define sealed domain make(singleton(<hit-info>));
define sealed domain initialize(<hit-info>);

define constant $cube-planes :: <simple-object-vector>
  =  vector(vector(vector3D( 0.0,  0.0, -1.0), 0.0), // Front
	    vector(vector3D( 0.0,  0.0,  1.0), -1.0), // Back
	    vector(vector3D(-1.0,  0.0,  0.0), 0.0), // Left
	    vector(vector3D( 1.0,  0.0,  0.0), -1.0), // Right
	    vector(vector3D( 0.0,  1.0,  0.0), -1.0), // Top
	    vector(vector3D( 0.0, -1.0,  0.0), 0.0)); // Bottom

define constant $u-methods :: <simple-object-vector> = vector(x, x, z, z, x, x);
define constant $v-methods :: <simple-object-vector> = vector(y, y, y, y, z, z);

define method real-intersection-before(m :: <cube>, ray :: <ray>, distance :: <fp>,
				       shadow-test? :: <boolean>)
 => (point, normal, surface, new-distance)
  block (shadow-hit)
    let hits = #f;
    for (p :: <simple-object-vector> in $cube-planes, which-one from 0)
      let (norm, displacement) = values(p[0], p[1]);
      let (hit, dist) = plane-intersection(norm, displacement, ray);
      if (hit)
	let hit :: <3D-point> = hit;
	let u-coord :: one-of(x, y, z) = $u-methods[which-one];
	let v-coord :: one-of(x, y, z) = $v-methods[which-one];
	let u :: <fp> = select(u-coord)
			  x => hit.x;
			  y => hit.y;
			  z => hit.z;
			end;
	let v :: <fp> = select(v-coord)
			  x => hit.x;
			  y => hit.y;
			  z => hit.z;
			end;
	
	if (u <= 1.0 & v <= 1.0 &
	      u >= 0.0 & v >= 0.0)
	  if (~hits)
	    if (shadow-test?)
	      shadow-hit(#f);
	    end;
	    hits := make(<stretchy-vector>);
	  end;
	  let hits :: <stretchy-object-vector> = hits;
	  add!(hits,
	       make(<hit-info>, dist: dist, hit: hit, norm: norm,
		    surface: make-surface-closure(which-one, u, v, m)));
	end;
      end if;
    end for;
    
    if (hits)
      let hits :: <stretchy-object-vector> 
	= sort!(hits, test: method(a :: <hit-info>, b :: <hit-info>) => (less :: <boolean>);
				a.dist < b.dist
			    end method);
      let best :: <hit-info> = hits[0];
      values(best.hit, best.norm, best.surface, magnitude(ray.ray-position - best.hit));
    else
      #f;
    end if;
  end block;
end method real-intersection-before;


define method real-intersection-before(m :: <cone>, ray :: <ray>, distance :: <fp>, shadow-test? :: <boolean>)
 => (point, normal, surface, new-distance)

  let hits = make(<stretchy-vector>);

  // check base:
  let (hit, dist) = plane-intersection(vector3D(0.0, -1.0, 0.0), 0.0,
				       ray);
  if (hit & (hit.x * hit.x + hit.z * hit.z) <= 1.0)
    add!(hits, vector(dist, hit, vector3D(0.0, -1.0, 0.0),
		      make-surface-closure(1, hit.x / 2.0 + 0.5, hit.y / 2.0 + 0.5, m)))
  end if;

  // Check side
  if (hits.size > 0)
    let hits = sort!(hits, test: method(a, b) a[0] < b[0]  end method);
    values(hits[0][1], hits[0][2], hits[0][3], magnitude(ray.ray-position - hits[0][1]));
  else
    #f;
  end if;

end method real-intersection-before;

define method real-intersection-before(m :: <cylinder>, ray :: <ray>, distance :: <fp>, shadow-test? :: <boolean>)
 => (point, normal, surface, new-distance)

  let hits = make(<stretchy-vector>);
  // Check top:
  let (hit, dist) = plane-intersection(vector3D(0.0, 1.0, 0.0), -1.0,
				       ray);
  if (hit & (hit.x * hit.x + hit.z * hit.z) <= 1.0)
    add!(hits, vector(dist, hit, vector3D(0.0, 1.0, 0.0),
		      make-surface-closure(1, hit.x / 2.0 + 0.5, hit.z / 2.0 + 0.5, m)))
  end if;
  // Check bottom:
  let (hit, dist) = plane-intersection(vector3D(0.0, -1.0, 0.0), 0.0,
				       ray);
  if (hit & (hit.x * hit.x + hit.z * hit.z) <= 1.0)
    add!(hits, vector(dist, hit, vector3D(0.0, -1.0, 0.0),
		      make-surface-closure(2, hit.x / 2.0 + 0.5, hit.z / 2.0 + 0.5, m)))
  end if;

  // Check side:
  let ray-to-center = vector3D(ray.ray-position.x, 0.0, ray.ray-position.z);
  let t_ca = -ray-to-center * vector3D(ray.ray-direction.x, 0.0, ray.ray-direction.z);
  let l_oc_2 = ray-to-center * ray-to-center;
  let d_2 = l_oc_2 - (t_ca * t_ca);

  if (d_2 <= 1.0)
    let t_hc = sqrt(1.0 - d_2);

    let hit = point3D(ray.ray-position.x + ray.ray-direction.x * (t_ca - t_hc),
		      0.0, 
		      ray.ray-position.z + ray.ray-direction.z * (t_ca - t_hc),
		      1.0);
    
    let real-hit = #f;
    if (ray.ray-direction.x ~= 0.0)
      real-hit := ray.ray-position + (ray.ray-direction * (hit.x /
								ray.ray-direction.x));
    elseif(ray.ray-direction.z ~= 0.0)
      real-hit := ray.ray-position + (ray.ray-direction * (hit.z / ray.ray-direction.z));
    end if;
    if (real-hit & real-hit.y >= 0.0 & real-hit.y <= 1.0)
      add!(hits, vector(magnitude(real-hit - ray.ray-position),
			real-hit, hit - $origin,
			make-surface-closure(0,
					     atan2(real-hit.x, real-hit.z) / (2.0 * $pi), 
					     real-hit.y, m)));
    end if;
  end if;

  if (hits.size > 0)
    let hits = sort!(hits, test: method(a, b) a[0] < b[0]  end method);
    values(hits[0][1], hits[0][2], hits[0][3], magnitude(ray.ray-position - hits[0][1]));
  else
    #f;
  end if;
end method real-intersection-before;

// CSG:
define method real-intersection-before(m :: <csg-union>, ray :: <ray>, distance :: <fp>,
				       shadow-test? :: <boolean>)
 => (point, normal, surface, new-distance)

  block(easy-out)
    let closest = distance;
    let (best-point, best-normal, best-surface) = #f;
    for (o in m.objects)
      let (p, n, surf, dist) = intersection-before(o, ray, closest, shadow-test?);
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
