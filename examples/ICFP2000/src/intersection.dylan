module: icfp2000
synopsis: Ray-intersection code
authors: Andreas Bogk, Jeff Dubrule, Bruce Hoult
copyright: this program may be freely used by anyone, for any purpose

define method intersection-before(o :: <obj>, pos, ray, distance)
 => (point, normal, surf-color, surf-diffuse, surf-specular, surf-phong)

  // First transform the pos/ray/distance into the coordinates of o,
  // using o.transform

  let our-pos = pos; // XXX
  let our-ray = ray; // XXX
  let our-distance = distance; // XXX

  let (our-point, our-normal, surf-color, surf-diffuse, surf-specular,
   surf-phong) = intersection-before(o.model, our-pos, our-ray, our-distance);

  // Now, transform the point & normal back into the caller's coordinates

  let point = our-point; // XXX
  let normal = our-normal; // XXX

  values(point, normal, surf-color, surf-diffuse, surf-specular,
	 surf-phong);
end method intersection-before;

define method intersection-before(m :: <sphere>, pos, ray, distance)
 => (point, normal, surf-color, surf-diffuse, surf-specular,
     surf-phong)

  // XXX: Assuming, for now that the ray comes from outside the sphere.

  let t_ca = -pos * ray;

  if (t_ca < 0.0)
    // Pointing away from sphere, no intersection
    values(#f, #f, #f, #f, #f, #f);
  else
    
  end if;
end method intersection-before;
    