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
  slot color, init-keyword: color:;
  slot diffusion-coefficient, init-keyword: diffusion:;
  slot specular-coefficient, init-keyword: specular:;
  slot phong-coefficient, init-keyword: phong:;
end class <surface>;

define method intersection-before(o :: <obj>, ray, distance)
 => (point, normal, surf-color, surf-diffuse, surf-specular, surf-phong)

  // First transform the pos/ray/distance into the coordinates of o,
  // using the inverse of o.transform.

  let local-ray = transform-with-matrix(ray, o.inverse-transform);

  let (our-point, our-normal, surf-color, surf-diffuse, surf-specular,
   surf-phong) = intersection-before(o.model, local-ray, distance);

  // Now, transform the point & normal back into the caller's coordinates
  if (our-point)
    let point = o.transform * our-point;
    let normal = o.transform * our-normal;

    values(point, normal, surf-color, surf-diffuse, surf-specular,
	   surf-phong);
  else
    values(#f, #f, #f, #f, #f, #f);
  end if;
  end method intersection-before;

define method intersection-before(m :: <sphere>, ray, distance)
 => (point, normal, surf-color, surf-diffuse, surf-specular,
     surf-phong)

  // XXX: Assuming, for now that the ray comes from outside the sphere.

  /*
  let unit-ray = homogenize(ray);
  unit-ray := normalize(vector(unit-ray[0], unit-ray[1],
			       unit-ray[2])); // XXX
  let homo-pos = homogenize(pos);
  homo-pos := vector(homo-pos[0], homo-pos[1], homo-pos[2]); // XXX
  */

  let t_ca = -ray.ray-position * ray.ray-direction;
  let l_oc_2 = ray.ray-direction * ray.ray-position;

  if (t_ca < 0.0)
    // Pointing away from sphere, no intersection
    values(#f, #f, #f, #f, #f, #f);
  elseif (abs(l_oc_2 - (t_ca * t_ca)) > 1.0)
    values(#f, #f, #f, #f, #f, #f);
  else 
    values(#[ 1.0, 1.0, 1.0, 1.0 ], #[1.0, 1.0, 1.0, 1.0], #f, #f, #f,
	   #f); // XXX
  end if;
end method intersection-before;
    