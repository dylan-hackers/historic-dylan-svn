module: ray-tracer
synopsis: Recursive raytracing renderer
authors: Andreas Bogk, Jeff Dubrule, Bruce Hoult
copyright: this program may be freely used by anyone, for any purpose

define constant $eye-pos :: <3D-point> = point3D(0.0, 0.0, -1.0, 1.0);

// When recursing, move a tad in the direction of the ray your going
// for to avoid getting cut off by the surface you're leaving.
define constant $surface-acne-prevention-offset = 0.000000000001;

define method get-tracer(o :: <obj>, ambient :: <color>, 
			 lights :: <collection>) => (tracer)
  local method tracer(ray, depth)
    if (depth > 0)
      let (point, normal, surface-method, distance) = 
	intersection-before(o, ray, 1.0/0.0);
      
      if (point)
	let reflection-color = make-black();
	let surf = surface-method();
	if (surf.specular-coefficient)
	  let reflection-vector = 2.0 * normal * 
	    (normal * ray.ray-direction) - ray.ray-direction;
	  let reflected-ray = make(<ray>, direction:
				     reflection-vector, 
				   position: point +
				     $surface-acne-prevention-offset *
				     reflection-vector);
	  reflection-color = tracer(reflected-ray, depth - 1);
	end if; 
	  
	let c = surf.color * (surf.diffusion-coefficient * ambient +
				surf.specular-coefficient * reflection-color);
	for (l in lights)
	  if (can-see(o, point, l))
	    c := c + surf.color * 
	      (intensity-on(l, point, normal) * surf.diffusion-coefficient
		+ phong-intensity-on(l, point, ray.ray-position, normal, surf.phong-coefficient)
		 * surf.specular-coefficient);
	  end if;
	end for;  
	c;
      else
	make-black();
      end if;    
    else 
      make-black();
    end if;
  end method;
  tracer;
end method get-tracer;

define method render-image(o, depth :: <integer>, filename, ambient :: <color>, 
			   lights :: <collection>, width :: <integer>,
			   height :: <integer>, fov :: <fp>)
  let canvas = make(<ppm-image>, filename: filename, depth: 255,
		    width: width, height: height);

  let world-width = 2.0 * tan(fov / 2.0);
  let world-height = world-width * as(<fp>, height) / as(<fp>, width);

  let trace = get-tracer(o, ambient, lights);

  for (row from truncate/(height, 2) above truncate/(-height, 2) by -1)
    for (column from truncate/(-width, 2) below truncate/(width, 2))
      let world-x :: <fp> = (as(<fp>, column) / as(<fp>, width))  * world-width;
      let world-y :: <fp> = (as(<fp>, row)    / as(<fp>, height)) * world-height;
      let ray = make(<ray>,
		     position: $eye-pos,
		     direction: point3D(world-x, world-y, 0.0, 1.0) -
		       $eye-pos);
      write-pixel(canvas, trace(ray, depth));
    end for;
  end for;

  close-ppm(canvas);
end method render-image;
