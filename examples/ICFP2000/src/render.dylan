module: icfp2000
synopsis: Recursive raytracing renderer
authors: Andreas Bogk, Jeff Dubrule, Bruce Hoult
copyright: this program may be freely used by anyone, for any purpose

define method get-tracer(o :: <obj>, ambient :: <color>, 
			 lights :: <collection>) => (tracer)
  local method tracer(ray, depth)
    if (depth > 0)
      let (point, normal, surf-color, surf-diffuse, surf-specular,
	   surf-phong) = intersection-before(o, ray, 1.0/0.0);
      
      if (point)
	/*	let reflection-color = 0;
	  if (surf-specular)
	    let reflected-ray = 2 * normal * (normal * ray) - ray;
	    reflection-color = tracer(o, point, reflected-ray, depth - 1, 
				      ambient, lights);
	  end if; 
	*/
	make-white();
      else
	make-black();
      end if;    
    else 
      make-black();
    end if;
  end method;
  tracer;
end method get-tracer;

define constant $eye-pos :: <vector> = vector(0.0, 0.0, -1.0, 1.0);

define method render-image(o, depth :: <integer>, filename, ambient :: <color>, 
			   lights :: <collection>, width :: <integer>,
			   height :: <integer>, fov :: <float>)
  let canvas = make(<ppm-image>, filename: filename, depth: 255,
		    width: width, height: height);

  let world-width = 2.0 * tan(fov / 2.0);
  let world-height = world-width * as(<float>, height) / as(<float>, width);

  let trace = get-tracer(o, ambient, lights);

  
  for (y from 0 below height)
    for (x from 0 below width)
      let world-x :: <float> = as(<float>, x - truncate/(width, 2)) 
				  / as(<float>, width) * world-width;
      let world-y :: <float> = as(<float>, y - truncate/(height, 2))
				  / as(<float>, height) * world-height;
      let ray = make(<ray>, position: $eye-pos, direction: vector(world-x, world-y, 0.0, 1.0) - $eye-pos);
      write-pixel(canvas, trace(ray, depth));
    end for;
  end for;

  close-ppm(canvas);
end method render-image;
