module: icfp2000
synopsis: Recursive raytracing renderer
authors: Andreas Bogk, Jeff Dubrule, Bruce Hoult
copyright: this program may be freely used by anyone, for any purpose

define method ray-color
    (o :: <obj>, pos :: <vector>, ray :: <vector>, depth :: <integer>, 
     ambient, lights :: <collection>)
 => (color)

  if (depth > 0)
    let (point, normal, surf-color, surf-diffuse, surf-specular,
	 surf-phong) = first-intersecting-surface(o, pos, ray);

    if (point)
      let reflection-color = 0;
      if (surf-specular)
	let reflected-ray = 2 * normal * (normal * ray) - ray;
	reflection-color = ray-color(o, point, reflected-ray, depth - 1, 
				     ambient, lights);
      end if;
      

    end if;    
  end if;


  ambient; // background color * ambient ???   
end method ray-color;

define method render-image(o, depth :: <integer>, filename, ambient, 
			   lights :: <collection>, )
  let canvas = make(<ppm-image>, filename, depth, width, height);

  for (y from 0 below height)
    for (x from 0 below width)
      
    end for;
  end for;
end method render-image;
