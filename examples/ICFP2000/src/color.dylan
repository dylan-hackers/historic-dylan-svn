module: icfp2000
synopsis: Color manipulation code
authors: Andreas Bogk, Jeff Dubrule, Bruce Hoult
copyright: this program may be freely used by anyone, for any purpose

define constant $color-component-type = <double-float>;

define class <color> (<object>)
  slot red :: $color-component-type, required-init-keyword: #"red";
  slot green :: $color-component-type, required-init-keyword: #"green";
  slot blue :: $color-component-type, required-init-keyword: #"blue";
end class <color>;

define method make-black() => (black :: <color>)
  make(<color>, red: 0.0, green: 0.0, blue: 0.0);
end method make-black;

define method make-white() => (white :: <color>)
  make(<color>, red: 1.0, green: 1.0, blue: 1.0);
end method make-white;

define method export-with-depth(c :: <color>, depth :: <integer>) 
 => (r :: <integer>, g :: <integer>, b :: <integer>)
  local method clampint(x)
	  if(x < 0)
	    0;
	  else
	    if(x > depth)
	      depth;
	    else
	      x;
	    end if;
	  end if;
	end method clampint;

  let d :: $color-component-type = as($color-component-type, depth);
  let r = floor(c.red * d);
  let g = floor(c.green * d);
  let b = floor(c.blue * d);

  values(clampint(r), clampint(g), clampint(b));
end method export-with-depth;

define method \* (c :: <color>, x :: <number>)
 => (scaled-color :: <color>)
  make(<color>, 
       red:   clamp(c.red   * x),
       green: clamp(c.green * x),
       blue:  clamp(c.blue  * x));
end method;

define method \* (x :: <number>, c :: <color>)
 => (scaled-color :: <color>)
  c * x;
end method;

define method \* (c1 :: <color>, c2 :: <color>)
 => (subtractive :: <color>)
  make(<color>,
         red: c1.red * c2.red,
         green: c1.green * c2.green,
         blue: c1.blue * c2.blue
       );
end method;

define method \+ (c1 :: <color>, c2 :: <color>)
    make(<color>, 
       red:   clamp(c1.red   + c2.red),
       green: clamp(c1.green + c2.green),
       blue:  clamp(c1.blue  + c2.blue));
end method;


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
  if(u > 0.1 & u < 0.9 & v > 0.1 & v < 0.9)
    values(make(<color>, red: clamp(u), green: clamp(v), blue: 0.0), 1.0, 0.2, 0.2);
  else
    values(make(<color>, red: 0.0, green: 0.0, blue: 0.0), 1.0, 0.2, 0.2);
  end if;
end method silly-texture;

define method red-texture(surface-id, u, v)
 => (color, diffusion, specular, phong-exp)
  values(make(<color>, red: 1.0, green: 0.0, blue: 0.0), 
	 1.0, 0.0, 5.0);
end method red-texture;

define method blue-texture(surface-id, u, v)
 => (color, diffusion, specular, phong-exp)
  values(make(<color>, red: 0.0, green: 0.0, blue: 1.0), 
	 1.0, 0.0, 5.0);
end method blue-texture;

define method mirror-texture(surface-id, u, v)
  => (color, diffusion, specular, phong-exp)
  values(make-white(), 0.0, 1.0, 1.0);
end method mirror-texture;