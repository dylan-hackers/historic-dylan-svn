module: ray-tracer
synopsis: Color manipulation code
authors: Andreas Bogk, Jeff Dubrule, Bruce Hoult
copyright: this program may be freely used by anyone, for any purpose

define constant $color-component-type = <double-float>;

define class <color> (<object>)
  slot red :: $color-component-type, required-init-keyword: #"red";
  slot green :: $color-component-type, required-init-keyword: #"green";
  slot blue :: $color-component-type, required-init-keyword: #"blue";
end class <color>;

define sealed domain make(singleton(<color>));
define sealed domain initialize(<color>);

define inline method make-black() => (black :: <color>)
  make(<color>, red: 0.0, green: 0.0, blue: 0.0);
end method make-black;

define inline method make-white() => (white :: <color>)
  make(<color>, red: 1.0, green: 1.0, blue: 1.0);
end method make-white;

define inline method export-with-depth(c :: <color>, depth :: <integer>) 
 => (r :: <integer>, g :: <integer>, b :: <integer>)
  local method clampint(x :: <integer>) => (clamped :: <integer>);
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
  let r :: <integer> = floor(c.red * d);
  let g :: <integer> = floor(c.green * d);
  let b :: <integer> = floor(c.blue * d);

  values(clampint(r), clampint(g), clampint(b));
end method export-with-depth;

define inline method \* (c :: <color>, x :: <fp>)
 => (scaled-color :: <color>)
  make(<color>, 
       red:   clamp(c.red   * x),
       green: clamp(c.green * x),
       blue:  clamp(c.blue  * x));
end method;

define inline method \* (x :: <fp>, c :: <color>)
 => (scaled-color :: <color>)
  c * x;
end method;

define inline method \* (c1 :: <color>, c2 :: <color>)
 => (subtractive :: <color>)
  make(<color>,
         red: c1.red * c2.red,
         green: c1.green * c2.green,
         blue: c1.blue * c2.blue
       );
end method;

define inline method \+ (c1 :: <color>, c2 :: <color>)
    make(<color>, 
       red:   clamp(c1.red   + c2.red),
       green: clamp(c1.green + c2.green),
       blue:  clamp(c1.blue  + c2.blue));
end method;


define sealed domain \*(<color>, <fp>);
define sealed domain \*(<fp>, <color>);
define sealed domain \*(<color>, <color>);
define sealed domain \+(<color>, <color>);


// Surface stuff:

define class <surface> (<object>)
  slot color :: <color>,
    init-keyword: color:,
    init-value: make(<color>, red: 1.0, green: 1.0, blue: 1.0);
  slot diffusion-coefficient :: <fp>,
    init-keyword: diffusion:, init-value: 1.0;
  slot specular-coefficient :: <fp>,
    init-keyword: specular:, init-value: 0.0;
  slot phong-coefficient :: <fp>,
    init-keyword: phong:, init-value: 0.0;
end class <surface>;

define sealed domain make(singleton(<surface>));
define sealed domain initialize(<surface>);


define method decode-surface-list(rest :: <pair>)
 => (surface :: <surface>);

  let (phong     :: <fp>, rest :: <pair>) = values(rest.head, rest.tail);
  let (specular  :: <fp>, rest :: <pair>) = values(rest.head, rest.tail);
  let (diffusion :: <fp>, rest :: <pair>) = values(rest.head, rest.tail);
  let (color :: <point>, rest :: #().singleton) = values(rest.head, rest.tail);

  make(<surface>,
       color: make(<color>,
		   red: color.point-x, green: color.point-y, blue: color.point-z),
       diffusion: diffusion, specular: specular, phong: phong);
end;

define method make-surface-closure(surface-id, u, v, m :: <primitive>)
 => (surface :: <surface>);

  if (m.constant-surface)
    m.constant-surface;
  else
    decode-surface-list(m.surface-interpreter-entry(list(v, u, surface-id)));
  end;
end method make-surface-closure;

define inline method clamp(x :: <fp>) => (res :: <fp>);
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

define method silly-texture(surface-id :: <integer>, u :: <fp>, v :: <fp>)
  if(u > 0.1 & u < 0.9 & v > 0.1 & v < 0.9)
    values(make(<color>, red: clamp(u), green: clamp(v), blue: 0.0), 1.0, 0.2, 0.2);
  else
    values(make(<color>, red: 0.0, green: 0.0, blue: 0.0), 1.0, 0.2, 0.2);
  end if;
end method silly-texture;

define inline method red-texture(surface-id :: <integer>, u :: <fp>, v :: <fp>)
 => (color :: <color>, diffusion :: <fp>, specular :: <fp>, phong-exp :: <fp>)
  values(make(<color>, red: 1.0, green: 0.0, blue: 0.0), 
	 1.0, 0.0, 5.0);
end method red-texture;

define inline method blue-texture(surface-id :: <integer>, u :: <fp>, v :: <fp>)
 => (color :: <color>, diffusion :: <fp>, specular :: <fp>, phong-exp :: <fp>)
  values(make(<color>, red: 0.0, green: 0.0, blue: 1.0), 
	 1.0, 0.0, 5.0);
end method blue-texture;

define inline method mirror-texture(surface-id :: <integer>, u :: <fp>, v :: <fp>)
 => (color :: <color>, diffusion :: <fp>, specular :: <fp>, phong-exp :: <fp>)
  values(make-white(), 0.0, 1.0, 1.0);
end method mirror-texture;