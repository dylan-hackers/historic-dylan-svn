module: ray-tracer
synopsis: Lighting classes & functions
authors: Andreas Bogk, Jeff Dubrule, Bruce Hoult
copyright: this program may be freely used by anyone, for any purpose

define abstract class <light> (<object>)
  slot light-color :: <color>, required-init-keyword: #"color";
end class <light>;

define sealed domain make(singleton(<light>));
define sealed domain initialize(<light>);


define class <star> (<light>)
  slot direction :: <3D-vector>, required-init-keyword: #"direction";
end class <star>;

define sealed domain make(singleton(<star>));


define class <firefly> (<light>)
  slot location :: <3D-point>, required-init-keyword: #"location";
end class <firefly>;

define sealed domain make(singleton(<firefly>));


define class <flashlight> (<light>)
  slot location :: <3D-point>, required-init-keyword: #"location";
  slot direction :: <3D-vector>, required-init-keyword: #"direction";
  slot cutoff :: <fp>, required-init-keyword: #"cutoff";
  slot exponent :: <fp>, required-init-keyword: #"exponent";
end class <flashlight>;

define sealed domain make(singleton(<flashlight>));

// Star stuff:
define method initialize(s :: <star>, #key, #all-keys)
  s.direction :=  normalize(s.direction);
end method initialize;

define method intensity-on
    (light :: <star>, point :: <3D-point>, normal :: <3D-vector>)
 => (color :: <color>)

  let light-dir = light.direction;
  let angle-factor :: <fp> = -(light-dir * normal);
  if (angle-factor < 0.0)
    make-black();
  else
    light.light-color * angle-factor;
  end if;
end method intensity-on;

define method phong-intensity-on
    (light :: <star>, point :: <3D-point>, viewer :: <3D-point>,
     normal :: <3D-vector>, phong-exp :: <fp>)
 => (color :: <color>)
  
  let ray-to-viewer :: <3D-vector> = normalize(viewer - point);
  let light-dir = light.direction;
  let angle-factor :: <fp> = ((ray-to-viewer - light-dir) * 0.5) * normal;
  if (angle-factor < 0.0)
    make-black();
  else
    light.light-color * angle-factor ^ phong-exp;
  end if;
end method phong-intensity-on;

// Fireflies:
define method intensity-on
    (light :: <firefly>, point :: <3D-point>, normal :: <3D-vector>)
 => (color :: <color>)

  let ray :: <3D-vector> = light.location - point;
  let angle-factor :: <fp> = normalize(ray) * normal;
  if (angle-factor < 0.0)
    make-black();
  else
    let color :: <color> = light.light-color;
    let ray-magnitude :: <fp> = magnitude(ray);
    color * angle-factor * 
      (100.0 / (99.0 + ray-magnitude * ray-magnitude));
  end if;
end method intensity-on;

define method phong-intensity-on
    (light :: <firefly>, point :: <3D-point>, viewer :: <3D-point>,
     normal :: <3D-vector>, phong-exp :: <fp>)
 => (color :: <color>)
  
  let ray-to-viewer :: <3D-vector> = normalize(viewer - point);
  let ray-to-light :: <3D-vector> = light.location - point;
  let angle-factor :: <fp> = ((ray-to-viewer - normalize(ray-to-light)) * 0.5) * normal;
  if (angle-factor < 0.0)
    make-black();
  else
    let color :: <fp> = light.light-color;
    let ray-magnitude :: <fp> = magnitude(ray-to-light);
    color * angle-factor ^ phong-exp * (100.0 / (99.0 + ray-magnitude * ray-magnitude));
  end if;
end method phong-intensity-on;

// Flashlights:
define method intensity-on
    (light :: <flashlight>, point :: <3D-point>, normal :: <3D-vector>)
 => (color :: <color>)

  let ray :: <3D-vector> = light.location - point;
  if (abs(normalize(ray) * normalize(-light.direction)) <
	cos(light.cutoff))
    make-black();
  else
    let angle-factor = normalize(ray) * normal;
    if (angle-factor < 0.0)
      make-black();
    else
      let color :: <color> = light.light-color;
      let direction :: <3D-vector> = light.direction;
      let norm :: <3D-vector> = normalize(-ray);
      let exponent :: <fp> = light.exponent;
      let magnitude :: <fp> = magnitude(ray);
      color * angle-factor * 
	abs(direction * norm) ^ exponent *
	(100.0 / (99.0 + magnitude * magnitude));
    end if;
  end if;
end method intensity-on;

define method phong-intensity-on
    (light :: <flashlight>, point :: <3D-point>, viewer :: <3D-point>,
     normal :: <3D-vector>, phong-exp :: <fp>)
 => (color :: <color>)
  
  let ray-to-viewer = normalize(viewer - point);
  let ray-to-light = light.location - point;
  let direction :: <3D-vector> = light.direction;
  let cutoff :: <fp> = light.cutoff;
  if (abs(normalize(ray-to-light) * normalize(-direction)) >
	cos(cutoff))
    make-black();
  else
    let angle-factor = ((ray-to-viewer - normalize(ray-to-light)) * 0.5) * normal;
    if (angle-factor < 0.0)
      make-black();
    else
      let color :: <color> = light.light-color;
      let direction :: <3D-vector> = light.direction;
      let exponent :: <fp> = light.exponent;
      let magnitude = magnitude(ray-to-light);
      color * angle-factor ^ phong-exp * 
	abs(direction * normalize(-ray-to-light)) ^ exponent * 
	(100.0 / (99.0 + magnitude * magnitude));
    end if;
  end if;
end method phong-intensity-on;


/* Shadow stuff */

define method can-see(o :: <obj>, point :: <3D-point>, l :: <star>)
 => (unblocked :: <boolean>)
  ~intersection-before(o, 
		       make(<ray>, position: point +
			      $surface-acne-prevention-offset * -l.direction, 
			    direction: -l.direction), 
		       1.0/0.0, #t);
end method can-see;

define method can-see(o :: <obj>, point :: <3D-point>, l :: <firefly>)
 => (unblocked :: <boolean>)
  let ray-to-light = normalize(l.location - point);
  ~intersection-before(o, 
		       make(<ray>, position: point +
			      $surface-acne-prevention-offset * ray-to-light,
			    direction: ray-to-light), 
		       1.0/0.0, #t);
end method can-see;

define method can-see(o :: <obj>, point :: <3D-point>, l :: <flashlight>)
 => (unblocked :: <boolean>)
  let ray-to-light = normalize(l.location - point);
  if (abs(ray-to-light * normalize(-l.direction)) <
	cos(l.cutoff))
    #f;
  else
    ~intersection-before(o, 
			 make(<ray>, position: point +
				$surface-acne-prevention-offset * ray-to-light,
			      direction: ray-to-light), 
			 1.0/0.0, #t);
  end if;
end method can-see;


define sealed domain can-see(<obj>, <3D-point>, <light>);
define sealed domain phong-intensity-on(<light>, <3D-point>, <3D-point>, <3D-vector>, <fp>);
define sealed domain intensity-on(<light>, <3D-point>, <3D-vector>);

/* Messy exponent function */

define method \^ (b :: <fp>, x :: <fp>)
 => (y :: <fp>)
  exp(log(b) * x);
end method;
