module: ray-tracer
synopsis: Lighting classes & functions
authors: Andreas Bogk, Jeff Dubrule, Bruce Hoult
copyright: this program may be freely used by anyone, for any purpose

define abstract class <light> (<object>)
  slot light-color :: <color>, required-init-keyword: #"color";
end class <light>;

define class <star> (<light>)
  slot direction :: <3D-vector>, required-init-keyword: #"direction";
end class <star>;

define method initialize(s :: <star>, #key, #all-keys)
  s.direction :=  normalize(s.direction);
end method initialize;

define method intensity-on
    (light :: <star>, point :: <3D-point>, normal :: <3D-vector>)
 => (color :: <color>)

  let angle-factor = -light.direction * normal;
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
  
  let ray-to-viewer = normalize(viewer - point);
  let angle-factor = ((ray-to-viewer - light.direction) * 0.5) * normal;
  if (angle-factor < 0.0)
    make-black();
  else
    light.light-color * angle-factor ^ phong-exp;
  end if;
end method phong-intensity-on;

/* Shadow stuff */

define method can-see(o :: <obj>, point :: <3D-point>, l :: <star>)
 => (unblocked :: <boolean>)
  ~intersection-before(o, 
		       make(<ray>, position: point +
			      $surface-acne-prevention-offset * -l.direction, 
			    direction: -l.direction), 
		       1.0/0.0, shadow-test: #t);
end method can-see;


/* Messy exponent function */

define method \^ (b :: <fp>, x :: <fp>)
 => (y :: <fp>)
  exp(log(b) * x);
end method;
