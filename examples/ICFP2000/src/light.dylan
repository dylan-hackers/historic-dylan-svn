module: icfp2000
synopsis: Lighting classes & functions
authors: Andreas Bogk, Jeff Dubrule, Bruce Hoult
copyright: this program may be freely used by anyone, for any purpose

define abstract class <light> (<object>)
  slot light-color :: <color>, required-init-keyword: #"color";
end class <light>;

define class <star> (<light>)
  slot direction :: <vector>, required-init-keyword: #"direction";
end class <star>;

define method initialize(s :: <star>, #key, #all-keys)
  s.direction :=  normalize(s.direction);
end method initialize;

define method intensity-on
    (light :: <star>, point :: <vector>, normal :: <vector>, #key
       phong: p = 1.0)
 => (color :: <color>)

  let angle-factor = -light.direction * normal;
  if (angle-factor < 0.0)
    make-black();
  else
    light.light-color * angle-factor ^ p;
  end if;
end method intensity-on;

/* Shadow stuff */

define constant $surface-acne-prevention-offset = 0.000000000001;

define method can-see(o :: <obj>, point :: <vector>, l :: <star>)
 => (unblocked :: <boolean>)
  ~intersection-before(o, 
		       make(<ray>, position: point +
			      $surface-acne-prevention-offset *  -l.direction, 
			    direction: -l.direction), 
		       1.0/0.0, shadow-test: #t);
end method can-see;


/* This was commented out of transcendental.dylan.  Why??? */

define method \^ (b :: <double-float>, x :: <real>)
 => (y :: <double-float>)
  exp(log(b) * x);
end method;
