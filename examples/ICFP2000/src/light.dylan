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


define method intensity-on
    (light :: <star>, point :: <vector>, normal :: <vector>)
 => (color :: <color>)

  light.light-color * (-light.direction * normal);
end method intensity-on;