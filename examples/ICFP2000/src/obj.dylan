module: icfp2000
synopsis: Object model
authors: Andreas Bogk, Jeff Dubrule, Bruce Hoult
copyright: this program may be freely used by anyone, for any purpose

define class <obj> (<object>)
  slot transform :: <matrix>;
  slot model :: type-union(<primitive>, <collection>);
end class <obj>;

define constant <primitive> = type-union(<sphere>);

define class <sphere> (<object>)
  slot radius;
end class <sphere>;
