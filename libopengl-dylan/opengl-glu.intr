module: opengl-glu
synopsis: Dylan bindings for GLUT functions
author: Jeff Dubrule <igor@pobox.com>
copyright: (C) Jefferson Dubrule.  See COPYING.LIB for license details.

define interface
  #include "GL/glu.h",
    name-mapper: minimal-name-mapping,
    exclude: {"gluUnProject4"},

// Generally useful mappings:  
    map: {"char*" => <byte-string>},
    equate: {"char*" => <c-string>};

  //pointer "GLdouble*" => <c-double-vector>, superclasses: {<c-vector>};
  pointer "GLdouble*" => <GLdouble*>, superclasses: {<c-vector>};
end interface;
