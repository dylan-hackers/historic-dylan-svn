module: opengl-osmesa
synopsis: Dylan bindings for OSMesa functions
author: Jeff Dubrule <igor@pobox.com>
copyright: (C) Jefferson Dubrule.  See COPYING.LIB for license details.

define interface
  #include "GL/osmesa.h",
    equate: {"GLenum"    => <GLenum>},
	     
    exclude: {"GLenum"},
  
// Generally useful mappings:  
    map: {"char*" => <byte-string>},
    equate: {"char*" => <c-string>};

end interface;
