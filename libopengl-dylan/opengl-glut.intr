module: opengl-glut
synopsis: Dylan bindings for GLUT functions
author: Jeff Dubrule <igor@pobox.com>
copyright: (C) Jefferson Dubrule.  See COPYING.LIB for license details.

define interface
  #include "GL/glut.h",
    
  name-mapper: minimal-name-mapping,
    
  exclude: {"GLenum"},
    
  // Generally useful mappings:  
  map: {"char*" => <byte-string>,
        "void (*)(void)" => <function>},
    
  rename: {"void (*)(void)" => <function-pointer>},
    
  equate: {"char*" => <c-string>};
  
  function "glutDisplayFunc",
    equate-argument: {1 => <function-pointer>},
    map-argument: {1 => <function>};
  function "glutReshapeFunc",
    equate-argument: {1 => <function-pointer>},
    map-argument: {1 => <function>};
  function "glutKeyboardFunc",
    equate-argument: {1 => <function-pointer>},
    map-argument: {1 => <function>};
  function "glutMouseFunc",
    equate-argument: {1 => <function-pointer>},
    map-argument: {1 => <function>};
  function "glutMotionFunc",
    equate-argument: {1 => <function-pointer>},
    map-argument: {1 => <function>};
  function "glutPassiveMotionFunc",
    equate-argument: {1 => <function-pointer>},
    map-argument: {1 => <function>};
  function "glutEntryFunc",
    equate-argument: {1 => <function-pointer>},
    map-argument: {1 => <function>};
  function "glutVisibilityFunc",
    equate-argument: {1 => <function-pointer>},
    map-argument: {1 => <function>};
  function "glutIdleFunc",
    equate-argument: {1 => <function-pointer>},
    map-argument: {1 => <function>};
  function "glutTimerFunc",
    equate-argument: {2 => <function-pointer>},
    map-argument: {2 => <function>};
  function "glutCreateMenu",
    equate-argument: {1 => <function-pointer>},
    map-argument: {1 => <function>};
  function "glutMenuStateFunc",
    equate-argument: {1 => <function-pointer>},
    map-argument: {1 => <function>};
  function "glutSpecialFunc",
    equate-argument: {1 => <function-pointer>},
    map-argument: {1 => <function>};
  function "glutSpaceballMotionFunc",
    equate-argument: {1 => <function-pointer>},
    map-argument: {1 => <function>};
  function "glutSpaceballRotateFunc",
    equate-argument: {1 => <function-pointer>},
    map-argument: {1 => <function>};
  function "glutSpaceballButtonFunc",
    equate-argument: {1 => <function-pointer>},
    map-argument: {1 => <function>};
  function "glutButtonBoxFunc",
    equate-argument: {1 => <function-pointer>},
    map-argument: {1 => <function>};
  function "glutDialsFunc",
    equate-argument: {1 => <function-pointer>},
    map-argument: {1 => <function>};
  function "glutTabletMotionFunc",
    equate-argument: {1 => <function-pointer>},
    map-argument: {1 => <function>};
  function "glutTabletButtonFunc",
    equate-argument: {1 => <function-pointer>},
    map-argument: {1 => <function>};
  function "glutMenuStatusFunc",
    equate-argument: {1 => <function-pointer>},
    map-argument: {1 => <function>};
  function "glutOverlayDisplayFunc",
    equate-argument: {1 => <function-pointer>},
    map-argument: {1 => <function>};
  function "glutWindowStatusFunc",
    equate-argument: {1 => <function-pointer>},
    map-argument: {1 => <function>};
  function "glutKeyboardUpFunc",
    equate-argument: {1 => <function-pointer>},
    map-argument: {1 => <function>};
  function "glutSpecialUpFunc",
    equate-argument: {1 => <function-pointer>},
    map-argument: {1 => <function>};
  function "glutJoystickFunc",
    equate-argument: {1 => <function-pointer>},
    map-argument: {1 => <function>};

end interface;


// Snarfed from EMK's gtk binding code:
define method export-value(cls == <function-pointer>, value :: <function>) => (result :: <function-pointer>);
  make(<function-pointer>, pointer: value.callback-entry); 
end method export-value;

define method import-value(cls == <function>, value :: <function-pointer>) => (result :: <function>);
  error("Is this possible?");
end method import-value;


define constant $GLUT-BITMAP-9-BY-15 = 
  as(<machine-pointer>, c-expr(ptr: "GLUT_BITMAP_9_BY_15"));
define constant $GLUT-BITMAP-8-BY-13 = 
  as(<machine-pointer>, c-expr(ptr: "GLUT_BITMAP_8_BY_13"));
define constant $GLUT-BITMAP-TIMES-ROMAN-10 = 
  as(<machine-pointer>, c-expr(ptr: "GLUT_BITMAP_TIMES_ROMAN_10"));
define constant $GLUT-BITMAP-TIMES-ROMAN-24 = 
  as(<machine-pointer>, c-expr(ptr: "GLUT_BITMAP_TIMES_ROMAN_24"));
define constant $GLUT-BITMAP-HELVETICA-10 = 
  as(<machine-pointer>, c-expr(ptr: "GLUT_BITMAP_HELVETICA_10"));
define constant $GLUT-BITMAP-HELVETICA-12 = 
  as(<machine-pointer>, c-expr(ptr: "GLUT_BITMAP_HELVETICA_12"));
define constant $GLUT-BITMAP-HELVETICA-18 = 
  as(<machine-pointer>, c-expr(ptr: "GLUT_BITMAP_HELVETICA_18"));
define constant $GLUT-STROKE-ROMAN = 
  as(<machine-pointer>, c-expr(ptr: "GLUT_STROKE_ROMAN"));
define constant $GLUT-STROKE-MONO-ROMAN = 
  as(<machine-pointer>, c-expr(ptr: "GLUT_STROKE_MONO_ROMAN"));


define method glut-init
    ()
 => ();
 c-expr( void: "extern int application_argc;" );
 c-expr( void: "extern char **application_argv;" );
  call-out( "glutInit", void:, 
  	ptr: c-expr( ptr: "&application_argc" ), 
  	ptr: c-expr( ptr: "application_argv" ) );
  values();
end method glut-init;
