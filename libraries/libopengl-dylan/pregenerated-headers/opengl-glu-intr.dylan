module: opengl-glu
synopsis: Dylan bindings for GLUT functions
author: Jeff Dubrule <igor@pobox.com>
copyright: (C) Jefferson Dubrule.  See COPYING.LIB for license details.

c-include("/usr/X11R6/include/GL/glu.h");

define functional class <GLUnurbs> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<GLUnurbs>));

define method pointer-value (value :: <GLUnurbs>, #key index = 0) => (result :: <GLUnurbs>);
  value + index * 0;
end method pointer-value;

define method content-size (value :: subclass(<GLUnurbs>)) => (result :: <integer>);
  0;
end method content-size;

define functional class <GLUquadric> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<GLUquadric>));

define method pointer-value (value :: <GLUquadric>, #key index = 0) => (result :: <GLUquadric>);
  value + index * 0;
end method pointer-value;

define method content-size (value :: subclass(<GLUquadric>)) => (result :: <integer>);
  0;
end method content-size;

define functional class <GLUtesselator> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<GLUtesselator>));

define method pointer-value (value :: <GLUtesselator>, #key index = 0) => (result :: <GLUtesselator>);
  value + index * 0;
end method pointer-value;

define method content-size (value :: subclass(<GLUtesselator>)) => (result :: <integer>);
  0;
end method content-size;

define constant <GLUnurbsObj> = <GLUnurbs>;

define constant <GLUquadricObj> = <GLUquadric>;

define constant <GLUtesselatorObj> = <GLUtesselator>;

define constant <GLUtriangulatorObj> = <GLUtesselator>;

define constant <GLvoid> = <void>;

define functional class <anonymous-1153> (<function-pointer>) end;

define constant <_GLUfuncptr> = <anonymous-1153>;

define function gluBeginCurve
    (arg1 :: <GLUnurbs>)
 => ();
  call-out("gluBeginCurve", void:, ptr:, (arg1).raw-value);
  values();
end function gluBeginCurve;

define function gluBeginPolygon
    (arg1 :: <GLUtesselator>)
 => ();
  call-out("gluBeginPolygon", void:, ptr:, (arg1).raw-value);
  values();
end function gluBeginPolygon;

define function gluBeginSurface
    (arg1 :: <GLUnurbs>)
 => ();
  call-out("gluBeginSurface", void:, ptr:, (arg1).raw-value);
  values();
end function gluBeginSurface;

define function gluBeginTrim
    (arg1 :: <GLUnurbs>)
 => ();
  call-out("gluBeginTrim", void:, ptr:, (arg1).raw-value);
  values();
end function gluBeginTrim;

define constant <GLint> = <integer>;

define constant <GLenum> = <integer>;

define constant <GLsizei> = <integer>;

define function gluBuild1DMipmapLevels
    (arg1 :: <GLenum>, arg2 :: <GLint>, arg3 :: <GLsizei>, arg4 :: <GLenum>, arg5 :: <GLenum>, arg6 :: <GLint>, arg7 :: <GLint>, arg8 :: <GLint>, arg9 :: <machine-pointer>)
 => (result :: <GLint>);
  let result-value
    = call-out("gluBuild1DMipmapLevels", int:, unsigned-int:, arg1, int:, arg2, int:, arg3, unsigned-int:, arg4, unsigned-int:, arg5, int:, arg6, int:, arg7, int:, arg8, ptr:, (arg9).raw-value);
  values(result-value);
end function gluBuild1DMipmapLevels;

define function gluBuild1DMipmaps
    (arg1 :: <GLenum>, arg2 :: <GLint>, arg3 :: <GLsizei>, arg4 :: <GLenum>, arg5 :: <GLenum>, arg6 :: <machine-pointer>)
 => (result :: <GLint>);
  let result-value
    = call-out("gluBuild1DMipmaps", int:, unsigned-int:, arg1, int:, arg2, int:, arg3, unsigned-int:, arg4, unsigned-int:, arg5, ptr:, (arg6).raw-value);
  values(result-value);
end function gluBuild1DMipmaps;

define function gluBuild2DMipmapLevels
    (arg1 :: <GLenum>, arg2 :: <GLint>, arg3 :: <GLsizei>, arg4 :: <GLsizei>, arg5 :: <GLenum>, arg6 :: <GLenum>, arg7 :: <GLint>, arg8 :: <GLint>, arg9 :: <GLint>, arg10 :: <machine-pointer>)
 => (result :: <GLint>);
  let result-value
    = call-out("gluBuild2DMipmapLevels", int:, unsigned-int:, arg1, int:, arg2, int:, arg3, int:, arg4, unsigned-int:, arg5, unsigned-int:, arg6, int:, arg7, int:, arg8, int:, arg9, ptr:, (arg10).raw-value);
  values(result-value);
end function gluBuild2DMipmapLevels;

define function gluBuild2DMipmaps
    (arg1 :: <GLenum>, arg2 :: <GLint>, arg3 :: <GLsizei>, arg4 :: <GLsizei>, arg5 :: <GLenum>, arg6 :: <GLenum>, arg7 :: <machine-pointer>)
 => (result :: <GLint>);
  let result-value
    = call-out("gluBuild2DMipmaps", int:, unsigned-int:, arg1, int:, arg2, int:, arg3, int:, arg4, unsigned-int:, arg5, unsigned-int:, arg6, ptr:, (arg7).raw-value);
  values(result-value);
end function gluBuild2DMipmaps;

define function gluBuild3DMipmapLevels
    (arg1 :: <GLenum>, arg2 :: <GLint>, arg3 :: <GLsizei>, arg4 :: <GLsizei>, arg5 :: <GLsizei>, arg6 :: <GLenum>, arg7 :: <GLenum>, arg8 :: <GLint>, arg9 :: <GLint>, arg10 :: <GLint>, arg11 :: <machine-pointer>)
 => (result :: <GLint>);
  let result-value
    = call-out("gluBuild3DMipmapLevels", int:, unsigned-int:, arg1, int:, arg2, int:, arg3, int:, arg4, int:, arg5, unsigned-int:, arg6, unsigned-int:, arg7, int:, arg8, int:, arg9, int:, arg10, ptr:, (arg11).raw-value);
  values(result-value);
end function gluBuild3DMipmapLevels;

define function gluBuild3DMipmaps
    (arg1 :: <GLenum>, arg2 :: <GLint>, arg3 :: <GLsizei>, arg4 :: <GLsizei>, arg5 :: <GLsizei>, arg6 :: <GLenum>, arg7 :: <GLenum>, arg8 :: <machine-pointer>)
 => (result :: <GLint>);
  let result-value
    = call-out("gluBuild3DMipmaps", int:, unsigned-int:, arg1, int:, arg2, int:, arg3, int:, arg4, int:, arg5, unsigned-int:, arg6, unsigned-int:, arg7, ptr:, (arg8).raw-value);
  values(result-value);
end function gluBuild3DMipmaps;

define constant <GLboolean> = <integer>;

define constant <GLubyte> = <integer>;

define functional class <GLubyte*> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<GLubyte*>));

define inline method pointer-value
    (ptr :: <GLubyte*>, #key index = 0)
 => (result :: <GLubyte>);
  unsigned-byte-at(ptr, offset: index * 1);
end method pointer-value;

define inline method pointer-value-setter
    (value :: <GLubyte>, ptr :: <GLubyte*>, #key index = 0)
 => (result :: <GLubyte>);
  unsigned-byte-at(ptr, offset: index * 1) := value;
  value;
end method pointer-value-setter;

define method content-size (value :: subclass(<GLubyte*>)) => (result :: <integer>);
  1;
end method content-size;

define function gluCheckExtension
    (arg1 :: <GLubyte*>, arg2 :: <GLubyte*>)
 => (result :: <GLboolean>);
  let result-value
    = call-out("gluCheckExtension", unsigned-char:, ptr:, (arg1).raw-value, ptr:, (arg2).raw-value);
  values(result-value);
end function gluCheckExtension;

define constant <GLdouble> = <double-float>;

define function gluCylinder
    (arg1 :: <GLUquadric>, arg2 :: <GLdouble>, arg3 :: <GLdouble>, arg4 :: <GLdouble>, arg5 :: <GLint>, arg6 :: <GLint>)
 => ();
  call-out("gluCylinder", void:, ptr:, (arg1).raw-value, double:, arg2, double:, arg3, double:, arg4, int:, arg5, int:, arg6);
  values();
end function gluCylinder;

define function gluDeleteNurbsRenderer
    (arg1 :: <GLUnurbs>)
 => ();
  call-out("gluDeleteNurbsRenderer", void:, ptr:, (arg1).raw-value);
  values();
end function gluDeleteNurbsRenderer;

define function gluDeleteQuadric
    (arg1 :: <GLUquadric>)
 => ();
  call-out("gluDeleteQuadric", void:, ptr:, (arg1).raw-value);
  values();
end function gluDeleteQuadric;

define function gluDeleteTess
    (arg1 :: <GLUtesselator>)
 => ();
  call-out("gluDeleteTess", void:, ptr:, (arg1).raw-value);
  values();
end function gluDeleteTess;

define function gluDisk
    (arg1 :: <GLUquadric>, arg2 :: <GLdouble>, arg3 :: <GLdouble>, arg4 :: <GLint>, arg5 :: <GLint>)
 => ();
  call-out("gluDisk", void:, ptr:, (arg1).raw-value, double:, arg2, double:, arg3, int:, arg4, int:, arg5);
  values();
end function gluDisk;

define function gluEndCurve
    (arg1 :: <GLUnurbs>)
 => ();
  call-out("gluEndCurve", void:, ptr:, (arg1).raw-value);
  values();
end function gluEndCurve;

define function gluEndPolygon
    (arg1 :: <GLUtesselator>)
 => ();
  call-out("gluEndPolygon", void:, ptr:, (arg1).raw-value);
  values();
end function gluEndPolygon;

define function gluEndSurface
    (arg1 :: <GLUnurbs>)
 => ();
  call-out("gluEndSurface", void:, ptr:, (arg1).raw-value);
  values();
end function gluEndSurface;

define function gluEndTrim
    (arg1 :: <GLUnurbs>)
 => ();
  call-out("gluEndTrim", void:, ptr:, (arg1).raw-value);
  values();
end function gluEndTrim;

define function gluErrorString
    (arg1 :: <GLenum>)
 => (result :: <GLubyte*>);
  let result-value
    = call-out("gluErrorString", ptr:, unsigned-int:, arg1);
  let result-value = make(<GLubyte*>, pointer: result-value);
  values(result-value);
end function gluErrorString;

define constant <GLfloat> = <single-float>;

define functional class <GLfloat*> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<GLfloat*>));

define inline method pointer-value
    (ptr :: <GLfloat*>, #key index = 0)
 => (result :: <GLfloat>);
  float-at(ptr, offset: index * 4);
end method pointer-value;

define inline method pointer-value-setter
    (value :: <GLfloat>, ptr :: <GLfloat*>, #key index = 0)
 => (result :: <GLfloat>);
  float-at(ptr, offset: index * 4) := value;
  value;
end method pointer-value-setter;

define method content-size (value :: subclass(<GLfloat*>)) => (result :: <integer>);
  4;
end method content-size;

define function gluGetNurbsProperty
    (arg1 :: <GLUnurbs>, arg2 :: <GLenum>, arg3 :: <GLfloat*>)
 => ();
  call-out("gluGetNurbsProperty", void:, ptr:, (arg1).raw-value, unsigned-int:, arg2, ptr:, (arg3).raw-value);
  values();
end function gluGetNurbsProperty;

define function gluGetString
    (arg1 :: <GLenum>)
 => (result :: <GLubyte*>);
  let result-value
    = call-out("gluGetString", ptr:, unsigned-int:, arg1);
  let result-value = make(<GLubyte*>, pointer: result-value);
  values(result-value);
end function gluGetString;

define functional class <GLdouble*> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<GLdouble*>));

define inline method pointer-value
    (ptr :: <GLdouble*>, #key index = 0)
 => (result :: <GLdouble>);
  double-at(ptr, offset: index * 8);
end method pointer-value;

define inline method pointer-value-setter
    (value :: <GLdouble>, ptr :: <GLdouble*>, #key index = 0)
 => (result :: <GLdouble>);
  double-at(ptr, offset: index * 8) := value;
  value;
end method pointer-value-setter;

define method content-size (value :: subclass(<GLdouble*>)) => (result :: <integer>);
  8;
end method content-size;

define function gluGetTessProperty
    (arg1 :: <GLUtesselator>, arg2 :: <GLenum>, arg3 :: <GLdouble*>)
 => ();
  call-out("gluGetTessProperty", void:, ptr:, (arg1).raw-value, unsigned-int:, arg2, ptr:, (arg3).raw-value);
  values();
end function gluGetTessProperty;

define functional class <GLint*> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<GLint*>));

define inline method pointer-value
    (ptr :: <GLint*>, #key index = 0)
 => (result :: <GLint>);
  signed-long-at(ptr, offset: index * 4);
end method pointer-value;

define inline method pointer-value-setter
    (value :: <GLint>, ptr :: <GLint*>, #key index = 0)
 => (result :: <GLint>);
  signed-long-at(ptr, offset: index * 4) := value;
  value;
end method pointer-value-setter;

define method content-size (value :: subclass(<GLint*>)) => (result :: <integer>);
  4;
end method content-size;

define function gluLoadSamplingMatrices
    (arg1 :: <GLUnurbs>, arg2 :: <GLfloat*>, arg3 :: <GLfloat*>, arg4 :: <GLint*>)
 => ();
  call-out("gluLoadSamplingMatrices", void:, ptr:, (arg1).raw-value, ptr:, (arg2).raw-value, ptr:, (arg3).raw-value, ptr:, (arg4).raw-value);
  values();
end function gluLoadSamplingMatrices;

define function gluLookAt
    (arg1 :: <GLdouble>, arg2 :: <GLdouble>, arg3 :: <GLdouble>, arg4 :: <GLdouble>, arg5 :: <GLdouble>, arg6 :: <GLdouble>, arg7 :: <GLdouble>, arg8 :: <GLdouble>, arg9 :: <GLdouble>)
 => ();
  call-out("gluLookAt", void:, double:, arg1, double:, arg2, double:, arg3, double:, arg4, double:, arg5, double:, arg6, double:, arg7, double:, arg8, double:, arg9);
  values();
end function gluLookAt;

define function gluNewNurbsRenderer
    ()
 => (result :: <GLUnurbs>);
  let result-value
    = call-out("gluNewNurbsRenderer", ptr:);
  let result-value = make(<GLUnurbs>, pointer: result-value);
  values(result-value);
end function gluNewNurbsRenderer;

define function gluNewQuadric
    ()
 => (result :: <GLUquadric>);
  let result-value
    = call-out("gluNewQuadric", ptr:);
  let result-value = make(<GLUquadric>, pointer: result-value);
  values(result-value);
end function gluNewQuadric;

define function gluNewTess
    ()
 => (result :: <GLUtesselator>);
  let result-value
    = call-out("gluNewTess", ptr:);
  let result-value = make(<GLUtesselator>, pointer: result-value);
  values(result-value);
end function gluNewTess;

define function gluNextContour
    (arg1 :: <GLUtesselator>, arg2 :: <GLenum>)
 => ();
  call-out("gluNextContour", void:, ptr:, (arg1).raw-value, unsigned-int:, arg2);
  values();
end function gluNextContour;

define function gluNurbsCallback
    (arg1 :: <GLUnurbs>, arg2 :: <GLenum>, arg3 :: <_GLUfuncptr>)
 => ();
  call-out("gluNurbsCallback", void:, ptr:, (arg1).raw-value, unsigned-int:, arg2, ptr:, (arg3).raw-value);
  values();
end function gluNurbsCallback;

define functional class <GLvoid*> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<GLvoid*>));

define method content-size (value :: subclass(<GLvoid*>)) => (result :: <integer>);
  0;
end method content-size;

define function gluNurbsCallbackData
    (arg1 :: <GLUnurbs>, arg2 :: <GLvoid*>)
 => ();
  call-out("gluNurbsCallbackData", void:, ptr:, (arg1).raw-value, ptr:, (arg2).raw-value);
  values();
end function gluNurbsCallbackData;

define function gluNurbsCallbackDataEXT
    (arg1 :: <GLUnurbs>, arg2 :: <GLvoid*>)
 => ();
  call-out("gluNurbsCallbackDataEXT", void:, ptr:, (arg1).raw-value, ptr:, (arg2).raw-value);
  values();
end function gluNurbsCallbackDataEXT;

define function gluNurbsCurve
    (arg1 :: <GLUnurbs>, arg2 :: <GLint>, arg3 :: <GLfloat*>, arg4 :: <GLint>, arg5 :: <GLfloat*>, arg6 :: <GLint>, arg7 :: <GLenum>)
 => ();
  call-out("gluNurbsCurve", void:, ptr:, (arg1).raw-value, int:, arg2, ptr:, (arg3).raw-value, int:, arg4, ptr:, (arg5).raw-value, int:, arg6, unsigned-int:, arg7);
  values();
end function gluNurbsCurve;

define function gluNurbsProperty
    (arg1 :: <GLUnurbs>, arg2 :: <GLenum>, arg3 :: <GLfloat>)
 => ();
  call-out("gluNurbsProperty", void:, ptr:, (arg1).raw-value, unsigned-int:, arg2, float:, arg3);
  values();
end function gluNurbsProperty;

define function gluNurbsSurface
    (arg1 :: <GLUnurbs>, arg2 :: <GLint>, arg3 :: <GLfloat*>, arg4 :: <GLint>, arg5 :: <GLfloat*>, arg6 :: <GLint>, arg7 :: <GLint>, arg8 :: <GLfloat*>, arg9 :: <GLint>, arg10 :: <GLint>, arg11 :: <GLenum>)
 => ();
  call-out("gluNurbsSurface", void:, ptr:, (arg1).raw-value, int:, arg2, ptr:, (arg3).raw-value, int:, arg4, ptr:, (arg5).raw-value, int:, arg6, int:, arg7, ptr:, (arg8).raw-value, int:, arg9, int:, arg10, unsigned-int:, arg11);
  values();
end function gluNurbsSurface;

define function gluOrtho2D
    (arg1 :: <GLdouble>, arg2 :: <GLdouble>, arg3 :: <GLdouble>, arg4 :: <GLdouble>)
 => ();
  call-out("gluOrtho2D", void:, double:, arg1, double:, arg2, double:, arg3, double:, arg4);
  values();
end function gluOrtho2D;

define function gluPartialDisk
    (arg1 :: <GLUquadric>, arg2 :: <GLdouble>, arg3 :: <GLdouble>, arg4 :: <GLint>, arg5 :: <GLint>, arg6 :: <GLdouble>, arg7 :: <GLdouble>)
 => ();
  call-out("gluPartialDisk", void:, ptr:, (arg1).raw-value, double:, arg2, double:, arg3, int:, arg4, int:, arg5, double:, arg6, double:, arg7);
  values();
end function gluPartialDisk;

define function gluPerspective
    (arg1 :: <GLdouble>, arg2 :: <GLdouble>, arg3 :: <GLdouble>, arg4 :: <GLdouble>)
 => ();
  call-out("gluPerspective", void:, double:, arg1, double:, arg2, double:, arg3, double:, arg4);
  values();
end function gluPerspective;

define function gluPickMatrix
    (arg1 :: <GLdouble>, arg2 :: <GLdouble>, arg3 :: <GLdouble>, arg4 :: <GLdouble>, arg5 :: <GLint*>)
 => ();
  call-out("gluPickMatrix", void:, double:, arg1, double:, arg2, double:, arg3, double:, arg4, ptr:, (arg5).raw-value);
  values();
end function gluPickMatrix;

define function gluProject
    (arg1 :: <GLdouble>, arg2 :: <GLdouble>, arg3 :: <GLdouble>, arg4 :: <GLdouble*>, arg5 :: <GLdouble*>, arg6 :: <GLint*>, arg7 :: <GLdouble*>, arg8 :: <GLdouble*>, arg9 :: <GLdouble*>)
 => (result :: <GLint>);
  let result-value
    = call-out("gluProject", int:, double:, arg1, double:, arg2, double:, arg3, ptr:, (arg4).raw-value, ptr:, (arg5).raw-value, ptr:, (arg6).raw-value, ptr:, (arg7).raw-value, ptr:, (arg8).raw-value, ptr:, (arg9).raw-value);
  values(result-value);
end function gluProject;

define function gluPwlCurve
    (arg1 :: <GLUnurbs>, arg2 :: <GLint>, arg3 :: <GLfloat*>, arg4 :: <GLint>, arg5 :: <GLenum>)
 => ();
  call-out("gluPwlCurve", void:, ptr:, (arg1).raw-value, int:, arg2, ptr:, (arg3).raw-value, int:, arg4, unsigned-int:, arg5);
  values();
end function gluPwlCurve;

define function gluQuadricCallback
    (arg1 :: <GLUquadric>, arg2 :: <GLenum>, arg3 :: <_GLUfuncptr>)
 => ();
  call-out("gluQuadricCallback", void:, ptr:, (arg1).raw-value, unsigned-int:, arg2, ptr:, (arg3).raw-value);
  values();
end function gluQuadricCallback;

define function gluQuadricDrawStyle
    (arg1 :: <GLUquadric>, arg2 :: <GLenum>)
 => ();
  call-out("gluQuadricDrawStyle", void:, ptr:, (arg1).raw-value, unsigned-int:, arg2);
  values();
end function gluQuadricDrawStyle;

define function gluQuadricNormals
    (arg1 :: <GLUquadric>, arg2 :: <GLenum>)
 => ();
  call-out("gluQuadricNormals", void:, ptr:, (arg1).raw-value, unsigned-int:, arg2);
  values();
end function gluQuadricNormals;

define function gluQuadricOrientation
    (arg1 :: <GLUquadric>, arg2 :: <GLenum>)
 => ();
  call-out("gluQuadricOrientation", void:, ptr:, (arg1).raw-value, unsigned-int:, arg2);
  values();
end function gluQuadricOrientation;

define function gluQuadricTexture
    (arg1 :: <GLUquadric>, arg2 :: <GLboolean>)
 => ();
  call-out("gluQuadricTexture", void:, ptr:, (arg1).raw-value, unsigned-char:, arg2);
  values();
end function gluQuadricTexture;

define function gluScaleImage
    (arg1 :: <GLenum>, arg2 :: <GLsizei>, arg3 :: <GLsizei>, arg4 :: <GLenum>, arg5 :: <machine-pointer>, arg6 :: <GLsizei>, arg7 :: <GLsizei>, arg8 :: <GLenum>, arg9 :: <GLvoid*>)
 => (result :: <GLint>);
  let result-value
    = call-out("gluScaleImage", int:, unsigned-int:, arg1, int:, arg2, int:, arg3, unsigned-int:, arg4, ptr:, (arg5).raw-value, int:, arg6, int:, arg7, unsigned-int:, arg8, ptr:, (arg9).raw-value);
  values(result-value);
end function gluScaleImage;

define function gluSphere
    (arg1 :: <GLUquadric>, arg2 :: <GLdouble>, arg3 :: <GLint>, arg4 :: <GLint>)
 => ();
  call-out("gluSphere", void:, ptr:, (arg1).raw-value, double:, arg2, int:, arg3, int:, arg4);
  values();
end function gluSphere;

define function gluTessBeginContour
    (arg1 :: <GLUtesselator>)
 => ();
  call-out("gluTessBeginContour", void:, ptr:, (arg1).raw-value);
  values();
end function gluTessBeginContour;

define function gluTessBeginPolygon
    (arg1 :: <GLUtesselator>, arg2 :: <GLvoid*>)
 => ();
  call-out("gluTessBeginPolygon", void:, ptr:, (arg1).raw-value, ptr:, (arg2).raw-value);
  values();
end function gluTessBeginPolygon;

define function gluTessCallback
    (arg1 :: <GLUtesselator>, arg2 :: <GLenum>, arg3 :: <_GLUfuncptr>)
 => ();
  call-out("gluTessCallback", void:, ptr:, (arg1).raw-value, unsigned-int:, arg2, ptr:, (arg3).raw-value);
  values();
end function gluTessCallback;

define function gluTessEndContour
    (arg1 :: <GLUtesselator>)
 => ();
  call-out("gluTessEndContour", void:, ptr:, (arg1).raw-value);
  values();
end function gluTessEndContour;

define function gluTessEndPolygon
    (arg1 :: <GLUtesselator>)
 => ();
  call-out("gluTessEndPolygon", void:, ptr:, (arg1).raw-value);
  values();
end function gluTessEndPolygon;

define function gluTessNormal
    (arg1 :: <GLUtesselator>, arg2 :: <GLdouble>, arg3 :: <GLdouble>, arg4 :: <GLdouble>)
 => ();
  call-out("gluTessNormal", void:, ptr:, (arg1).raw-value, double:, arg2, double:, arg3, double:, arg4);
  values();
end function gluTessNormal;

define function gluTessProperty
    (arg1 :: <GLUtesselator>, arg2 :: <GLenum>, arg3 :: <GLdouble>)
 => ();
  call-out("gluTessProperty", void:, ptr:, (arg1).raw-value, unsigned-int:, arg2, double:, arg3);
  values();
end function gluTessProperty;

define function gluTessVertex
    (arg1 :: <GLUtesselator>, arg2 :: <GLdouble*>, arg3 :: <GLvoid*>)
 => ();
  call-out("gluTessVertex", void:, ptr:, (arg1).raw-value, ptr:, (arg2).raw-value, ptr:, (arg3).raw-value);
  values();
end function gluTessVertex;

define function gluUnProject
    (arg1 :: <GLdouble>, arg2 :: <GLdouble>, arg3 :: <GLdouble>, arg4 :: <GLdouble*>, arg5 :: <GLdouble*>, arg6 :: <GLint*>, arg7 :: <GLdouble*>, arg8 :: <GLdouble*>, arg9 :: <GLdouble*>)
 => (result :: <GLint>);
  let result-value
    = call-out("gluUnProject", int:, double:, arg1, double:, arg2, double:, arg3, ptr:, (arg4).raw-value, ptr:, (arg5).raw-value, ptr:, (arg6).raw-value, ptr:, (arg7).raw-value, ptr:, (arg8).raw-value, ptr:, (arg9).raw-value);
  values(result-value);
end function gluUnProject;

define function gluUnProject4
    (arg1 :: <GLdouble>, arg2 :: <GLdouble>, arg3 :: <GLdouble>, arg4 :: <GLdouble>, arg5 :: <GLdouble*>, arg6 :: <GLdouble*>, arg7 :: <GLint*>, arg8 :: <GLdouble>, arg9 :: <GLdouble>, arg10 :: <GLdouble*>, arg11 :: <GLdouble*>, arg12 :: <GLdouble*>, arg13 :: <GLdouble*>)
 => (result :: <GLint>);
  let result-value
    = call-out("gluUnProject4", int:, double:, arg1, double:, arg2, double:, arg3, double:, arg4, ptr:, (arg5).raw-value, ptr:, (arg6).raw-value, ptr:, (arg7).raw-value, double:, arg8, double:, arg9, ptr:, (arg10).raw-value, ptr:, (arg11).raw-value, ptr:, (arg12).raw-value, ptr:, (arg13).raw-value);
  values(result-value);
end function gluUnProject4;

define constant $GLU-EXT-object-space-tess = 1;

define constant $GLU-EXT-nurbs-tessellator = 1;

define constant $GLU-FALSE = 0;

define constant $GLU-TRUE = 1;

define constant $GLU-VERSION-1-1 = 1;

define constant $GLU-VERSION-1-2 = 1;

define constant $GLU-VERSION-1-3 = 1;

define constant $GLU-VERSION = 100800;

define constant $GLU-EXTENSIONS = 100801;

define constant $GLU-INVALID-ENUM = 100900;

define constant $GLU-INVALID-VALUE = 100901;

define constant $GLU-OUT-OF-MEMORY = 100902;

define constant $GLU-INVALID-OPERATION = 100904;

define constant $GLU-OUTLINE-POLYGON = 100240;

define constant $GLU-OUTLINE-PATCH = 100241;

define constant $GLU-NURBS-ERROR = 100103;

define constant $GLU-ERROR = 100103;

define constant $GLU-NURBS-BEGIN = 100164;

define constant $GLU-NURBS-BEGIN-EXT = 100164;

define constant $GLU-NURBS-VERTEX = 100165;

define constant $GLU-NURBS-VERTEX-EXT = 100165;

define constant $GLU-NURBS-NORMAL = 100166;

define constant $GLU-NURBS-NORMAL-EXT = 100166;

define constant $GLU-NURBS-COLOR = 100167;

define constant $GLU-NURBS-COLOR-EXT = 100167;

define constant $GLU-NURBS-TEXTURE-COORD = 100168;

define constant $GLU-NURBS-TEX-COORD-EXT = 100168;

define constant $GLU-NURBS-END = 100169;

define constant $GLU-NURBS-END-EXT = 100169;

define constant $GLU-NURBS-BEGIN-DATA = 100170;

define constant $GLU-NURBS-BEGIN-DATA-EXT = 100170;

define constant $GLU-NURBS-VERTEX-DATA = 100171;

define constant $GLU-NURBS-VERTEX-DATA-EXT = 100171;

define constant $GLU-NURBS-NORMAL-DATA = 100172;

define constant $GLU-NURBS-NORMAL-DATA-EXT = 100172;

define constant $GLU-NURBS-COLOR-DATA = 100173;

define constant $GLU-NURBS-COLOR-DATA-EXT = 100173;

define constant $GLU-NURBS-TEXTURE-COORD-DATA = 100174;

define constant $GLU-NURBS-TEX-COORD-DATA-EXT = 100174;

define constant $GLU-NURBS-END-DATA = 100175;

define constant $GLU-NURBS-END-DATA-EXT = 100175;

define constant $GLU-NURBS-ERROR1 = 100251;

define constant $GLU-NURBS-ERROR2 = 100252;

define constant $GLU-NURBS-ERROR3 = 100253;

define constant $GLU-NURBS-ERROR4 = 100254;

define constant $GLU-NURBS-ERROR5 = 100255;

define constant $GLU-NURBS-ERROR6 = 100256;

define constant $GLU-NURBS-ERROR7 = 100257;

define constant $GLU-NURBS-ERROR8 = 100258;

define constant $GLU-NURBS-ERROR9 = 100259;

define constant $GLU-NURBS-ERROR10 = 100260;

define constant $GLU-NURBS-ERROR11 = 100261;

define constant $GLU-NURBS-ERROR12 = 100262;

define constant $GLU-NURBS-ERROR13 = 100263;

define constant $GLU-NURBS-ERROR14 = 100264;

define constant $GLU-NURBS-ERROR15 = 100265;

define constant $GLU-NURBS-ERROR16 = 100266;

define constant $GLU-NURBS-ERROR17 = 100267;

define constant $GLU-NURBS-ERROR18 = 100268;

define constant $GLU-NURBS-ERROR19 = 100269;

define constant $GLU-NURBS-ERROR20 = 100270;

define constant $GLU-NURBS-ERROR21 = 100271;

define constant $GLU-NURBS-ERROR22 = 100272;

define constant $GLU-NURBS-ERROR23 = 100273;

define constant $GLU-NURBS-ERROR24 = 100274;

define constant $GLU-NURBS-ERROR25 = 100275;

define constant $GLU-NURBS-ERROR26 = 100276;

define constant $GLU-NURBS-ERROR27 = 100277;

define constant $GLU-NURBS-ERROR28 = 100278;

define constant $GLU-NURBS-ERROR29 = 100279;

define constant $GLU-NURBS-ERROR30 = 100280;

define constant $GLU-NURBS-ERROR31 = 100281;

define constant $GLU-NURBS-ERROR32 = 100282;

define constant $GLU-NURBS-ERROR33 = 100283;

define constant $GLU-NURBS-ERROR34 = 100284;

define constant $GLU-NURBS-ERROR35 = 100285;

define constant $GLU-NURBS-ERROR36 = 100286;

define constant $GLU-NURBS-ERROR37 = 100287;

define constant $GLU-AUTO-LOAD-MATRIX = 100200;

define constant $GLU-CULLING = 100201;

define constant $GLU-SAMPLING-TOLERANCE = 100203;

define constant $GLU-DISPLAY-MODE = 100204;

define constant $GLU-PARAMETRIC-TOLERANCE = 100202;

define constant $GLU-SAMPLING-METHOD = 100205;

define constant $GLU-U-STEP = 100206;

define constant $GLU-V-STEP = 100207;

define constant $GLU-NURBS-MODE = 100160;

define constant $GLU-NURBS-MODE-EXT = 100160;

define constant $GLU-NURBS-TESSELLATOR = 100161;

define constant $GLU-NURBS-TESSELLATOR-EXT = 100161;

define constant $GLU-NURBS-RENDERER = 100162;

define constant $GLU-NURBS-RENDERER-EXT = 100162;

define constant $GLU-OBJECT-PARAMETRIC-ERROR = 100208;

define constant $GLU-OBJECT-PARAMETRIC-ERROR-EXT = 100208;

define constant $GLU-OBJECT-PATH-LENGTH = 100209;

define constant $GLU-OBJECT-PATH-LENGTH-EXT = 100209;

define constant $GLU-PATH-LENGTH = 100215;

define constant $GLU-PARAMETRIC-ERROR = 100216;

define constant $GLU-DOMAIN-DISTANCE = 100217;

define constant $GLU-MAP1-TRIM-2 = 100210;

define constant $GLU-MAP1-TRIM-3 = 100211;

define constant $GLU-POINT = 100010;

define constant $GLU-LINE = 100011;

define constant $GLU-FILL = 100012;

define constant $GLU-SILHOUETTE = 100013;

define constant $GLU-SMOOTH = 100000;

define constant $GLU-FLAT = 100001;

define constant $GLU-NONE = 100002;

define constant $GLU-OUTSIDE = 100020;

define constant $GLU-INSIDE = 100021;

define constant $GLU-TESS-BEGIN = 100100;

define constant $GLU-BEGIN = 100100;

define constant $GLU-TESS-VERTEX = 100101;

define constant $GLU-VERTEX = 100101;

define constant $GLU-TESS-END = 100102;

define constant $GLU-END = 100102;

define constant $GLU-TESS-ERROR = 100103;

define constant $GLU-TESS-EDGE-FLAG = 100104;

define constant $GLU-EDGE-FLAG = 100104;

define constant $GLU-TESS-COMBINE = 100105;

define constant $GLU-TESS-BEGIN-DATA = 100106;

define constant $GLU-TESS-VERTEX-DATA = 100107;

define constant $GLU-TESS-END-DATA = 100108;

define constant $GLU-TESS-ERROR-DATA = 100109;

define constant $GLU-TESS-EDGE-FLAG-DATA = 100110;

define constant $GLU-TESS-COMBINE-DATA = 100111;

define constant $GLU-CW = 100120;

define constant $GLU-CCW = 100121;

define constant $GLU-INTERIOR = 100122;

define constant $GLU-EXTERIOR = 100123;

define constant $GLU-UNKNOWN = 100124;

define constant $GLU-TESS-WINDING-RULE = 100140;

define constant $GLU-TESS-BOUNDARY-ONLY = 100141;

define constant $GLU-TESS-TOLERANCE = 100142;

define constant $GLU-TESS-ERROR1 = 100151;

define constant $GLU-TESS-ERROR2 = 100152;

define constant $GLU-TESS-ERROR3 = 100153;

define constant $GLU-TESS-ERROR4 = 100154;

define constant $GLU-TESS-ERROR5 = 100155;

define constant $GLU-TESS-ERROR6 = 100156;

define constant $GLU-TESS-ERROR7 = 100157;

define constant $GLU-TESS-ERROR8 = 100158;

define constant $GLU-TESS-MISSING-BEGIN-POLYGON = 100151;

define constant $GLU-TESS-MISSING-BEGIN-CONTOUR = 100152;

define constant $GLU-TESS-MISSING-END-POLYGON = 100153;

define constant $GLU-TESS-MISSING-END-CONTOUR = 100154;

define constant $GLU-TESS-COORD-TOO-LARGE = 100155;

define constant $GLU-TESS-NEED-COMBINE-CALLBACK = 100156;

define constant $GLU-TESS-WINDING-ODD = 100130;

define constant $GLU-TESS-WINDING-NONZERO = 100131;

define constant $GLU-TESS-WINDING-POSITIVE = 100132;

define constant $GLU-TESS-WINDING-NEGATIVE = 100133;

define constant $GLU-TESS-WINDING-ABS-GEQ-TWO = 100134;

