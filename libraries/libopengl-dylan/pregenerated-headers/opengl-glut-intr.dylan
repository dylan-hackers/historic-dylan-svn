module: opengl-glut
synopsis: Dylan bindings for GLUT functions
author: Jeff Dubrule <igor@pobox.com>
copyright: (C) Jefferson Dubrule.  See COPYING.LIB for license details.

c-include("/usr/include/GL/glut.h");

define sealed method glutStrokeRoman () => (result :: <machine-pointer>);
  as(<machine-pointer>, c-variable-ref(ptr: "&glutStrokeRoman"));
end method glutStrokeRoman;

define sealed method glutStrokeRoman-setter (value :: <machine-pointer>) => (result :: <machine-pointer>);
  c-variable-ref(ptr: "&glutStrokeRoman") := value;
  value;
end method glutStrokeRoman-setter;

define sealed method glutStrokeMonoRoman () => (result :: <machine-pointer>);
  as(<machine-pointer>, c-variable-ref(ptr: "&glutStrokeMonoRoman"));
end method glutStrokeMonoRoman;

define sealed method glutStrokeMonoRoman-setter (value :: <machine-pointer>) => (result :: <machine-pointer>);
  c-variable-ref(ptr: "&glutStrokeMonoRoman") := value;
  value;
end method glutStrokeMonoRoman-setter;

define sealed method glutBitmap9By15 () => (result :: <machine-pointer>);
  as(<machine-pointer>, c-variable-ref(ptr: "&glutBitmap9By15"));
end method glutBitmap9By15;

define sealed method glutBitmap9By15-setter (value :: <machine-pointer>) => (result :: <machine-pointer>);
  c-variable-ref(ptr: "&glutBitmap9By15") := value;
  value;
end method glutBitmap9By15-setter;

define sealed method glutBitmap8By13 () => (result :: <machine-pointer>);
  as(<machine-pointer>, c-variable-ref(ptr: "&glutBitmap8By13"));
end method glutBitmap8By13;

define sealed method glutBitmap8By13-setter (value :: <machine-pointer>) => (result :: <machine-pointer>);
  c-variable-ref(ptr: "&glutBitmap8By13") := value;
  value;
end method glutBitmap8By13-setter;

define sealed method glutBitmapTimesRoman10 () => (result :: <machine-pointer>);
  as(<machine-pointer>, c-variable-ref(ptr: "&glutBitmapTimesRoman10"));
end method glutBitmapTimesRoman10;

define sealed method glutBitmapTimesRoman10-setter (value :: <machine-pointer>) => (result :: <machine-pointer>);
  c-variable-ref(ptr: "&glutBitmapTimesRoman10") := value;
  value;
end method glutBitmapTimesRoman10-setter;

define sealed method glutBitmapTimesRoman24 () => (result :: <machine-pointer>);
  as(<machine-pointer>, c-variable-ref(ptr: "&glutBitmapTimesRoman24"));
end method glutBitmapTimesRoman24;

define sealed method glutBitmapTimesRoman24-setter (value :: <machine-pointer>) => (result :: <machine-pointer>);
  c-variable-ref(ptr: "&glutBitmapTimesRoman24") := value;
  value;
end method glutBitmapTimesRoman24-setter;

define sealed method glutBitmapHelvetica10 () => (result :: <machine-pointer>);
  as(<machine-pointer>, c-variable-ref(ptr: "&glutBitmapHelvetica10"));
end method glutBitmapHelvetica10;

define sealed method glutBitmapHelvetica10-setter (value :: <machine-pointer>) => (result :: <machine-pointer>);
  c-variable-ref(ptr: "&glutBitmapHelvetica10") := value;
  value;
end method glutBitmapHelvetica10-setter;

define sealed method glutBitmapHelvetica12 () => (result :: <machine-pointer>);
  as(<machine-pointer>, c-variable-ref(ptr: "&glutBitmapHelvetica12"));
end method glutBitmapHelvetica12;

define sealed method glutBitmapHelvetica12-setter (value :: <machine-pointer>) => (result :: <machine-pointer>);
  c-variable-ref(ptr: "&glutBitmapHelvetica12") := value;
  value;
end method glutBitmapHelvetica12-setter;

define sealed method glutBitmapHelvetica18 () => (result :: <machine-pointer>);
  as(<machine-pointer>, c-variable-ref(ptr: "&glutBitmapHelvetica18"));
end method glutBitmapHelvetica18;

define sealed method glutBitmapHelvetica18-setter (value :: <machine-pointer>) => (result :: <machine-pointer>);
  c-variable-ref(ptr: "&glutBitmapHelvetica18") := value;
  value;
end method glutBitmapHelvetica18-setter;

define functional class <int*> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<int*>));

define inline method pointer-value
    (ptr :: <int*>, #key index = 0)
 => (result :: <integer>);
  signed-long-at(ptr, offset: index * 4);
end method pointer-value;

define inline method pointer-value-setter
    (value :: <integer>, ptr :: <int*>, #key index = 0)
 => (result :: <integer>);
  signed-long-at(ptr, offset: index * 4) := value;
  value;
end method pointer-value-setter;

define method content-size (value :: subclass(<int*>)) => (result :: <integer>);
  4;
end method content-size;

define functional class <char**> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<char**>));

define inline method pointer-value
    (ptr :: <char**>, #key index = 0)
 => (result :: <byte-string>);
  import-value(<byte-string>, pointer-at(ptr, offset: index * 4, class: <c-string>));
end method pointer-value;

define inline method pointer-value-setter
    (value :: <byte-string>, ptr :: <char**>, #key index = 0)
 => (result :: <byte-string>);
  pointer-at(ptr, offset: index * 4, class: <c-string>) := export-value(<c-string>, value);
  value;
end method pointer-value-setter;

define method content-size (value :: subclass(<char**>)) => (result :: <integer>);
  4;
end method content-size;

define function glutInit
    (arg1 :: <int*>, arg2 :: <char**>)
 => ();
  call-out("glutInit", void:, ptr:, (arg1).raw-value, ptr:, (arg2).raw-value);
  values();
end function glutInit;

define function glutInitWindowPosition
    (arg1 :: <integer>, arg2 :: <integer>)
 => ();
  call-out("glutInitWindowPosition", void:, int:, arg1, int:, arg2);
  values();
end function glutInitWindowPosition;

define function glutInitWindowSize
    (arg1 :: <integer>, arg2 :: <integer>)
 => ();
  call-out("glutInitWindowSize", void:, int:, arg1, int:, arg2);
  values();
end function glutInitWindowSize;

define function glutInitDisplayMode
    (arg1 :: <integer>)
 => ();
  call-out("glutInitDisplayMode", void:, unsigned-int:, arg1);
  values();
end function glutInitDisplayMode;

define function glutInitDisplayString
    (arg1 :: <byte-string>)
 => ();
  call-out("glutInitDisplayString", void:, ptr:, (export-value(<c-string>, arg1)).raw-value);
  values();
end function glutInitDisplayString;

define function glutMainLoop
    ()
 => ();
  call-out("glutMainLoop", void:);
  values();
end function glutMainLoop;

define function glutCreateWindow
    (arg1 :: <byte-string>)
 => (result :: <integer>);
  let result-value
    = call-out("glutCreateWindow", int:, ptr:, (export-value(<c-string>, arg1)).raw-value);
  values(result-value);
end function glutCreateWindow;

define function glutCreateSubWindow
    (arg1 :: <integer>, arg2 :: <integer>, arg3 :: <integer>, arg4 :: <integer>, arg5 :: <integer>)
 => (result :: <integer>);
  let result-value
    = call-out("glutCreateSubWindow", int:, int:, arg1, int:, arg2, int:, arg3, int:, arg4, int:, arg5);
  values(result-value);
end function glutCreateSubWindow;

define function glutDestroyWindow
    (arg1 :: <integer>)
 => ();
  call-out("glutDestroyWindow", void:, int:, arg1);
  values();
end function glutDestroyWindow;

define function glutSetWindow
    (arg1 :: <integer>)
 => ();
  call-out("glutSetWindow", void:, int:, arg1);
  values();
end function glutSetWindow;

define function glutGetWindow
    ()
 => (result :: <integer>);
  let result-value
    = call-out("glutGetWindow", int:);
  values(result-value);
end function glutGetWindow;

define function glutSetWindowTitle
    (arg1 :: <byte-string>)
 => ();
  call-out("glutSetWindowTitle", void:, ptr:, (export-value(<c-string>, arg1)).raw-value);
  values();
end function glutSetWindowTitle;

define function glutSetIconTitle
    (arg1 :: <byte-string>)
 => ();
  call-out("glutSetIconTitle", void:, ptr:, (export-value(<c-string>, arg1)).raw-value);
  values();
end function glutSetIconTitle;

define function glutReshapeWindow
    (arg1 :: <integer>, arg2 :: <integer>)
 => ();
  call-out("glutReshapeWindow", void:, int:, arg1, int:, arg2);
  values();
end function glutReshapeWindow;

define function glutPositionWindow
    (arg1 :: <integer>, arg2 :: <integer>)
 => ();
  call-out("glutPositionWindow", void:, int:, arg1, int:, arg2);
  values();
end function glutPositionWindow;

define function glutShowWindow
    ()
 => ();
  call-out("glutShowWindow", void:);
  values();
end function glutShowWindow;

define function glutHideWindow
    ()
 => ();
  call-out("glutHideWindow", void:);
  values();
end function glutHideWindow;

define function glutIconifyWindow
    ()
 => ();
  call-out("glutIconifyWindow", void:);
  values();
end function glutIconifyWindow;

define function glutPushWindow
    ()
 => ();
  call-out("glutPushWindow", void:);
  values();
end function glutPushWindow;

define function glutPopWindow
    ()
 => ();
  call-out("glutPopWindow", void:);
  values();
end function glutPopWindow;

define function glutFullScreen
    ()
 => ();
  call-out("glutFullScreen", void:);
  values();
end function glutFullScreen;

define function glutPostWindowRedisplay
    (arg1 :: <integer>)
 => ();
  call-out("glutPostWindowRedisplay", void:, int:, arg1);
  values();
end function glutPostWindowRedisplay;

define function glutPostRedisplay
    ()
 => ();
  call-out("glutPostRedisplay", void:);
  values();
end function glutPostRedisplay;

define function glutSwapBuffers
    ()
 => ();
  call-out("glutSwapBuffers", void:);
  values();
end function glutSwapBuffers;

define function glutWarpPointer
    (arg1 :: <integer>, arg2 :: <integer>)
 => ();
  call-out("glutWarpPointer", void:, int:, arg1, int:, arg2);
  values();
end function glutWarpPointer;

define function glutSetCursor
    (arg1 :: <integer>)
 => ();
  call-out("glutSetCursor", void:, int:, arg1);
  values();
end function glutSetCursor;

define function glutEstablishOverlay
    ()
 => ();
  call-out("glutEstablishOverlay", void:);
  values();
end function glutEstablishOverlay;

define function glutRemoveOverlay
    ()
 => ();
  call-out("glutRemoveOverlay", void:);
  values();
end function glutRemoveOverlay;

define function glutUseLayer
    (arg1 :: <GLenum>)
 => ();
  call-out("glutUseLayer", void:, unsigned-int:, arg1);
  values();
end function glutUseLayer;

define function glutPostOverlayRedisplay
    ()
 => ();
  call-out("glutPostOverlayRedisplay", void:);
  values();
end function glutPostOverlayRedisplay;

define function glutPostWindowOverlayRedisplay
    (arg1 :: <integer>)
 => ();
  call-out("glutPostWindowOverlayRedisplay", void:, int:, arg1);
  values();
end function glutPostWindowOverlayRedisplay;

define function glutShowOverlay
    ()
 => ();
  call-out("glutShowOverlay", void:);
  values();
end function glutShowOverlay;

define function glutHideOverlay
    ()
 => ();
  call-out("glutHideOverlay", void:);
  values();
end function glutHideOverlay;

define functional class <anonymous-1246> (<function-pointer>) end;

define function glutCreateMenu
    (arg1 :: <function>)
 => (result :: <integer>);
  let result-value
    = call-out("glutCreateMenu", int:, ptr:, (export-value(<function-pointer>, arg1)).raw-value);
  values(result-value);
end function glutCreateMenu;

define function glutDestroyMenu
    (arg1 :: <integer>)
 => ();
  call-out("glutDestroyMenu", void:, int:, arg1);
  values();
end function glutDestroyMenu;

define function glutGetMenu
    ()
 => (result :: <integer>);
  let result-value
    = call-out("glutGetMenu", int:);
  values(result-value);
end function glutGetMenu;

define function glutSetMenu
    (arg1 :: <integer>)
 => ();
  call-out("glutSetMenu", void:, int:, arg1);
  values();
end function glutSetMenu;

define function glutAddMenuEntry
    (arg1 :: <byte-string>, arg2 :: <integer>)
 => ();
  call-out("glutAddMenuEntry", void:, ptr:, (export-value(<c-string>, arg1)).raw-value, int:, arg2);
  values();
end function glutAddMenuEntry;

define function glutAddSubMenu
    (arg1 :: <byte-string>, arg2 :: <integer>)
 => ();
  call-out("glutAddSubMenu", void:, ptr:, (export-value(<c-string>, arg1)).raw-value, int:, arg2);
  values();
end function glutAddSubMenu;

define function glutChangeToMenuEntry
    (arg1 :: <integer>, arg2 :: <byte-string>, arg3 :: <integer>)
 => ();
  call-out("glutChangeToMenuEntry", void:, int:, arg1, ptr:, (export-value(<c-string>, arg2)).raw-value, int:, arg3);
  values();
end function glutChangeToMenuEntry;

define function glutChangeToSubMenu
    (arg1 :: <integer>, arg2 :: <byte-string>, arg3 :: <integer>)
 => ();
  call-out("glutChangeToSubMenu", void:, int:, arg1, ptr:, (export-value(<c-string>, arg2)).raw-value, int:, arg3);
  values();
end function glutChangeToSubMenu;

define function glutRemoveMenuItem
    (arg1 :: <integer>)
 => ();
  call-out("glutRemoveMenuItem", void:, int:, arg1);
  values();
end function glutRemoveMenuItem;

define function glutAttachMenu
    (arg1 :: <integer>)
 => ();
  call-out("glutAttachMenu", void:, int:, arg1);
  values();
end function glutAttachMenu;

define function glutDetachMenu
    (arg1 :: <integer>)
 => ();
  call-out("glutDetachMenu", void:, int:, arg1);
  values();
end function glutDetachMenu;

define functional class <anonymous-1258> (<function-pointer>) end;

define function glutTimerFunc
    (arg1 :: <integer>, arg2 :: <function>, arg3 :: <integer>)
 => ();
  call-out("glutTimerFunc", void:, unsigned-int:, arg1, ptr:, (export-value(<function-pointer>, arg2)).raw-value, int:, arg3);
  values();
end function glutTimerFunc;

define functional class <anonymous-1260> (<function-pointer>) end;

define function glutIdleFunc
    (arg1 :: <function>)
 => ();
  call-out("glutIdleFunc", void:, ptr:, (export-value(<function-pointer>, arg1)).raw-value);
  values();
end function glutIdleFunc;

define functional class <anonymous-1262> (<function-pointer>) end;

define function glutKeyboardFunc
    (arg1 :: <function>)
 => ();
  call-out("glutKeyboardFunc", void:, ptr:, (export-value(<function-pointer>, arg1)).raw-value);
  values();
end function glutKeyboardFunc;

define functional class <anonymous-1264> (<function-pointer>) end;

define function glutSpecialFunc
    (arg1 :: <function>)
 => ();
  call-out("glutSpecialFunc", void:, ptr:, (export-value(<function-pointer>, arg1)).raw-value);
  values();
end function glutSpecialFunc;

define functional class <anonymous-1266> (<function-pointer>) end;

define function glutReshapeFunc
    (arg1 :: <function>)
 => ();
  call-out("glutReshapeFunc", void:, ptr:, (export-value(<function-pointer>, arg1)).raw-value);
  values();
end function glutReshapeFunc;

define functional class <anonymous-1268> (<function-pointer>) end;

define function glutVisibilityFunc
    (arg1 :: <function>)
 => ();
  call-out("glutVisibilityFunc", void:, ptr:, (export-value(<function-pointer>, arg1)).raw-value);
  values();
end function glutVisibilityFunc;

define functional class <anonymous-1270> (<function-pointer>) end;

define function glutDisplayFunc
    (arg1 :: <function>)
 => ();
  call-out("glutDisplayFunc", void:, ptr:, (export-value(<function-pointer>, arg1)).raw-value);
  values();
end function glutDisplayFunc;

define functional class <anonymous-1272> (<function-pointer>) end;

define function glutMouseFunc
    (arg1 :: <function>)
 => ();
  call-out("glutMouseFunc", void:, ptr:, (export-value(<function-pointer>, arg1)).raw-value);
  values();
end function glutMouseFunc;

define functional class <anonymous-1274> (<function-pointer>) end;

define function glutMotionFunc
    (arg1 :: <function>)
 => ();
  call-out("glutMotionFunc", void:, ptr:, (export-value(<function-pointer>, arg1)).raw-value);
  values();
end function glutMotionFunc;

define functional class <anonymous-1276> (<function-pointer>) end;

define function glutPassiveMotionFunc
    (arg1 :: <function>)
 => ();
  call-out("glutPassiveMotionFunc", void:, ptr:, (export-value(<function-pointer>, arg1)).raw-value);
  values();
end function glutPassiveMotionFunc;

define functional class <anonymous-1278> (<function-pointer>) end;

define function glutEntryFunc
    (arg1 :: <function>)
 => ();
  call-out("glutEntryFunc", void:, ptr:, (export-value(<function-pointer>, arg1)).raw-value);
  values();
end function glutEntryFunc;

define functional class <anonymous-1280> (<function-pointer>) end;

define function glutKeyboardUpFunc
    (arg1 :: <function>)
 => ();
  call-out("glutKeyboardUpFunc", void:, ptr:, (export-value(<function-pointer>, arg1)).raw-value);
  values();
end function glutKeyboardUpFunc;

define functional class <anonymous-1282> (<function-pointer>) end;

define function glutSpecialUpFunc
    (arg1 :: <function>)
 => ();
  call-out("glutSpecialUpFunc", void:, ptr:, (export-value(<function-pointer>, arg1)).raw-value);
  values();
end function glutSpecialUpFunc;

define functional class <anonymous-1284> (<function-pointer>) end;

define function glutJoystickFunc
    (arg1 :: <function>, arg2 :: <integer>)
 => ();
  call-out("glutJoystickFunc", void:, ptr:, (export-value(<function-pointer>, arg1)).raw-value, int:, arg2);
  values();
end function glutJoystickFunc;

define functional class <anonymous-1286> (<function-pointer>) end;

define function glutMenuStateFunc
    (arg1 :: <function>)
 => ();
  call-out("glutMenuStateFunc", void:, ptr:, (export-value(<function-pointer>, arg1)).raw-value);
  values();
end function glutMenuStateFunc;

define functional class <anonymous-1288> (<function-pointer>) end;

define function glutMenuStatusFunc
    (arg1 :: <function>)
 => ();
  call-out("glutMenuStatusFunc", void:, ptr:, (export-value(<function-pointer>, arg1)).raw-value);
  values();
end function glutMenuStatusFunc;

define functional class <anonymous-1290> (<function-pointer>) end;

define function glutOverlayDisplayFunc
    (arg1 :: <function>)
 => ();
  call-out("glutOverlayDisplayFunc", void:, ptr:, (export-value(<function-pointer>, arg1)).raw-value);
  values();
end function glutOverlayDisplayFunc;

define functional class <anonymous-1292> (<function-pointer>) end;

define function glutWindowStatusFunc
    (arg1 :: <function>)
 => ();
  call-out("glutWindowStatusFunc", void:, ptr:, (export-value(<function-pointer>, arg1)).raw-value);
  values();
end function glutWindowStatusFunc;

define functional class <anonymous-1294> (<function-pointer>) end;

define function glutSpaceballMotionFunc
    (arg1 :: <function>)
 => ();
  call-out("glutSpaceballMotionFunc", void:, ptr:, (export-value(<function-pointer>, arg1)).raw-value);
  values();
end function glutSpaceballMotionFunc;

define functional class <anonymous-1296> (<function-pointer>) end;

define function glutSpaceballRotateFunc
    (arg1 :: <function>)
 => ();
  call-out("glutSpaceballRotateFunc", void:, ptr:, (export-value(<function-pointer>, arg1)).raw-value);
  values();
end function glutSpaceballRotateFunc;

define functional class <anonymous-1298> (<function-pointer>) end;

define function glutSpaceballButtonFunc
    (arg1 :: <function>)
 => ();
  call-out("glutSpaceballButtonFunc", void:, ptr:, (export-value(<function-pointer>, arg1)).raw-value);
  values();
end function glutSpaceballButtonFunc;

define functional class <anonymous-1300> (<function-pointer>) end;

define function glutButtonBoxFunc
    (arg1 :: <function>)
 => ();
  call-out("glutButtonBoxFunc", void:, ptr:, (export-value(<function-pointer>, arg1)).raw-value);
  values();
end function glutButtonBoxFunc;

define functional class <anonymous-1302> (<function-pointer>) end;

define function glutDialsFunc
    (arg1 :: <function>)
 => ();
  call-out("glutDialsFunc", void:, ptr:, (export-value(<function-pointer>, arg1)).raw-value);
  values();
end function glutDialsFunc;

define functional class <anonymous-1304> (<function-pointer>) end;

define function glutTabletMotionFunc
    (arg1 :: <function>)
 => ();
  call-out("glutTabletMotionFunc", void:, ptr:, (export-value(<function-pointer>, arg1)).raw-value);
  values();
end function glutTabletMotionFunc;

define functional class <anonymous-1306> (<function-pointer>) end;

define function glutTabletButtonFunc
    (arg1 :: <function>)
 => ();
  call-out("glutTabletButtonFunc", void:, ptr:, (export-value(<function-pointer>, arg1)).raw-value);
  values();
end function glutTabletButtonFunc;

define function glutGet
    (arg1 :: <GLenum>)
 => (result :: <integer>);
  let result-value
    = call-out("glutGet", int:, unsigned-int:, arg1);
  values(result-value);
end function glutGet;

define function glutDeviceGet
    (arg1 :: <GLenum>)
 => (result :: <integer>);
  let result-value
    = call-out("glutDeviceGet", int:, unsigned-int:, arg1);
  values(result-value);
end function glutDeviceGet;

define function glutGetModifiers
    ()
 => (result :: <integer>);
  let result-value
    = call-out("glutGetModifiers", int:);
  values(result-value);
end function glutGetModifiers;

define function glutLayerGet
    (arg1 :: <GLenum>)
 => (result :: <integer>);
  let result-value
    = call-out("glutLayerGet", int:, unsigned-int:, arg1);
  values(result-value);
end function glutLayerGet;

define function glutBitmapCharacter
    (arg1 :: <machine-pointer>, arg2 :: <integer>)
 => ();
  call-out("glutBitmapCharacter", void:, ptr:, (arg1).raw-value, int:, arg2);
  values();
end function glutBitmapCharacter;

define function glutBitmapWidth
    (arg1 :: <machine-pointer>, arg2 :: <integer>)
 => (result :: <integer>);
  let result-value
    = call-out("glutBitmapWidth", int:, ptr:, (arg1).raw-value, int:, arg2);
  values(result-value);
end function glutBitmapWidth;

define function glutStrokeCharacter
    (arg1 :: <machine-pointer>, arg2 :: <integer>)
 => ();
  call-out("glutStrokeCharacter", void:, ptr:, (arg1).raw-value, int:, arg2);
  values();
end function glutStrokeCharacter;

define function glutStrokeWidth
    (arg1 :: <machine-pointer>, arg2 :: <integer>)
 => (result :: <integer>);
  let result-value
    = call-out("glutStrokeWidth", int:, ptr:, (arg1).raw-value, int:, arg2);
  values(result-value);
end function glutStrokeWidth;

define functional class <unsigned-char*> (<statically-typed-pointer>) end;

define sealed domain make (singleton(<unsigned-char*>));

define inline method pointer-value
    (ptr :: <unsigned-char*>, #key index = 0)
 => (result :: <integer>);
  unsigned-byte-at(ptr, offset: index * 1);
end method pointer-value;

define inline method pointer-value-setter
    (value :: <integer>, ptr :: <unsigned-char*>, #key index = 0)
 => (result :: <integer>);
  unsigned-byte-at(ptr, offset: index * 1) := value;
  value;
end method pointer-value-setter;

define method content-size (value :: subclass(<unsigned-char*>)) => (result :: <integer>);
  1;
end method content-size;

define function glutBitmapLength
    (arg1 :: <machine-pointer>, arg2 :: <unsigned-char*>)
 => (result :: <integer>);
  let result-value
    = call-out("glutBitmapLength", int:, ptr:, (arg1).raw-value, ptr:, (arg2).raw-value);
  values(result-value);
end function glutBitmapLength;

define function glutStrokeLength
    (arg1 :: <machine-pointer>, arg2 :: <unsigned-char*>)
 => (result :: <integer>);
  let result-value
    = call-out("glutStrokeLength", int:, ptr:, (arg1).raw-value, ptr:, (arg2).raw-value);
  values(result-value);
end function glutStrokeLength;

define function glutWireCube
    (arg1 :: <GLdouble>)
 => ();
  call-out("glutWireCube", void:, double:, arg1);
  values();
end function glutWireCube;

define function glutSolidCube
    (arg1 :: <GLdouble>)
 => ();
  call-out("glutSolidCube", void:, double:, arg1);
  values();
end function glutSolidCube;

define function glutWireSphere
    (arg1 :: <GLdouble>, arg2 :: <GLint>, arg3 :: <GLint>)
 => ();
  call-out("glutWireSphere", void:, double:, arg1, int:, arg2, int:, arg3);
  values();
end function glutWireSphere;

define function glutSolidSphere
    (arg1 :: <GLdouble>, arg2 :: <GLint>, arg3 :: <GLint>)
 => ();
  call-out("glutSolidSphere", void:, double:, arg1, int:, arg2, int:, arg3);
  values();
end function glutSolidSphere;

define function glutWireCone
    (arg1 :: <GLdouble>, arg2 :: <GLdouble>, arg3 :: <GLint>, arg4 :: <GLint>)
 => ();
  call-out("glutWireCone", void:, double:, arg1, double:, arg2, int:, arg3, int:, arg4);
  values();
end function glutWireCone;

define function glutSolidCone
    (arg1 :: <GLdouble>, arg2 :: <GLdouble>, arg3 :: <GLint>, arg4 :: <GLint>)
 => ();
  call-out("glutSolidCone", void:, double:, arg1, double:, arg2, int:, arg3, int:, arg4);
  values();
end function glutSolidCone;

define function glutWireTorus
    (arg1 :: <GLdouble>, arg2 :: <GLdouble>, arg3 :: <GLint>, arg4 :: <GLint>)
 => ();
  call-out("glutWireTorus", void:, double:, arg1, double:, arg2, int:, arg3, int:, arg4);
  values();
end function glutWireTorus;

define function glutSolidTorus
    (arg1 :: <GLdouble>, arg2 :: <GLdouble>, arg3 :: <GLint>, arg4 :: <GLint>)
 => ();
  call-out("glutSolidTorus", void:, double:, arg1, double:, arg2, int:, arg3, int:, arg4);
  values();
end function glutSolidTorus;

define function glutWireDodecahedron
    ()
 => ();
  call-out("glutWireDodecahedron", void:);
  values();
end function glutWireDodecahedron;

define function glutSolidDodecahedron
    ()
 => ();
  call-out("glutSolidDodecahedron", void:);
  values();
end function glutSolidDodecahedron;

define function glutWireOctahedron
    ()
 => ();
  call-out("glutWireOctahedron", void:);
  values();
end function glutWireOctahedron;

define function glutSolidOctahedron
    ()
 => ();
  call-out("glutSolidOctahedron", void:);
  values();
end function glutSolidOctahedron;

define function glutWireTetrahedron
    ()
 => ();
  call-out("glutWireTetrahedron", void:);
  values();
end function glutWireTetrahedron;

define function glutSolidTetrahedron
    ()
 => ();
  call-out("glutSolidTetrahedron", void:);
  values();
end function glutSolidTetrahedron;

define function glutWireIcosahedron
    ()
 => ();
  call-out("glutWireIcosahedron", void:);
  values();
end function glutWireIcosahedron;

define function glutSolidIcosahedron
    ()
 => ();
  call-out("glutSolidIcosahedron", void:);
  values();
end function glutSolidIcosahedron;

define function glutWireTeapot
    (arg1 :: <GLdouble>)
 => ();
  call-out("glutWireTeapot", void:, double:, arg1);
  values();
end function glutWireTeapot;

define function glutSolidTeapot
    (arg1 :: <GLdouble>)
 => ();
  call-out("glutSolidTeapot", void:, double:, arg1);
  values();
end function glutSolidTeapot;

define function glutGameModeString
    (arg1 :: <byte-string>)
 => ();
  call-out("glutGameModeString", void:, ptr:, (export-value(<c-string>, arg1)).raw-value);
  values();
end function glutGameModeString;

define function glutEnterGameMode
    ()
 => (result :: <integer>);
  let result-value
    = call-out("glutEnterGameMode", int:);
  values(result-value);
end function glutEnterGameMode;

define function glutLeaveGameMode
    ()
 => ();
  call-out("glutLeaveGameMode", void:);
  values();
end function glutLeaveGameMode;

define function glutGameModeGet
    (arg1 :: <GLenum>)
 => (result :: <integer>);
  let result-value
    = call-out("glutGameModeGet", int:, unsigned-int:, arg1);
  values(result-value);
end function glutGameModeGet;

define function glutVideoResizeGet
    (arg1 :: <GLenum>)
 => (result :: <integer>);
  let result-value
    = call-out("glutVideoResizeGet", int:, unsigned-int:, arg1);
  values(result-value);
end function glutVideoResizeGet;

define function glutSetupVideoResizing
    ()
 => ();
  call-out("glutSetupVideoResizing", void:);
  values();
end function glutSetupVideoResizing;

define function glutStopVideoResizing
    ()
 => ();
  call-out("glutStopVideoResizing", void:);
  values();
end function glutStopVideoResizing;

define function glutVideoResize
    (arg1 :: <integer>, arg2 :: <integer>, arg3 :: <integer>, arg4 :: <integer>)
 => ();
  call-out("glutVideoResize", void:, int:, arg1, int:, arg2, int:, arg3, int:, arg4);
  values();
end function glutVideoResize;

define function glutVideoPan
    (arg1 :: <integer>, arg2 :: <integer>, arg3 :: <integer>, arg4 :: <integer>)
 => ();
  call-out("glutVideoPan", void:, int:, arg1, int:, arg2, int:, arg3, int:, arg4);
  values();
end function glutVideoPan;

define function glutSetColor
    (arg1 :: <integer>, arg2 :: <GLfloat>, arg3 :: <GLfloat>, arg4 :: <GLfloat>)
 => ();
  call-out("glutSetColor", void:, int:, arg1, float:, arg2, float:, arg3, float:, arg4);
  values();
end function glutSetColor;

define function glutGetColor
    (arg1 :: <integer>, arg2 :: <integer>)
 => (result :: <GLfloat>);
  let result-value
    = call-out("glutGetColor", float:, int:, arg1, int:, arg2);
  values(result-value);
end function glutGetColor;

define function glutCopyColormap
    (arg1 :: <integer>)
 => ();
  call-out("glutCopyColormap", void:, int:, arg1);
  values();
end function glutCopyColormap;

define function glutIgnoreKeyRepeat
    (arg1 :: <integer>)
 => ();
  call-out("glutIgnoreKeyRepeat", void:, int:, arg1);
  values();
end function glutIgnoreKeyRepeat;

define function glutSetKeyRepeat
    (arg1 :: <integer>)
 => ();
  call-out("glutSetKeyRepeat", void:, int:, arg1);
  values();
end function glutSetKeyRepeat;

define function glutForceJoystickFunc
    ()
 => ();
  call-out("glutForceJoystickFunc", void:);
  values();
end function glutForceJoystickFunc;

define function glutExtensionSupported
    (arg1 :: <byte-string>)
 => (result :: <integer>);
  let result-value
    = call-out("glutExtensionSupported", int:, ptr:, (export-value(<c-string>, arg1)).raw-value);
  values(result-value);
end function glutExtensionSupported;

define function glutReportErrors
    ()
 => ();
  call-out("glutReportErrors", void:);
  values();
end function glutReportErrors;

define constant $FREEGLUT = 1;

define constant $GLUT-API-VERSION = 4;

define constant $FREEGLUT-VERSION-2-0 = 1;

define constant $GLUT-XLIB-IMPLEMENTATION = 13;

define constant $GLUT-KEY-F1 = 1;

define constant $GLUT-KEY-F2 = 2;

define constant $GLUT-KEY-F3 = 3;

define constant $GLUT-KEY-F4 = 4;

define constant $GLUT-KEY-F5 = 5;

define constant $GLUT-KEY-F6 = 6;

define constant $GLUT-KEY-F7 = 7;

define constant $GLUT-KEY-F8 = 8;

define constant $GLUT-KEY-F9 = 9;

define constant $GLUT-KEY-F10 = 10;

define constant $GLUT-KEY-F11 = 11;

define constant $GLUT-KEY-F12 = 12;

define constant $GLUT-KEY-LEFT = 100;

define constant $GLUT-KEY-UP = 101;

define constant $GLUT-KEY-RIGHT = 102;

define constant $GLUT-KEY-DOWN = 103;

define constant $GLUT-KEY-PAGE-UP = 104;

define constant $GLUT-KEY-PAGE-DOWN = 105;

define constant $GLUT-KEY-HOME = 106;

define constant $GLUT-KEY-END = 107;

define constant $GLUT-KEY-INSERT = 108;

define constant $GLUT-LEFT-BUTTON = 0;

define constant $GLUT-MIDDLE-BUTTON = 1;

define constant $GLUT-RIGHT-BUTTON = 2;

define constant $GLUT-DOWN = 0;

define constant $GLUT-UP = 1;

define constant $GLUT-LEFT = 0;

define constant $GLUT-ENTERED = 1;

define constant $GLUT-RGB = 0;

define constant $GLUT-RGBA = 0;

define constant $GLUT-INDEX = 1;

define constant $GLUT-SINGLE = 0;

define constant $GLUT-DOUBLE = 2;

define constant $GLUT-ACCUM = 4;

define constant $GLUT-ALPHA = 8;

define constant $GLUT-DEPTH = 16;

define constant $GLUT-STENCIL = 32;

define constant $GLUT-MULTISAMPLE = 128;

define constant $GLUT-STEREO = 256;

define constant $GLUT-LUMINANCE = 512;

define constant $GLUT-MENU-NOT-IN-USE = 0;

define constant $GLUT-MENU-IN-USE = 1;

define constant $GLUT-NOT-VISIBLE = 0;

define constant $GLUT-VISIBLE = 1;

define constant $GLUT-HIDDEN = 0;

define constant $GLUT-FULLY-RETAINED = 1;

define constant $GLUT-PARTIALLY-RETAINED = 2;

define constant $GLUT-FULLY-COVERED = 3;

define constant $GLUT-WINDOW-X = 100;

define constant $GLUT-WINDOW-Y = 101;

define constant $GLUT-WINDOW-WIDTH = 102;

define constant $GLUT-WINDOW-HEIGHT = 103;

define constant $GLUT-WINDOW-BUFFER-SIZE = 104;

define constant $GLUT-WINDOW-STENCIL-SIZE = 105;

define constant $GLUT-WINDOW-DEPTH-SIZE = 106;

define constant $GLUT-WINDOW-RED-SIZE = 107;

define constant $GLUT-WINDOW-GREEN-SIZE = 108;

define constant $GLUT-WINDOW-BLUE-SIZE = 109;

define constant $GLUT-WINDOW-ALPHA-SIZE = 110;

define constant $GLUT-WINDOW-ACCUM-RED-SIZE = 111;

define constant $GLUT-WINDOW-ACCUM-GREEN-SIZE = 112;

define constant $GLUT-WINDOW-ACCUM-BLUE-SIZE = 113;

define constant $GLUT-WINDOW-ACCUM-ALPHA-SIZE = 114;

define constant $GLUT-WINDOW-DOUBLEBUFFER = 115;

define constant $GLUT-WINDOW-RGBA = 116;

define constant $GLUT-WINDOW-PARENT = 117;

define constant $GLUT-WINDOW-NUM-CHILDREN = 118;

define constant $GLUT-WINDOW-COLORMAP-SIZE = 119;

define constant $GLUT-WINDOW-NUM-SAMPLES = 120;

define constant $GLUT-WINDOW-STEREO = 121;

define constant $GLUT-WINDOW-CURSOR = 122;

define constant $GLUT-SCREEN-WIDTH = 200;

define constant $GLUT-SCREEN-HEIGHT = 201;

define constant $GLUT-SCREEN-WIDTH-MM = 202;

define constant $GLUT-SCREEN-HEIGHT-MM = 203;

define constant $GLUT-MENU-NUM-ITEMS = 300;

define constant $GLUT-DISPLAY-MODE-POSSIBLE = 400;

define constant $GLUT-INIT-WINDOW-X = 500;

define constant $GLUT-INIT-WINDOW-Y = 501;

define constant $GLUT-INIT-WINDOW-WIDTH = 502;

define constant $GLUT-INIT-WINDOW-HEIGHT = 503;

define constant $GLUT-INIT-DISPLAY-MODE = 504;

define constant $GLUT-ELAPSED-TIME = 700;

define constant $GLUT-WINDOW-FORMAT-ID = 123;

define constant $GLUT-INIT-STATE = 124;

define constant $GLUT-HAS-KEYBOARD = 600;

define constant $GLUT-HAS-MOUSE = 601;

define constant $GLUT-HAS-SPACEBALL = 602;

define constant $GLUT-HAS-DIAL-AND-BUTTON-BOX = 603;

define constant $GLUT-HAS-TABLET = 604;

define constant $GLUT-NUM-MOUSE-BUTTONS = 605;

define constant $GLUT-NUM-SPACEBALL-BUTTONS = 606;

define constant $GLUT-NUM-BUTTON-BOX-BUTTONS = 607;

define constant $GLUT-NUM-DIALS = 608;

define constant $GLUT-NUM-TABLET-BUTTONS = 609;

define constant $GLUT-DEVICE-IGNORE-KEY-REPEAT = 610;

define constant $GLUT-DEVICE-KEY-REPEAT = 611;

define constant $GLUT-HAS-JOYSTICK = 612;

define constant $GLUT-OWNS-JOYSTICK = 613;

define constant $GLUT-JOYSTICK-BUTTONS = 614;

define constant $GLUT-JOYSTICK-AXES = 615;

define constant $GLUT-JOYSTICK-POLL-RATE = 616;

define constant $GLUT-OVERLAY-POSSIBLE = 800;

define constant $GLUT-LAYER-IN-USE = 801;

define constant $GLUT-HAS-OVERLAY = 802;

define constant $GLUT-TRANSPARENT-INDEX = 803;

define constant $GLUT-NORMAL-DAMAGED = 804;

define constant $GLUT-OVERLAY-DAMAGED = 805;

define constant $GLUT-VIDEO-RESIZE-POSSIBLE = 900;

define constant $GLUT-VIDEO-RESIZE-IN-USE = 901;

define constant $GLUT-VIDEO-RESIZE-X-DELTA = 902;

define constant $GLUT-VIDEO-RESIZE-Y-DELTA = 903;

define constant $GLUT-VIDEO-RESIZE-WIDTH-DELTA = 904;

define constant $GLUT-VIDEO-RESIZE-HEIGHT-DELTA = 905;

define constant $GLUT-VIDEO-RESIZE-X = 906;

define constant $GLUT-VIDEO-RESIZE-Y = 907;

define constant $GLUT-VIDEO-RESIZE-WIDTH = 908;

define constant $GLUT-VIDEO-RESIZE-HEIGHT = 909;

define constant $GLUT-NORMAL = 0;

define constant $GLUT-OVERLAY = 1;

define constant $GLUT-ACTIVE-SHIFT = 1;

define constant $GLUT-ACTIVE-CTRL = 2;

define constant $GLUT-ACTIVE-ALT = 4;

define constant $GLUT-CURSOR-RIGHT-ARROW = 0;

define constant $GLUT-CURSOR-LEFT-ARROW = 1;

define constant $GLUT-CURSOR-INFO = 2;

define constant $GLUT-CURSOR-DESTROY = 3;

define constant $GLUT-CURSOR-HELP = 4;

define constant $GLUT-CURSOR-CYCLE = 5;

define constant $GLUT-CURSOR-SPRAY = 6;

define constant $GLUT-CURSOR-WAIT = 7;

define constant $GLUT-CURSOR-TEXT = 8;

define constant $GLUT-CURSOR-CROSSHAIR = 9;

define constant $GLUT-CURSOR-UP-DOWN = 10;

define constant $GLUT-CURSOR-LEFT-RIGHT = 11;

define constant $GLUT-CURSOR-TOP-SIDE = 12;

define constant $GLUT-CURSOR-BOTTOM-SIDE = 13;

define constant $GLUT-CURSOR-LEFT-SIDE = 14;

define constant $GLUT-CURSOR-RIGHT-SIDE = 15;

define constant $GLUT-CURSOR-TOP-LEFT-CORNER = 16;

define constant $GLUT-CURSOR-TOP-RIGHT-CORNER = 17;

define constant $GLUT-CURSOR-BOTTOM-RIGHT-CORNER = 18;

define constant $GLUT-CURSOR-BOTTOM-LEFT-CORNER = 19;

define constant $GLUT-CURSOR-INHERIT = 100;

define constant $GLUT-CURSOR-NONE = 101;

define constant $GLUT-CURSOR-FULL-CROSSHAIR = 102;

define constant $GLUT-RED = 0;

define constant $GLUT-GREEN = 1;

define constant $GLUT-BLUE = 2;

define constant $GLUT-KEY-REPEAT-OFF = 0;

define constant $GLUT-KEY-REPEAT-ON = 1;

define constant $GLUT-KEY-REPEAT-DEFAULT = 2;

define constant $GLUT-JOYSTICK-BUTTON-A = 1;

define constant $GLUT-JOYSTICK-BUTTON-B = 2;

define constant $GLUT-JOYSTICK-BUTTON-C = 4;

define constant $GLUT-JOYSTICK-BUTTON-D = 8;

define constant $GLUT-GAME-MODE-ACTIVE = 0;

define constant $GLUT-GAME-MODE-POSSIBLE = 1;

define constant $GLUT-GAME-MODE-WIDTH = 2;

define constant $GLUT-GAME-MODE-HEIGHT = 3;

define constant $GLUT-GAME-MODE-PIXEL-DEPTH = 4;

define constant $GLUT-GAME-MODE-REFRESH-RATE = 5;

define constant $GLUT-GAME-MODE-DISPLAY-CHANGED = 6;

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
