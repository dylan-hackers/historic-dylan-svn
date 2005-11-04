module:    inertia
synopsis:  Inertia initializations
author:    Mike Austin
copyright: Copyright (C) 2005 Mike L. Austin.  All rights reserved.
license:   MIT/BSD, see LICENCE.txt for details

//
// inertia-main.dylan
//

c-include("/usr/include/w32api/GL/glu.h");

define variable reshape :: <function> = callback-method (width :: <integer>, height :: <integer>) => ();
//define variable reshape = callback-method (width :: <integer>, height :: <integer>) => ();
  glMatrixMode ($GL-PROJECTION);
    glLoadIdentity ();
    gluOrtho2D (0.0, as(<double-float>, width), as(<double-float>, height), 0.0);
  glMatrixMode ($GL-MODELVIEW);
  glViewport (0, 0, width, height);
end;

define variable display :: <function> = callback-method () => ();
  glClear ($GL-COLOR-BUFFER-BIT + $GL-DEPTH-BUFFER-BIT);
  glLoadIdentity ();
  glTranslate (0.375, 0.375, 0.0);

  glColor (0.0, 0.0, 0.0, 1.0);

  draw-shape (*screen*);

  glutSwapBuffers ();
end;

define variable *button* = #"none-button";

define variable mouse-callback :: <function> = callback-method (button :: <integer>,
                                  state :: <integer>, x :: <integer>, y :: <integer>) => ();
  let origin = make (<point>, x: as(<double-float>, x), y: as(<double-float>, y));
  *button* := select (button)
    $GLUT-LEFT-BUTTON => $left-button;
    $GLUT-MIDDLE-BUTTON => $middle-button;
    $GLUT-RIGHT-BUTTON => $right-button;
  end;

  if (state == $GLUT-DOWN)
    send-event (*screen*, make (<mouse-down-event>, origin: origin), *button*);
  elseif (state == $GLUT-UP)
    send-event (*screen*, make (<mouse-up-event>, origin: origin), *button*);
    *button* := #"none-button";
  end;
  glutPostRedisplay ();
end;

define variable passive-motion :: <function> = callback-method (x :: <integer>, y :: <integer>)
 => ();
  let origin = make (<point>, x: as(<double-float>, x), y: as(<double-float>, y));
  send-event (*screen*, make (<mouse-motion-event>, origin: origin), *button*);
end;

define variable motion-callback :: <function> = callback-method (x :: <integer>, y :: <integer>)
 => ();
  let origin = make (<point>, x: as(<double-float>, x), y: as(<double-float>, y));
  send-event (*screen*, make (<mouse-drag-event>, origin: origin), *button*);
  glutPostRedisplay ();
end;

define variable polygon-begin :: <function> = callback-method (mode :: <integer>) => ();
end;

begin
  glut-init ();
  glutInitDisplayMode ($GLUT-RGBA + $GLUT-DEPTH + $GLUT-DOUBLE + $GLUT-STENCIL);

  glutInitWindowPosition (200, 100);
  glutInitWindowSize (1024, 768);
  glutCreateWindow ("Dylan Inertia");

  glutDisplayFunc (display);
  glutReshapeFunc (reshape);
  glutPassiveMotionFunc (passive-motion);
  glutMouseFunc (mouse-callback);
  glutMotionFunc (motion-callback);
  
  glClearColor (1.0s0, 1.0s0, 1.0s0, 1.0s0);
  
  glEnable ($GL-BLEND); glBlendFunc ($GL-SRC-ALPHA, $GL-ONE-MINUS-SRC-ALPHA);
  glEnable ($GL-LINE-SMOOTH); glLineWidth (1.5s0);

  define variable *tess-object* = gluNewTess ();

  call-out ("gluTessCallback", void:, ptr: *tess-object*.raw-value, int: 100100, ptr: c-expr (ptr: "glBegin"));
  call-out ("gluTessCallback", void:, ptr: *tess-object*.raw-value, int: 100101, ptr: c-expr (ptr: "glVertex3dv"));
  call-out ("gluTessCallback", void:, ptr: *tess-object*.raw-value, int: 100102, ptr: c-expr (ptr: "glEnd"));

  define variable *screen* = make (<screen>, width: 1024.0, height: 768.0);
  define variable *menu* = make (<shape-menu>, width: 100.0, height: 180.0);
  add-child (*screen*, *menu*);
end;

