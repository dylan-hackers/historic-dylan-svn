module: vrml-viewer
author: Andreas Bogk <andreas@andreas.org>
copyright: (C) Andreas Bogk, under LGPL

define function s(float :: <float>) => (single :: <single-float>)
  as(<single-float>, float);
end function;

define functional class <point> (<object>)
  slot x :: <integer>, required-init-keyword: x:;
  slot y :: <integer>, required-init-keyword: y:;
end class <point>;

define method print-object(location :: <point>, stream :: <stream>) => ()
  format(stream, "{<point>, x: %=, y: %=}", location.x, location.y);
end method print-object;

define abstract class <glut-event> (<object>)
end class <glut-event>;

define class <mouse-event> (<glut-event>)
  slot button :: <integer>, required-init-keyword: button:;
  slot state :: <integer>, required-init-keyword: state:;
  slot location :: <point>, required-init-keyword: location:;
end class <mouse-event>;

define method print-object(event :: <mouse-event>, stream :: <stream>) => ()
  format(stream, "{<mouse-event>, button: %=, state: %=, location: %=}", event.button, event.state, event.location);
end;

define class <motion-event> (<glut-event>)
  slot location :: <point>, required-init-keyword: location:;
end class <motion-event>;

define method print-object(event :: <motion-event>, stream :: <stream>) => ()
  format(stream, "{<motion-event>, location: %=}", event.location);
end;

define class <reshape-event> (<glut-event>)
  slot location :: <point>, required-init-keyword: location:;
end;

define method print-object(event :: <reshape-event>, stream :: <stream>) => ()
  format(stream, "{<reshape-event>, location: %=}", event.location);
end;

define method post-event(event :: <glut-event>)
  format(*standard-output*, "Got event: %=\n", event);
  force-output(*standard-output*);
end method post-event;

define variable timer-func :: <function> = callback-method(value :: <integer>) => ();
  glMatrixMode($GL-PROJECTION);
  glRotate(s(5.0), s(0.0), s(1.0), s(0.0));
  glMatrixMode($GL-MODELVIEW);
  glutPostRedisplay();
  glutTimerFunc(20, timer-func, 0);
end;

define variable mouse-func :: <function> = callback-method(button :: <integer>, state :: <integer>, x :: <integer>, y :: <integer>) => ();
  post-event(make(<mouse-event>, button: button, state: state, location: make(<point>, x: x, y: y)));
end;

define variable keyboard-func :: <function> = callback-method(character :: <byte>, x :: <integer>, y :: <integer>) => ();
//  post-event(make(<mouse-event>, button: button, state: state, location: make(<point>, x: x, y: y)));
  if(character == 27)
    exit();
  end if;
end;

define variable special-func :: <function> = callback-method(key :: <integer>, x :: <integer>, y :: <integer>) => ();
//  post-event(make(<mouse-event>, button: button, state: state, location: make(<point>, x: x, y: y)));
  select(key)
    $GLUT-KEY-UP => glutFullScreen();
    $GLUT-KEY-DOWN => glutReshapeWindow(500, 500);
  end select;
end;

define variable motion-func :: <function> = callback-method(x :: <integer>, y :: <integer>) => ();
  post-event(make(<motion-event>, location: make(<point>, x: x, y: y)));
end;
  
define variable reshape-func :: <function> = callback-method(x :: <integer>, y :: <integer>) => ();
  glViewport(0, 0, x, y);
/*
  glMatrixMode($GL-PROJECTION);
  glLoadIdentity();
  if(y = 0)
    gluPerspective(80.0d0, as(<double-float>, x), 1.0, 5000.0)
  else
    gluPerspective(80.0d0, as(<double-float>, x) / as(<double-float>, y), 1.0, 5000.0)
  end if;
*/
//  glMatrixMode($GL-MODELVIEW);
//  glLoadIdentity();
  post-event(make(<reshape-event>, location: make(<point>, x: x, y: y)));
end;

define constant *scene-graph* = 
  make(<container-node>, 
       children: vector(make(<line-grid>),
                        make(<transform>, scale: #[0.01, 0.01, 0.01],
                             children: vector(make(<indexed-face-set>,
                                                   points: #[#[-15.0, 0.0, 0.000001],
                                                             #[-4.635254, 0.0, -14.265848],
                                                             #[12.135255, 0.0, -8.816778],
                                                             #[12.135253, 0.0, 8.816781],
                                                             #[-4.635254, 0.0, 14.265848],
                                                             #[-12.0, 65.0, 0.000001],
                                                             #[-3.708204, 65.0, -11.412679],
                                                             #[9.708204, 65.0, -7.053422],
                                                             #[9.708202, 65.0, 7.053425],
                                                             #[-3.708204, 65.0, 11.412679]],
                                                   indices: #[#[0,1,6,5],
                                                              #[1,2,7,6],
                                                              #[2,3,8,7],
                                                              #[3,4,9,8],
                                                              #[4,0,5,9]])))));

/*
define constant *scene-graph* = make(<indexed-face-set>,
                                     points: #[#[-0.5, -0.5, -0.5],
                                               #[ 0.5, -0.5, -0.5],
                                               #[ 0.5,  0.5, -0.5],
                                               #[-0.5,  0.5, -0.5],
                                               #[ 0.5,  0.5,  0.5],
                                               #[ 0.5, -0.5,  0.5],
                                               #[-0.5, -0.5,  0.5],
                                               #[-0.5,  0.5,  0.5]],
                                     indices: #[#[0,1,2,3],
                                                #[4,5,6,7],
                                                #[0,1,5,6],
                                                #[2,3,7,4],
                                                #[0,3,7,6],
                                                #[1,2,4,5]]);
*/
                                                 
define variable display-func :: <function> = callback-method() => ();
  glClear(logior($GL-COLOR-BUFFER-BIT, $GL-DEPTH-BUFFER-BIT));
  glLoadIdentity();
  render-to-opengl(*scene-graph*);
  glutSwapBuffers();
end;

define method main(progname, #rest arguments)
  glutInitDisplayMode(logior($GLUT-RGBA, $GLUT-DEPTH, $GLUT-DOUBLE));
  glutInitWindowSize(500, 500);
  let win :: <integer> = glutCreateWindow("Foo");

  glShadeModel($GL-SMOOTH);
  glEnable($GL-DEPTH-TEST);
  glDepthFunc($GL-LESS);
  glEnable($GL-COLOR-MATERIAL);
  glHint($GL-PERSPECTIVE-CORRECTION-HINT, $GL-NICEST);

  GC-enable-incremental();

  format(*standard-output*, "GL_VENDOR: %s\n", 
	                     glGetString($GL-VENDOR));
  format(*standard-output*, "GL_RENDERER: %s\n", 
	                     glGetString($GL-RENDERER));
  format(*standard-output*, "GL_VERSION: %s\n", 
	                     glGetString($GL-VERSION));
  format(*standard-output*, "GL_EXTENSIONS: %s\n", 
	                     glGetString($GL-EXTENSIONS));
  format(*standard-output*, "Depth test: %=\n", 
	                     glIsEnabled($GL-DEPTH-TEST));
  force-output(*standard-output*);
//  glutFullScreen();

  glMatrixMode($GL-PROJECTION);
  glLoadIdentity();
  glFrustum(-0.25, 0.25, -0.25, 0.25, 0.5, 100.0);
  gluLookAt(0.0,  1.8, -3.0, // eye position
	    0.0,  0.0, 0.0,  // looking at
	    0.0,  1.0, 0.0); // up direction

  glMatrixMode($GL-MODELVIEW);

  glEnable($GL-AUTO-NORMAL);
//  glCullFace($GL-BACK);
//  glEnable($GL-CULL-FACE)
  glEnable($GL-LIGHTING);
  glEnable($GL-LIGHT0);
  glLight($GL-LIGHT0, $GL-POSITION, 3, 3, -2, 1);
  glLight($GL-LIGHT0, $GL-AMBIENT, 0.1, 0.1, 0.2, 1.0);
  glLight($GL-LIGHT0, $GL-DIFFUSE, 0.5, 0.5, 0.9, 1.0);
  glLight($GL-LIGHT0, $GL-SPECULAR, 0.5, 0.5, 1.0, 1.0);
  glLight($GL-LIGHT0, $GL-SPOT-DIRECTION, -3, -3, 2);

  glEnable($GL-FOG);
  glEnable($GL-BLEND);
  glFog($GL-FOG-MODE, $GL-EXP);
  glFog($GL-FOG-COLOR, s(0.2), s(0.2), s(0.2), s(1.0));
  glFog($GL-FOG-DENSITY, s(5.0));
  glFog($GL-FOG-START, s(-3.0));
  glFog($GL-FOG-END, s(4.0));

  glClearColor(s(0.5), s(0.5), s(0.5), s(1.0));
  glClearDepth(1.0d0);

  glutDisplayFunc(display-func);
  glutTimerFunc(20, timer-func, 0);
  glutMouseFunc(mouse-func);
  glutMotionFunc(motion-func);
  glutReshapeFunc(reshape-func);
  glutKeyboardFunc(keyboard-func);
  glutSpecialFunc(special-func);
  glutMainLoop();
  glutDestroyWindow(win);
  exit(exit-code: 0);
end method main;

apply(main, application-name(), application-arguments());