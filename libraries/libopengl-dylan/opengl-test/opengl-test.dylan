library: opengl-test
module: opengl-test
author: Jeff Dubrule <igor@pobox.com>
copyright: (C) Jefferson Dubrule.  See COPYING.LIB for license details.

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
  glRotate(s(5.0), s(1.0), s(1.0), s(1.0));
  glutPostRedisplay();
  glutTimerFunc(20, timer-func, 0);
end;

define variable mouse-func :: <function> = callback-method(button :: <integer>, state :: <integer>, x :: <integer>, y :: <integer>) => ();
  post-event(make(<mouse-event>, button: button, state: state, location: make(<point>, x: x, y: y)));
end;

define variable motion-func :: <function> = callback-method(x :: <integer>, y :: <integer>) => ();
  post-event(make(<motion-event>, location: make(<point>, x: x, y: y)));
end;
  
define variable reshape-func :: <function> = callback-method(x :: <integer>, y :: <integer>) => ();
  glViewport(0, 0, x, y);
  post-event(make(<reshape-event>, location: make(<point>, x: x, y: y)));
end;
  

define variable display-func :: <function> = callback-method() => ();
  glClear($GL-COLOR-BUFFER-BIT + $GL-DEPTH-BUFFER-BIT);
  with-glBegin ($GL-QUADS)

    // Front:
    glColor(0.0, 0.0, 1.0);
    glNormal(0.0, 0.0, -1.0 );
    glVertex(-0.5, -0.5, -0.5);
    glVertex( 0.5, -0.5, -0.5);
    glVertex( 0.5,  0.5, -0.5);
    glVertex(-0.5,  0.5, -0.5);
    
    // Back:
    glColor(0.0, 1.0, 1.0);
    glNormal(0.0, 0.0, 1.0);
    glVertex( 0.5,  0.5,  0.5);
    glVertex( 0.5, -0.5,  0.5);
    glVertex(-0.5, -0.5,  0.5);
    glVertex(-0.5,  0.5,  0.5);
    
    // Top:
    glColor(1.0, 0.0, 0.0);
    glNormal(0.0, 1.0, 0.0);
    glVertex( 0.5,  0.5, -0.5);
    glVertex( 0.5,  0.5,  0.5);
    glVertex(-0.5,  0.5,  0.5);
    glVertex(-0.5,  0.5, -0.5);

    // Bottom:
    glColor(1.0, 0.0, 1.0);
    glNormal(0.0, -1.0, 0.0);
    glVertex(-0.5, -0.5,  0.5);
    glVertex( 0.5, -0.5,  0.5);
    glVertex( 0.5, -0.5, -0.5);
    glVertex(-0.5, -0.5, -0.5);

    // Left
    glColor(1.0, 1.0, 0.0);
    glNormal(-1.0, 0.0, 0.0);
    glVertex(-0.5,  0.5,  0.5);
    glVertex(-0.5, -0.5,  0.5);
    glVertex(-0.5, -0.5, -0.5);
    glVertex(-0.5,  0.5, -0.5);

    // Right
    glColor(0.0, 1.0, 0.0);
    glNormal(1.0, 0.0, 0.0);
    glVertex( 0.5,  0.5, -0.5);
    glVertex( 0.5, -0.5, -0.5);
    glVertex( 0.5, -0.5,  0.5);
    glVertex( 0.5,  0.5,  0.5);

  end;
  glutSwapBuffers();
end;

define method main(progname, #rest arguments)
//  let (argc, argv) = c-arguments(progname, arguments);
//  glutInit(argc, argv);
  // MacOS X window manager gets very upset if we don't call this
  glut-init();

  // This hangs MacOS X 10.2 at present
  //GC-enable-incremental();
  glutInitDisplayMode($GLUT-RGBA + $GLUT-DEPTH + $GLUT-DOUBLE);

  let win :: <integer> = glutCreateWindow("Foo");
  format(*standard-output*, "GL_VENDOR: %s\n", 
	                     glGetString($GL-VENDOR));
  format(*standard-output*, "GL_RENDERER: %s\n", 
	                     glGetString($GL-RENDERER));
  format(*standard-output*, "GL_VERSION: %s\n", 
	                     glGetString($GL-VERSION));
  format(*standard-output*, "GL_EXTENSIONS: %s\n", 
	                     glGetString($GL-EXTENSIONS));
  force-output(*standard-output*);

  glOrtho(-1.0, 1.0, -1.0, 1.0, -1.5, 1.5);
  glMatrixMode($GL-PROJECTION);
  glLoadIdentity();
  glFrustum(-1.0, 1.0, -1.0, 1.0, 8.0, 12.0);
  gluLookAt(0.0, 4.0, 10.0,
	    0.0,  0.0, 0.0,
	    0.0,  1.0, 0.0);
  glMatrixMode($GL-MODELVIEW);

  glCullFace($GL-BACK);
  glEnable($GL-CULL-FACE);

/*  glEnable($GL-LIGHTING);
  glEnable($GL-LIGHT0);
  glLight(0, $GL-POSITION, 3, 3, -2, 1);
  glLight(0, $GL-AMBIENT, 0.1, 0.1, 0.2, 1.0);
  glLight(0, $GL-DIFFUSE, 0.5, 0.5, 0.9, 1.0);
  glLight(0, $GL-SPECULAR, 0.5, 0.5, 1.0, 1.0);
  glLight(0, $GL-SPOT-DIRECTION, -3, -3, 2);

  glEnable($GL-COLOR-MATERIAL);


  glEnable($GL-FOG);
  glEnable($GL-BLEND);
  glFog($GL-FOG-MODE, $GL-EXP);
  glFog($GL-FOG-COLOR, s(0.2), s(0.2), s(0.2), s(1.0));
  glFog($GL-FOG-DENSITY, s(5.0));
  glFog($GL-FOG-START, s(-3.0));
  glFog($GL-FOG-END, s(4.0));
*/
  glClearColor(s(0.5), s(0.5), s(0.5), s(1.0));

  
  glutDisplayFunc(display-func);
  glutTimerFunc(20, timer-func, 0);
  glutMouseFunc(mouse-func);
  glutMotionFunc(motion-func);
  glutReshapeFunc(reshape-func);
  glutMainLoop();
  glutDestroyWindow(win);
  exit(exit-code: 0);
end method main;


/*
define method c-arguments(progname :: <string>, arguments)
 => (<integer>, <c-string-vector>)
  let argc = 1 + arguments.size;
  let argv :: <c-string-vector> =
    make(<c-string-vector>, element-count: argc + 1);
  // XXX - We'd need to delete these if we weren't using
  // a garbage collector which handles the C heap.
  argv[0] := progname;
  for (i from 1 below argc,
       arg in arguments)
    argv[i] := arg;
  end for;
  as(<c-pointer-vector>, argv)[argc] := null-pointer;
  values(argc, argv);
end method c-arguments;
*/
