module: glu-test
synopsis: 
author: 
copyright: 

define constant $PI = 3.141592654;

define constant $ROTATION-SPEED = 2;

define variable angle = 0.0;
define variable last-frame-time = 0.0;

define variable draw :: <function> = callback-method() => ();
  glClear($GL-COLOR-BUFFER-BIT + $GL-DEPTH-BUFFER-BIT);

  let now = glutGet($GLUT-ELAPSED-TIME);
  let dt = as(<GLdouble>, now - last-frame-time) / 1000.0;
  last-frame-time := now;

  angle := angle + ($ROTATION-SPEED * dt);
  if (angle > (2.0 * $PI))
    angle := angle - (2.0 * $PI);
  end if;

  glLoadIdentity();
  gluLookAt(5.0 * cos(angle), 0.0, 5.0 * sin(angle),
            0.0, 0.0, 0.0,
            0.0, 1.0, 0.0);

  let sphere = gluNewQuadric();
  gluQuadricDrawStyle(sphere, $GLU-LINE);
  gluSphere(sphere, 1.0, 32, 16);
  gluDeleteQuadric(sphere);
  
  glutSwapBuffers();
end;

define variable reshape :: <function> =
        callback-method(width :: <integer>, height :: <integer> ) => ();
  let aspect = as(<GLdouble>, width) / as(<GLdouble>, height);
        
  glViewport(0, 0, width, height);
  glMatrixMode($GL-PROJECTION);
  glLoadIdentity();
  gluPerspective(45.0, aspect, 1.0, 100.0);
  glMatrixMode($GL-MODELVIEW);
end;

define variable idle :: <function> =
        callback-method() => ();
  glutPostRedisplay();
end;

begin
  glut-init();
  glutInitDisplayMode($GLUT-RGB + $GLUT-DEPTH + $GLUT-DOUBLE);

  glutInitWindowPosition(0, 0);
  glutInitWindowSize(300, 300);
  glutCreateWindow("GLU Quadric Test");

  glutDisplayFunc(draw);
  glutReshapeFunc(reshape);
  glutIdleFunc(idle);

  glutMainLoop();
end;
