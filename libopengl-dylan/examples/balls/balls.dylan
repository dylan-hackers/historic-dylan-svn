module: balls
synopsis: Dylan version of Mike Austin's OpenGL "Balls" example
author:  Brent Fulgham <bfulgham@debian.org>, based on Mike Austin's 'balls.io'
copyright: (C) Brent Fulgham.  Terms: Public Domain
use-libraries: dylan, opengl
use-modules: dylan, system, opengl, opengl-glut, opengl-glu

define variable *objectStore* = make(<vector>, size: 3);
define variable *angleX* :: <double-float> = 22.0;
define variable *angleY* :: <double-float> = 0.0;

// The Objects to be displayed
define class <gameObject> (<object>)
  slot x :: <double-float>, init-keyword: x:;
  slot y :: <double-float>, init-keyword: y:;
  slot z :: <double-float>, init-keyword: z:;
  slot xvel :: <double-float>, init-value: 0.01;
  slot yvel :: <double-float>, init-value: 0.01;
  slot zvel :: <double-float>, init-value: 0.00;
  slot zangle :: <double-float>, init-value: 0.0;
  slot zavel :: <double-float>, init-value: -1.0;
end class;

define class <Ball> (<gameObject>)
end class;

define class <Cube> (<gameObject>)
end class;

// Rendering methods
define method render ( obj :: <Ball> ) => ();
  glutSolidSphere (0.2, 24, 32 );
end method;

define method render ( obj :: <Cube> ) => ();
  glutSolidCube ( 0.3 );
end method;

define method advance( obj :: <gameObject>) => ();
  obj.zvel := obj.zvel - 0.001;
  obj.x := obj.x + obj.xvel;
  obj.y := obj.y + obj.yvel;
  obj.z := obj.z + obj.zvel;
  obj.zangle := obj.zangle + obj.zavel;

  // Clamp coordinates inside cube:
  if ( obj.x > 1.3 )
    obj.xvel := 0 - obj.xvel;
    obj.x := 1.3;
  elseif ( obj.x < -1.3 )
    obj.xvel := 0 - obj.xvel;
    obj.x := -1.3;
  end if;

  if ( obj.y > 1.3 )
    obj.yvel := 0 - obj.yvel;
    obj.y := 1.3;
  elseif ( obj.y < -1.3 )
    obj.yvel := 0 - obj.yvel;
    obj.y := -1.3;
  end if;

  if ( obj.z < -1 )
    obj.zvel := 0 - obj.zvel;
    obj.z := -1.0
  end if;
end method;

define variable draw :: <function> = callback-method() => ();
  glClear($GL-COLOR-BUFFER-BIT + $GL-DEPTH-BUFFER-BIT);
  glLoadIdentity();
  glTranslate( 0.0, 0.0, -5.0);
  glRotate( *angleX*, 1.0, 0.0, 0.0 );
  glRotate( *angleY*, 0.0, 1.0, 0.0 );

  // Loop
  for (obj in *objectStore*)
    with-glPushMatrix()
      glTranslate( obj.x, obj.y, obj.z );
      glRotate( obj.zangle, 0.0, 1.0, 0.0 );
      render ( obj );
    end;
  end for;

  glDisable($GL-LIGHTING);
  glutWireCube( 3.0 );
  glEnable($GL-LIGHTING);
  glFlush();

  glutSwapBuffers();
end;

define variable *lastX* :: <integer> = 0;
define variable *lastY* :: <integer> = 0;

define variable mouse :: <function> =
        callback-method( button :: <integer>, state :: <integer>,
                         x :: <integer> , y :: <integer>) => ();
  *lastX* := x;
  *lastY* := y;
end;

define variable motion :: <function> =
        callback-method( x :: <integer>, y :: <integer>) => ();
  *angleX* := *angleX* + ( y - *lastY* );
  *angleY* := *angleY* + (x - *lastX* );
  *lastX* := x;
  *lastY* := y;
  glutPostRedisplay();
end;

define variable timer :: <function> =
        callback-method( x :: <integer>) => ();
  for (obj in *objectStore*)
    advance( obj );
  end for;

  glutTimerFunc(x, timer, x);
  glutPostRedisplay();
end;

// new window size or exposure
define variable reshape :: <function> =
        callback-method(width :: <integer>, height :: <integer> ) => ();
  glMatrixMode($GL-PROJECTION);
  glLoadIdentity();
  gluPerspective( 60.0, as(<double-float>, width) / as(<double-float>, height), 0.5, 10.0);
  glMatrixMode($GL-MODELVIEW);
  glViewport( 0, 0, width, height);
end;

define method init (progname, #rest arguments) => ();
  glClearColor(0.0s0, 0.0s0, 0.0s0, 1.0s0);
  glEnable($GL-DEPTH-TEST);
  glEnable($GL-LIGHTING);
  glEnable($GL-LIGHT0);
  glEnable($GL-BLEND);
  glBlendFunc($GL-SRC-ALPHA, $GL-ONE-MINUS-SRC-ALPHA);
  glEnable($GL-LINE-SMOOTH);
  glLineWidth(1.5s0);

  // Set up the object store
  *objectStore*[0] := make(<Ball>, x: 0.0, y: 0.0, z: 0.0);
  *objectStore*[1] := make(<Ball>, x: 0.5, y: -1.0, z: 2.0);
  *objectStore*[2] := make(<Cube>, x: -1.0, y: 0.5, z: 3.0);
end method init;

define method main(progname, #rest arguments) => ();
  glut-init();
  glutInitDisplayMode($GLUT-RGB + $GLUT-DEPTH + $GLUT-DOUBLE);

  glutInitWindowPosition(0, 0);
  glutInitWindowSize(512, 512);
  glutCreateWindow("Dylan Balls3D");
  init(progname, arguments);

  glutDisplayFunc(draw);
  glutReshapeFunc(reshape);
  glutMouseFunc(mouse);
  glutMotionFunc(motion);
  glutTimerFunc(0, timer, 10);

  glutMainLoop();
end method main;

main( "Balls", 1);
