module: dylan-gears
synopsis: Dylan version of the classic OpenGL "Gears" example
author:  Brent Fulgham <bfulgham@debian.org>, based on Brian Paul's 'gears.c'
copyright: (C) Brent Fulgham.  Terms: Public Domain

/*
 * 3-D gear wheels.  This program is in the public domain.
 *
 * Command line options:
 *    -info      print GL implementation information
 *
 *
 */

// Conversion to GLUT by Mark J. Kilgard
define constant $PI = 3.14159265;
define variable *Frames* = 0;
define variable T0 = 0;
define variable *gear1* = glGenLists(1);
define variable *gear2* = glGenLists(1);
define variable *gear3* = glGenLists(1);
define variable *angle* :: <double-float> = 0.0;


/*

  Draw a gear wheel.  You'll probably want to call this function when
  building a display list since we do a lot of trig here.
 
  Input:  inner_radius - radius of hole at center
          outer_radius - radius at center of teeth
          width - width of gear
          teeth - number of teeth
          tooth_depth - depth of tooth

 */

define method gear(inner_radius :: <double-float>, outer_radius :: <double-float>, width :: <double-float>,
  teeth :: <integer>, tooth_depth :: <double-float>) => ()
  format(*standard-output*, "Building gears...\n");
  force-output(*standard-output*);
  glShadeModel($GL-FLAT);

  glNormal(0.0, 0.0, 1.0);

  // draw front face
  let r0 :: <double-float> = inner_radius;
  let r1 :: <double-float> = outer_radius - tooth_depth / 2.0;
  let r2 :: <double-float> = outer_radius + tooth_depth / 2.0;
  let da :: <double-float> = 2.0 * $PI / teeth / 4.0;

  with-glBegin($GL-QUAD-STRIP)
    for (i from 0 below (teeth + 1))
        let angle :: <double-float> = i * 2.0 * $PI / teeth;
        glVertex3(r0 * cos(angle), r0 * sin(angle), width * 0.5);
        glVertex3(r1 * cos(angle), r1 * sin(angle), width * 0.5);
        if (i < teeth)
                glVertex3(r0 * cos(angle), r0 * sin(angle), width * 0.5);
                glVertex3(r1 * cos(angle + 3 * da), r1 * sin(angle + 3 * da), width * 0.5);
        end if;
    end for;
  end;

  //draw front sides of teeth
  with-glBegin($GL-QUADS)
    let da :: <double-float> = 2.0 * $PI / teeth / 4.0;
    for (i from 0 below teeth)
      let angle :: <double-float> = i * 2.0 * $PI / teeth;
      glVertex3(r1 * cos(angle), r1 * sin(angle), width * 0.5);
      glVertex3(r2 * cos(angle + da), r2 * sin(angle + da), width * 0.5);
      glVertex3(r2 * cos(angle + 2 * da), r2 * sin(angle + 2 * da), width * 0.5);
      glVertex3(r1 * cos(angle + 3 * da), r1 * sin(angle + 3 * da), width * 0.5);
    end for;
  end;

  glNormal(0.0, 0.0, -1.0);

  // draw back face
  with-glBegin($GL-QUAD-STRIP)
    for (i from 0 below (teeth + 1))
      let da :: <double-float> = 2.0 * $PI / teeth / 4.0;
      let angle :: <double-float> = i * 2.0 * $PI / teeth;
      glVertex3(r1 * cos(angle), r1 * sin(angle), -width * 0.5);
      glVertex3(r0 * cos(angle), r0 * sin(angle), -width * 0.5);
      if (i < teeth)
        glVertex3(r1 * cos(angle + 3 * da), r1 * sin(angle + 3 * da), -width * 0.5);
        glVertex3(r0 * cos(angle), r0 * sin(angle), -width * 0.5);
      end if;
    end for;
  end;

  // draw back sides of teeth
  with-glBegin($GL-QUADS)
    let da :: <double-float> = 2.0 * $PI / teeth / 4.0;
    for (i from 0 below teeth)
      let angle :: <double-float> = i * 2.0 * $PI / teeth;
      glVertex3(r1 * cos(angle + 3 * da), r1 * sin(angle + 3 * da), -width * 0.5);
      glVertex3(r2 * cos(angle + 2 * da), r2 * sin(angle + 2 * da), -width * 0.5);
      glVertex3(r2 * cos(angle + da), r2 * sin(angle + da), -width * 0.5);
      glVertex3(r1 * cos(angle), r1 * sin(angle), -width * 0.5);
    end for;
  end;

  // draw outward faces of teeth
  with-glBegin($GL-QUAD-STRIP)
    for (i from 0 below teeth)
      let angle :: <double-float> = i * 2.0 * $PI / teeth;
      let da :: <double-float> = 2.0 * $PI / teeth / 4.0;
 
      glVertex3(r1 * cos(angle), r1 * sin(angle), width * 0.5);
      glVertex3(r1 * cos(angle), r1 * sin(angle), -width * 0.5);
      let u :: <double-float> = r2 * cos(angle + da) - r1 * cos(angle);
      let v :: <double-float> = r2 * sin(angle + da) - r1 * sin(angle);
      let len :: <double-float> = sqrt(u * u + v * v);
      let u2 :: <double-float> = u / len;
      let v2 :: <double-float> = v / len;
      glNormal(v2, -u2, 0.0);
      glVertex3(r2 * cos(angle + da), r2 * sin(angle + da), width * 0.5);
      glVertex3(r2 * cos(angle + da), r2 * sin(angle + da), -width * 0.5);
      glNormal(cos(angle), sin(angle), 0.0);
      glVertex3(r2 * cos(angle + 2 * da), r2 * sin(angle + 2 * da), width * 0.5);
      glVertex3(r2 * cos(angle + 2 * da), r2 * sin(angle + 2 * da), -width * 0.5);
      let u3 :: <double-float> = r1 * cos(angle + 3 * da) - r2 * cos(angle + 2 * da);
      let v3 :: <double-float> = r1 * sin(angle + 3 * da) - r2 * sin(angle + 2 * da);
      glNormal(v3, -u3, 0.0);
      glVertex3(r1 * cos(angle + 3 * da), r1 * sin(angle + 3 * da), width * 0.5);
      glVertex3(r1 * cos(angle + 3 * da), r1 * sin(angle + 3 * da), -width * 0.5);
      glNormal(cos(angle), sin(angle), 0.0);
    end for;
  
    glVertex3(r1 * cos(0), r1 * sin(0), width * 0.5);
    glVertex3(r1 * cos(0), r1 * sin(0), -width * 0.5);
  end;

  glShadeModel($GL-SMOOTH);

  // draw inside radius cylinder
  with-glBegin($GL-QUAD-STRIP)
    for (i from 0 below (teeth + 1))
      let angle :: <double-float> = i * 2.0 * $PI / teeth;
      glNormal(-cos(angle), -sin(angle), 0.0);
      glVertex3(r0 * cos(angle), r0 * sin(angle), -width * 0.5);
      glVertex3(r0 * cos(angle), r0 * sin(angle), width * 0.5);
    end for;
  end;
  format(*standard-output*, "Built gears.\n");
  force-output(*standard-output*);
end method gear;

define variable $view-rotx :: <double-float> = 20.0;
define variable $view-roty :: <double-float> = 30.0;
define variable $view-rotz :: <double-float> = 0.0;

define variable draw :: <function> = callback-method() => ();
  glClear($GL-COLOR-BUFFER-BIT + $GL-DEPTH-BUFFER-BIT);

  glPushMatrix();
  glRotate($view-rotx, 1.0, 0.0, 0.0);
  glRotate($view-roty, 0.0, 1.0, 0.0);
  glRotate($view-rotz, 0.0, 0.0, 1.0);

  glPushMatrix();
  glTranslate(-3.0, -2.0, 0.0);
  glRotate(*angle*, 0.0, 0.0, 1.0);
  glCallList(*gear1*);
  glPopMatrix();

  glPushMatrix();
  glTranslate(3.1, -2.0, 0.0);
  glRotate(-2.0 * *angle* - 9.0, 0.0, 0.0, 1.0);
  glCallList(*gear2*);
  glPopMatrix();

  glPushMatrix();
  glTranslate(-3.1, 4.2, 0.0);
  glRotate(-2.0 * *angle* - 25.0, 0.0, 0.0, 1.0);
  glCallList(*gear3*);
  glPopMatrix();

  glPopMatrix();

  glutSwapBuffers();

  *Frames* := *Frames* + 1;
  
  let t :: <integer> = glutGet($GLUT-ELAPSED-TIME);

  if (t - T0 >= 5000)
    let seconds :: <double-float> = (t - T0) / 1000.0;
    let fps :: <double-float> = *Frames* / seconds;
    format(*standard-output*, "%= frames in %= seconds = %= FPS\n", *Frames*, seconds, fps);
    T0 := t;
    *Frames* := 0;
    force-output(*standard-output*);
  end if;
end;


define variable idle :: <function> = callback-method() => ();
  *angle* := *angle* + 2.0;
  glutPostRedisplay();
end;

// change view angle, exit upon ESC 
define variable key :: <function> =
  callback-method( k, x, y) => ();
  select (k)
    'z' => $view-rotz = $view-rotz + 5.0;
    'Z' => $view-rotz = $view-rotz - 5.0;
    as(<character>, 27) => #f; //exit(0);
    otherwise => #f;
  end select;
  glutPostRedisplay();
  format(*standard-output*, "Key.\n");
  force-output(*standard-output*);
end;

// change view angle
define variable special :: <function> =
        callback-method(k :: <integer>, x :: <integer>,
                          y :: <integer> ) => ();
  select (k)
    $GLUT-KEY-UP => $view-rotx := $view-rotx + 5.0;
    $GLUT-KEY-DOWN => $view-rotx := $view-rotx - 5.0;
    $GLUT-KEY-LEFT => $view-roty = $view-roty + 5.0;
    $GLUT-KEY-RIGHT => $view-roty = $view-roty - 5.0;
    otherwise => #f;
  end select;
  glutPostRedisplay();
  format(*standard-output*, "Special.\n");
  force-output(*standard-output*);
end;

// new window size or exposure
define variable reshape :: <function> =
        callback-method(width :: <integer>, height :: <integer> ) => ();
  let h :: <double-float> =
    as(<double-float>, height) / as(<double-float>, width);

  glViewport(0, 0, width, height);
  glMatrixMode($GL-PROJECTION);
  glLoadIdentity();
  glFrustum(-1.0, 1.0, -h, h, 5.0, 60.0);
  glMatrixMode($GL-MODELVIEW);
  glLoadIdentity();
  glTranslate(0.0, 0.0, -40.0);
  format(*standard-output*, "Reshaping.\n");
  force-output(*standard-output*);
end;

define method init (progname, #rest arguments) => ();
  let pos = make(<vector>, size: 4);
  pos[0] = 5.0; pos[1] = 5.0; pos[2] = 10.0; pos[3] = 0.0;
  let red = make(<vector>, size: 4);
  red[0] = 0.8; red[1] = 0.1; red[2] = 0.0; red[3] = 1.0;
  let green = make(<vector>, size: 4);
  green[0] = 0.0; green[1] = 0.8; green[2] = 0.2; green[3] = 1.0;
  let blue = make(<vector>, size: 4);
  blue[0] = 0.2; blue[1] = 0.2; blue[2] = 1.0; blue[3] = 1.0;

  glLight($GL-LIGHT0, $GL-POSITION, 5.0, 5.0, 10.0, 0.0);
  glEnable($GL-CULL-FACE);
  glEnable($GL-LIGHTING);
  glEnable($GL-LIGHT0);
  glEnable($GL-DEPTH-TEST);

  // make the gears
  *gear1* := glGenLists(1);
  with-glNewList(*gear1*, $GL-COMPILE)
    glMaterial($GL-FRONT, $GL-AMBIENT-AND-DIFFUSE, 0.8, 0.1, 0.0, 1.0);
    gear(1.0, 4.0, 1.0, 20, 0.7);
  end;

  *gear2* := glGenLists(1);
  with-glNewList(*gear2*, $GL-COMPILE)
    glMaterial($GL-FRONT, $GL-AMBIENT-AND-DIFFUSE, 0.0, 0.8, 0.2, 1.0);
    gear(0.5, 2.0, 2.0, 10, 0.7);
  end;

  *gear3* := glGenLists(1);
  with-glNewList(*gear3*, $GL-COMPILE)
    glMaterial($GL-FRONT, $GL-AMBIENT-AND-DIFFUSE, 0.2, 0.2, 1.0, 1.0);
    gear(1.3, 2.0, 0.5, 10, 0.7);
  end;

  glEnable($GL-NORMALIZE);

  format(*standard-output*, "GL_RENDERER   = %s\n", glGetString($GL-RENDERER));
  format(*standard-output*, "GL_VERSION    = %s\n", glGetString($GL-VERSION));
  format(*standard-output*, "GL_VENDOR     = %s\n", glGetString($GL-VENDOR));
  format(*standard-output*, "GL_EXTENSIONS = %s\n", glGetString($GL-EXTENSIONS));
  force-output(*standard-output*);
end method init;

define variable visible :: <function> = callback-method( vis :: <integer> ) => ();
  if (vis == $GLUT-VISIBLE)
    glutIdleFunc(idle);
  //else
  //  glutIdleFunc(NULL);
  end if;
end;


define method main(progname, #rest arguments) => ();
  glut-init();
  glutInitDisplayMode($GLUT-RGB + $GLUT-DEPTH + $GLUT-DOUBLE);

  glutInitWindowPosition(0, 0);
  glutInitWindowSize(300, 300);
  glutCreateWindow("Gears");
  init(progname, arguments);

  glutDisplayFunc(draw);
  glutReshapeFunc(reshape);
  glutKeyboardFunc(key);
  glutSpecialFunc(special);
  glutVisibilityFunc(visible);

  glutMainLoop();
end method main;
