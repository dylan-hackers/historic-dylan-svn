module: ant-visualizer
synopsis: 
author: 
copyright: 

define constant $PI = 3.141592654;
define constant $PI-OVER-3 = $PI / 3.0;

define constant $ROTATION-SPEED = 2;

define variable angle = 0.0;
define variable last-frame-time = 0.0;

define function draw-hex(x, y) => ()
    glBegin($GL-TRIANGLE-FAN);
        glVertex(x                  , y                  );
        glVertex(x                  , y + 1              );
        glVertex(x + sin($PI-OVER-3), y + cos($PI-OVER-3));
        glVertex(x + sin($PI-OVER-3), y - cos($PI-OVER-3));
        glVertex(x                  , y - 1              );
        glVertex(x - sin($PI-OVER-3), y - cos($PI-OVER-3));
        glVertex(x - sin($PI-OVER-3), y + cos($PI-OVER-3));
        glVertex(x                  , y + 1              );
    glEnd();
end;

define variable draw :: <function> =
        callback-method() => ();
    glClear($GL-COLOR-BUFFER-BIT + $GL-DEPTH-BUFFER-BIT);

    glPushMatrix();
    glScale(50.0, 50.0, 1.0);
    draw-hex(2, 2);
    glPopMatrix();
    
    glutSwapBuffers();
end;

define variable reshape :: <function> =
        callback-method(width :: <integer>, height :: <integer> ) => ();                
    glViewport(0, 0, width, height);
    glMatrixMode($GL-PROJECTION);
    glLoadIdentity();
    glOrtho(0.0,
            as(<GLdouble>, width),
            0.0,
            as(<GLdouble>, height),
            -1.0,
            1.0);
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
