module: ant-visualizer
synopsis: 
author: 
copyright: 

define functional class <color> (<object>)
    constant slot r :: <float>, required-init-keyword: red:;
    constant slot g :: <float>, required-init-keyword: green:;
    constant slot b :: <float>, required-init-keyword: blue:;
    constant slot a :: <float> = 1.0, init-keyword: alpha:;
end;

define function set-color(color :: <color>) => ()
    glColor(color.r, color.g, color.b, color.a);
end;

define function make-color(red, green, blue) => (result)
    make(<color>, red: red, green: green, blue: blue)
end;

define constant $PI = 3.141592654;
define constant $PI-OVER-3 = $PI / 3.0;

define constant $ROOT-3-OVER-2 = sqrt(3.0) * 0.5;

define constant $ROCK-COLOR = make-color(0.5, 0.5, 0.5);
define constant $GROUND-COLOR = make-color(1.0, 1.0, 0.8);
define constant $RED-ANTHILL-COLOR = make-color(1.0, 0.8, 0.8);
define constant $BLACK-ANTHILL-COLOR = make-color(0.8, 0.8, 0.8);
define constant $FOOD-COLOR = make-color(0.0, 1.0, 0.0);

define constant $WHITE = make-color(1.0, 1.0, 1.0);

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

define function draw-world(width, height) => ()
    let even-row-start-x = 1.0;
    let odd-row-start-x = even-row-start-x + $ROOT-3-OVER-2;
    let start-y = 1.0;
    let x-offset = 2 * $ROOT-3-OVER-2;
    let y-offset = 1.5;
    
    for (y-index from 0 below height)
        for (x-index from 0 below width)
            let x = x-index * x-offset;
            let y = y-index * y-offset;
            
            if (modulo(y-index, 2) == 0)
                set-color($GROUND-COLOR);
                draw-hex(even-row-start-x + x, start-y + y);
            else
                set-color($RED-ANTHILL-COLOR);
                draw-hex(odd-row-start-x + x, start-y + y);
            end;
        end;
    end;
    
    set-color($WHITE);
end;

define variable draw :: <function> =
        callback-method() => ();
    glClear($GL-COLOR-BUFFER-BIT + $GL-DEPTH-BUFFER-BIT);

    glPushMatrix();
    glScale(50.0, 50.0, 1.0);
    draw-world(10, 10);
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
            as(<GLdouble>, height),
            0.0,
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
