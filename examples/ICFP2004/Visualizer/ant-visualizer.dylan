module: ant-visualizer
synopsis: 
author: 
copyright: 

define functional class <gl-color> (<object>)
    constant slot r :: <float>, required-init-keyword: red:;
    constant slot g :: <float>, required-init-keyword: green:;
    constant slot b :: <float>, required-init-keyword: blue:;
    constant slot a :: <float> = 1.0, init-keyword: alpha:;
end;

define function set-gl-color(color :: <gl-color>) => ()
    glColor(color.r, color.g, color.b, color.a);
end;

define function make-gl-color(red, green, blue) => (result)
    make(<gl-color>, red: red, green: green, blue: blue)
end;

define constant $PI = 3.141592654;
define constant $PI-OVER-3 = $PI / 3.0;
define constant $180-OVER-PI = 180.0 / $PI;

define constant $ROOT-3-OVER-2 = sqrt(3.0) * 0.5;

define constant $ROCK-COLOR = make-gl-color(0.5, 0.5, 0.5);
define constant $GROUND-COLOR = make-gl-color(1.0, 1.0, 0.8);
define constant $RED-ANTHILL-COLOR = make-gl-color(1.0, 0.8, 0.8);
define constant $BLACK-ANTHILL-COLOR = make-gl-color(0.8, 0.8, 0.8);
define constant $FOOD-COLOR = make-gl-color(0.0, 1.0, 0.0);

define constant $RED-ANT-COLOR = make-gl-color(1.0, 0.0, 0.0);
define constant $BLACk-ANT-COLOR = make-gl-color(0.0, 0.0, 0.0);

define constant $WHITE = make-gl-color(1.0, 1.0, 1.0);

define constant $EVEN-ROW-START-X = 1.0;
define constant $ODD-ROW-START-X = $EVEN-ROW-START-X + $ROOT-3-OVER-2;
define constant $START-Y = 1.0;
define constant $X-OFFSET = 2 * $ROOT-3-OVER-2;
define constant $Y-OFFSET = 1.5;

define function cell-position(position) => (x-result, y-result)
    let gl-x = position.x * $X-OFFSET;
    let gl-y = position.y * $Y-OFFSET;
    
    if (modulo(position.y, 2) == 0)
        values($EVEN-ROW-START-X + gl-x, $START-Y + gl-y);
    else
        values($ODD-ROW-START-X + gl-x, $START-Y + gl-y);
    end;
end;

define function draw-hex(position) => ()
    let (x, y) = cell-position(position);

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

define function cell-color(cell) => (color)
    if (cell.rocky)
        $ROCK-COLOR
    elseif (cell.food > 0)
        $FOOD-COLOR
    elseif (cell.anthill)
        if (cell.anthill == #"red")
            $RED-ANTHILL-COLOR
        else
            $BLACK-ANTHILL-COLOR
        end
    else
        $GROUND-COLOR
    end;
end;

define function draw-cell(cell, position) => ()
    set-gl-color(cell-color(cell));
    draw-hex(position);
end;

define function ant-color(ant) => (color)
    if (ant.color == #"red")
        $RED-ANT-COLOR
    else
        $BLACK-ANT-COLOR
    end;
end;

define function draw-ant(ant, position) => ()
    let direction = ant.direction;
    let angle = $PI-OVER-3 * direction;
    let angle-degrees = $180-OVER-PI * angle;
    let (x, y) = cell-position(position);
    
    glPushMatrix();
        glTranslate(x, y, 0.0);
        glRotate(angle-degrees, 0.0, 0.0, 1.0);
        
        set-gl-color(ant-color(ant));
        glBegin($GL-TRIANGLES);
            glVertex($ROOT-3-OVER-2, 0.0);
            glVertex(-sin($PI-OVER-3), -cos($PI-OVER-3));
            glVertex(-sin($PI-OVER-3),  cos($PI-OVER-3));
        glEnd();
    glPopMatrix();
end;

define variable *world-display-list* = 0;

define function cache-world-display-list() => ()
    *world-display-list* := glGenLists(1);
    glNewList(*world-display-list*, $GL-COMPILE);

    let world-size = dimensions(*world*);
    let width = world-size[0];
    let height = world-size[1];

    for (y-index from 0 below height)
        for (x-index from 0 below width)
            let position = make-position(x-index, y-index);
            
            draw-cell(*world*[x-index, y-index], position);
        end;
    end;
    
    glEndList();
end;

define function draw-world() => ()
    glCallList(*world-display-list*);
    
    let world-size = dimensions(*world*);
    let width = world-size[0];
    let height = world-size[1];

    for (y-index from 0 below height)
        for (x-index from 0 below width)
            let this-ant = *world*[x-index, y-index].ant;
            let position = make-position(x-index, y-index);
        
            if (this-ant)
                draw-ant(this-ant, position);
            end;
        end;
    end;
end;

define variable *scale* = 5.0;

define variable draw :: <function> =
        callback-method() => ();
    glClear($GL-COLOR-BUFFER-BIT + $GL-DEPTH-BUFFER-BIT);

    glPushMatrix();
    glScale(*scale*, *scale*, 1.0);
    draw-world();
    glPopMatrix();
    
    glutSwapBuffers();
    glutReportErrors();
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
      step-world();
    glutPostRedisplay();
end;

define variable keyboard :: <function> =
        callback-method(key :: <integer>, x :: <integer>, y :: <integer>) => ();
    let char = as(<character>, key);
    
    if (char == '+' | char == '=')
        *scale* := *scale* * 2.0;
    elseif (char == '-')
        *scale* := *scale* * 0.5;
    end if;
end;

begin
  load-world();
    glut-init();
    glutInitDisplayMode($GLUT-RGB + $GLUT-DEPTH + $GLUT-DOUBLE);

    glutInitWindowPosition(0, 0);
    glutInitWindowSize(1000, 1000);
    glutCreateWindow("Marching Dylants");
    
    cache-world-display-list();

    glutDisplayFunc(draw);
    glutReshapeFunc(reshape);
    glutIdleFunc(idle);
    glutKeyboardFunc(keyboard);

    glutMainLoop();
end;
