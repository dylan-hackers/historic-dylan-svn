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

define constant $COS-PI-OVER-3 = cos($PI-OVER-3);
define constant $SIN-PI-OVER-3 = sin($PI-OVER-3);

define constant $ROOT-3-OVER-2 = sqrt(3.0) * 0.5;

define constant $ROCK-COLOR = make-gl-color(0.5, 0.5, 0.5);
define constant $GROUND-COLOR = make-gl-color(1.0, 1.0, 0.8);
define constant $RED-ANTHILL-COLOR = make-gl-color(1.0, 0.8, 0.8);
define constant $BLACK-ANTHILL-COLOR = make-gl-color(0.8, 0.8, 0.8);
define constant $FOOD-COLOR = make-gl-color(0.0, 1.0, 0.0);

define constant $RED-ANT-COLOR = make-gl-color(1.0, 0.0, 0.0);
define constant $BLACK-ANT-COLOR = make-gl-color(0.0, 0.0, 0.0);

define constant $WHITE = make-gl-color(1.0, 1.0, 1.0);
define constant $BLUE = make-gl-color(0.0, 0.0, 1.0);

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

define function draw-hex(position, color) => ()
    let (x, y) = cell-position(position);

    glBegin($GL-TRIANGLE-FAN);
        set-gl-color(color);
        glVertex(x                 , y                 );
        set-gl-color(color);
        glVertex(x                 , y + 1             );
        set-gl-color(color);
        glVertex(x + $SIN-PI-OVER-3, y + $COS-PI-OVER-3);
        set-gl-color(color);
        glVertex(x + $SIN-PI-OVER-3, y - $COS-PI-OVER-3);
        set-gl-color(color);
        glVertex(x                 , y - 1             );
        set-gl-color(color);
        glVertex(x - $SIN-PI-OVER-3, y - $COS-PI-OVER-3);
        set-gl-color(color);
        glVertex(x - $SIN-PI-OVER-3, y + $COS-PI-OVER-3);
        set-gl-color(color);
        glVertex(x                 , y + 1             );
    glEnd();
end;

define function cell-color(cell) => (color)
    if (cell.rocky)
        $ROCK-COLOR
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
    draw-hex(position, cell-color(cell));
end;

define constant $font = $GLUT-BITMAP-9-BY-15;

define function draw-cell-food(cell, position) => ()
    if (cell.food > 0)
      let (x, y) = cell-position(position);
      glPushMatrix();
      glTranslate(x, y, 0.0);
      set-gl-color($FOOD-COLOR);
      glBegin($GL-QUADS);
      glVertex(-$SIN-PI-OVER-3,       0.0);
      glVertex(-0.5 * $SIN-PI-OVER-3, -0.5 * $SIN-PI-OVER-3);
      glVertex(0.0,                   0.0);
      glVertex(-0.5 * $SIN-PI-OVER-3, 0.5 * $SIN-PI-OVER-3);
      glEnd();
      glPopMatrix();
/*
        let count-string = integer-to-string(cell.food);
        
        let total-width = 0.0;
        for (character in count-string)
            total-width := total-width +
                glutStrokeWidth($font,
                                as(<integer>, character));
        end;
    
        glPushMatrix();
            glTranslate(x, y, 0.0);
            glScale(0.01, -0.01, 1.0);
            glTranslate(-0.5 * total-width, -50.0, 0.0);
            set-gl-color($FOOD-COLOR);
            for (character in count-string)
                glutStrokeCharacter($font,
                                    as(<integer>, character));
            end;
        glPopMatrix();
*/
    end;
end;

define constant $ONE-EIGHTH = 0.125;
define constant $ONE-COLOR = make-gl-color(0.0, 0.0, 0.8);
define constant $TWO-COLOR = make-gl-color(0.8, 0.0, 0.0);
define constant $THREE-COLOR = make-gl-color(0.0, 0.8, 0.0);
define constant $TRAIL-COLOR = make-gl-color(0.3, 0.3, 0.3);


define function draw-markers(cell, position) => ()
  let (x, y) = cell-position(position);
  
  glPushMatrix();
  glTranslate(x, y, 0.0);
  glBegin($GL-QUADS);
  let val = 0;
  if (cell.black-marker[0]) val := val + 2 end;
  if (cell.black-marker[1]) val := val + 1 end;

  if (cell.black-marker[2])
    set-gl-color($TRAIL-COLOR);
    glVertex(-1,1);
    glVertex(1,1);
    glVertex(1,-1);
    glVertex(-1,-1);
  end;

  if (val > 0)
    set-gl-color(select (val)
                   1 => $ONE-COLOR;
                   2 => $TWO-COLOR;
                   3 => $THREE-COLOR;
                 end);
    glVertex(-0.5,0.5);
    glVertex(0.5,0.5);
    glVertex(0.5,-0.5);
    glVertex(-0.5,-0.5);
  end;


  glEnd();
  glPopMatrix();
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
            glVertex(-$SIN-PI-OVER-3, -$COS-PI-OVER-3);
            glVertex(-$SIN-PI-OVER-3,  $COS-PI-OVER-3);
        glEnd();
        
        if (ant.has-food)
            set-gl-color($FOOD-COLOR);
            glBegin($GL-QUADS);
                glVertex(-$SIN-PI-OVER-3,       0.0);
                glVertex(-0.5 * $SIN-PI-OVER-3, -0.5 * $SIN-PI-OVER-3);
                glVertex(0.0,                   0.0);
                glVertex(-0.5 * $SIN-PI-OVER-3, 0.5 * $SIN-PI-OVER-3);
            glEnd();
        end;
    glPopMatrix();
end;

define variable *world-display-list* = 0;

define function cache-world-display-list() => ()
    *world-display-list* := glGenLists(1);
    glNewList(*world-display-list*, $GL-COMPILE);

    let width = *world*.world-x;
    let height = *world*.world-y;

    for (y-index from 0 below height)
        for (x-index from 0 below width)
            let position = make-position(x-index, y-index);
            let cell = cell-at(position);
            
            draw-cell(cell, position);
        end;
    end;
    
    glEndList();
end;

define function draw-world() => ()
    glCallList(*world-display-list*);
    
    let width = *world*.world-x;
    let height = *world*.world-y;

    for (y-index from 0 below height)
        for (x-index from 0 below width)
            let position = make-position(x-index, y-index);
            let cell = cell-at(position);

            glLineWidth(1.0s0);
            draw-markers(cell, position);
            
            glLineWidth(3.0s0);
            draw-cell-food(cell, position);
        
            let this-ant = ant-at(position);
            if (this-ant)
                draw-ant(this-ant, position);
            end;
            
        end;
    end;
end;

define variable *scale* = 5.0;

define variable *x-offset* = 0.0;
define variable *y-offset* = 0.0;

define variable draw :: <function> =
        callback-method() => ();
    glClear($GL-COLOR-BUFFER-BIT + $GL-DEPTH-BUFFER-BIT);

    glPushMatrix();
    glScale(*scale*, *scale*, 1.0);
    glTranslate(*x-offset*, *y-offset*, 0.0);
    draw-world();
    glPopMatrix();
    
    glutSwapBuffers();
    glutReportErrors();
    glutPostRedisplay();
end;

define variable *window-x* = 1000; 
define variable *window-y* = 1000; 

define variable reshape :: <function> =
        callback-method(width :: <integer>, height :: <integer> ) => ();                
    *window-x* := width;
    *window-y* := height;
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

define variable *gen-count* :: <integer> = 0;

define variable idle :: <function> =
        callback-method() => ();
    for(i from 1 to 10)
      step-world();
      *gen-count* := *gen-count* + 1;
      if (modulo(*gen-count*, 1000) == 0)
        format-out("%d generations\n", *gen-count*);
      end;
    end;
end;

define variable keyboard :: <function> =
        callback-method(key :: <integer>, x :: <integer>, y :: <integer>) => ();
    let char = as(<character>, key);
    
    if (char == '+' | char == '=')
        *scale* := *scale* * 2.0;
      *x-offset* := *x-offset* - (*window-x* / *scale*) / 2.0; 
      *y-offset* := *y-offset* - (*window-y* / *scale*) / 2.0; 
    elseif (char == '-')
      *x-offset* := *x-offset* + (*window-x* / *scale*) / 2.0; 
      *y-offset* := *y-offset* + (*window-y* / *scale*) / 2.0; 
        *scale* := *scale* * 0.5;
    end if;
end;

define variable *original-mouse-x* = 0;
define variable *original-mouse-y* = 0;

define function update-offsets(x, y) => ()
    let x-distance = x - *original-mouse-x*;
    let y-distance = y - *original-mouse-y*;
    
    let world-x-distance = x-distance / *scale*;
    let world-y-distance = y-distance / *scale*;

    *x-offset* := *x-offset* + world-x-distance;
    *y-offset* := *y-offset* + world-y-distance;
    
    *original-mouse-x* := x;
    *original-mouse-y* := y;
end;

define variable mouse :: <function> =
        callback-method(button :: <integer>,
                        state :: <integer>,
                        x :: <integer>,
                        y :: <integer>) => ();
    if (state == $GLUT-DOWN)
        *original-mouse-x* := x;
        *original-mouse-y* := y;
    else
        update-offsets(x, y);
    end;
end;

define variable motion :: <function> =
        callback-method(x :: <integer>,
                        y :: <integer>) => ();
    update-offsets(x, y);
end;

begin
  load-world();
  glut-init();
  glutInitDisplayMode($GLUT-RGB + $GLUT-DEPTH + $GLUT-DOUBLE);
  
  glutInitWindowPosition(0, 0);
  glutInitWindowSize(*window-x*, *window-y*);
  glutCreateWindow("Marching Dylants");
  
  cache-world-display-list();
  
  glutDisplayFunc(draw);
  glutReshapeFunc(reshape);
  glutIdleFunc(idle);
  glutKeyboardFunc(keyboard);
  glutMouseFunc(mouse);
  glutMotionFunc(motion);
  
  glutMainLoop();
end;
