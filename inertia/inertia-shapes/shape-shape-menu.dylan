module:    inertia-shapes
synopsis:  Core UI shapes
author:    Mike Austin
copyright: Copyright (C) 2005 Mike L. Austin.  All rights reserved.
license:   MIT/BSD, see LICENCE.txt for details

//
// shape-shape-menu.dylan
//

define class <shape-menu-center> (<rectangle>)
  inherited slot fill-color = vector (0.5, 0.5, 0.5, 0.9);
  inherited slot line-width = 1.0;
  keyword width: = 120.0;
  keyword height: = 120.0;
end;

define class <shape-menu> (<rectangle>)
  inherited slot fill-color = vector (0.5, 0.5, 0.5, 0.9);
  inherited slot line-width = 1.0;
end;

define method initialize (menu :: <shape-menu>, #rest init-args, #key) => ()
  next-method ();
  add-child (menu, make (<shape-menu-center>, left: menu.shape-width / 2.0 - 60.0,
                                              top: menu.shape-height / 2.0 - 60.0));
end;

define method class-name (menu :: <shape-menu>) "<shape-menu>" end;

// ------------------------------------------------------------------------- //
// shape-menu methods definitions
// ------------------------------------------------------------------------- //

define method draw-content (shape :: <shape>, menu :: <shape-menu-center>)
  let radius = 60.0;

  glPushMatrix ();
  glTranslate (shape.shape-width / 2.0, shape.shape-height / 2.0, 0.0);
  glBegin ($GL-TRIANGLE-FAN);
    glVertex (0.0, 0.0, 0.0);
    for (angle from 0 to $PI * 2 by $PI / 20.0)
      glVertex (cos (angle) * radius, sin (angle) * radius, 0.0);
    end;
    glVertex (cos (0) * radius, sin (0) * radius, 0.0);
  glEnd ();

  glLineWidth (2.0s0);
  glColor (1.0, 1.0, 1.0, 0.7);

  glBegin ($GL-LINE-LOOP);
    for (angle from 0 to $PI * 2 by $PI / 20.0)
      glVertex (cos (angle) * radius, sin (angle) * radius, 0.0);
    end;
  glEnd ();

  glBegin ($GL-TRIANGLE-FAN);
    glVertex (0.0, 0.0, 0.0);
    for (angle from 0 to $PI * 2 by $PI / 20.0)
      glVertex (cos (angle) * 5.0, sin (angle) * 5.0, 0.0);
    end;
    glVertex (cos (0) * 5.0, sin (0) * 5.0, 0.0);
  glEnd ();

  glBegin ($GL-LINE-LOOP);
    for (angle from 0 to $PI * 2 by $PI / 20.0)
      glVertex (cos (angle) * 5.0, sin (angle) * 5.0, 0.0);
    end;
  glEnd ();
  
  glBegin ($GL-LINES);
    glVertex ( cos ($PI * (1.0 / 4.0)) * radius,  sin ($PI * (1.0 / 4.0)) * radius);
    glVertex (-cos ($PI * (1.0 / 4.0)) * radius, -sin ($PI * (1.0 / 4.0)) * radius);
    glVertex ( cos ($PI * (3.0 / 4.0)) * radius,  sin ($PI * (3.0 / 4.0)) * radius);
    glVertex (-cos ($PI * (3.0 / 4.0)) * radius, -sin ($PI * (3.0 / 4.0)) * radius);
  glEnd ();
  glPopMatrix ();
end;

define method draw-overlay (shape :: <shape>, menu :: <shape-menu-center>) => ()
  next-method ();
  let radius = 60.0;
  glPushMatrix ();
  glTranslate (shape.shape-width / 2.0, shape.shape-height / 2.0, 0.0);

  glColor (1.0, 1.0, 1.0, 1.0);

  draw-centered-string (  0, -40 + 5, "Cut");
  draw-centered-string (-35,   0 + 5, "Copy");
  draw-centered-string ( 35,   0 + 5, "Paste");
  draw-centered-string (  0,  40 + 5, "Clone");
  glPopMatrix ();
end;

define method draw-effects (menu :: <shape-menu-center>, layer :: <effect-layer>) end;
define method draw-outline (menu :: <shape-menu-center>) end;

// ------------------------------------------------------------------------- //

define method draw-overlay (shape :: <shape>, menu :: <shape-menu>) => ()
  next-method ();
  let radius = 60.0;
  glPushMatrix ();
  glTranslate (shape.shape-width / 2.0, shape.shape-height / 2.0, 0.0);

  glColor (1.0, 1.0, 1.0, 1.0);
  //draw-centered-string (0, -50 + 7, "Bring to Front");
  draw-centered-string (0, -75 + 5, "Bring Forward");
  draw-centered-string (0,  75 + 5, "Send Backward");
  //draw-centered-string (0,  50 + 7, "Send to Back");
  glPopMatrix ();
end;

define method draw-effects (menu :: <shape-menu>, layer :: <effect-layer>) end;
define method draw-outline (menu :: <shape-menu>) end;

define method draw-centered-string (x, y, string :: <string>)
  local draw-string (x, y, string)
    let width :: <integer> = glutxBitmapLength ($GLUT-BITMAP-HELVETICA-12, string);
    glRasterPos (round/ (-width, 2.0) + x, y);
    glutxBitmapString ($GLUT-BITMAP-HELVETICA-12, string);
  end;

  draw-string (x, y, string);
  draw-string (x + 1, y, string);
end;

