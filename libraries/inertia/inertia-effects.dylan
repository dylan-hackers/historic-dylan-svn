module:    inertia-shapes
synopsis:  Core shape effects
author:    Mike Austin
copyright: Copyright (C) 2005 Mike L. Austin.  All rights reserved.
license:   MIT/BSD, see LICENCE.txt for details

//
// inertia-effects.dylan
//

define constant <effect-layer> = one-of (#"below", #"inside", #"above");

// ---------------------------------------------------------------------------------------------- //
// all class definitions
// ---------------------------------------------------------------------------------------------- //

define class <shape-effect> (<object>)
  slot effect-layer :: one-of (#"below", #"inside", #"above");
end;

define class <gradient-effect> (<shape-effect>)
  inherited slot effect-layer = #"inside";
  slot gradient-angle = 0.0, init-keyword: angle:;
  slot gradient-color0 = #[0.0, 0.0, 0.0, 0.0];
  slot gradient-color1 = #[0.0, 0.0, 0.0, 1.0];
end;

define class <shadow-effect> (<shape-effect>)
  inherited slot effect-layer = #"below";
end;

// ---------------------------------------------------------------------------------------------- //
// shape-effect methods definitions
// ---------------------------------------------------------------------------------------------- //

define method draw-effect (shape :: <shape>, effect :: <gradient-effect>)
  glBegin ($GL-QUADS);
    glColor (1.0, 1.0, 1.0, 0.6); glVertex (              0.0,                      0.0, 0.0);
    glColor (1.0, 1.0, 1.0, 0.0); glVertex (              0.0, shape.shape-height / 2.0, 0.0);
    glColor (1.0, 1.0, 1.0, 0.0); glVertex (shape.shape-width, shape.shape-height / 2.0, 0.0);
    glColor (1.0, 1.0, 1.0, 0.6); glVertex (shape.shape-width,                      0.0, 0.0);

    glColor (0.0, 0.0, 0.0, 0.0); glVertex (              0.0, shape.shape-height / 2.0, 0.0);
    glColor (0.0, 0.0, 0.0, 0.05); glVertex (              0.0,       shape.shape-height, 0.0);
    glColor (0.0, 0.0, 0.0, 0.05); glVertex (shape.shape-width,       shape.shape-height, 0.0);
    glColor (0.0, 0.0, 0.0, 0.0); glVertex (shape.shape-width, shape.shape-height / 2.0, 0.0);
  glEnd ();
end;

define method draw-effect (shape :: <shape>, effect :: <shadow-effect>)
  glPushMatrix ();
    glColor (0.0, 0.0, 0.0, 0.03);
    glTranslate (3.0, 0.0, 0.0);
    draw-content (shape, shape);
    glTranslate (-3.0, 3.0, 0.0);
    draw-content (shape, shape);
    glTranslate (3.0, 2.0, 0.0);
    draw-content (shape, shape);
    glTranslate (2.0, -2.0, 0.0);
    draw-content (shape, shape);
  glPopMatrix ();
end;

