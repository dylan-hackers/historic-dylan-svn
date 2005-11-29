module:    inertia-shapes
synopsis:  Core UI shapes
author:    Mike Austin
copyright: Copyright (C) 2005 Mike L. Austin.  All rights reserved.
license:   MIT/BSD, see LICENCE.txt for details

//
// shape-rectangle.dylan
//

define method class-name (rectangle :: <rectangle>) "<rectangle>" end;

// ------------------------------------------------------------------------- //
// rectangle methods definitions
// ------------------------------------------------------------------------- //

define method initialize (rectangle :: <rectangle>, #rest init-args,
                          #key left = 0.0, top = 0.0, width = 100.0, height = 100.0) => ()
  apply (next-method, init-args);
  rectangle.shape-left := left;
  rectangle.shape-top := top;
  rectangle.shape-width := width;
  rectangle.shape-height := height;
end;

define method draw-content (shape :: <shape>, rectangle :: <rectangle>) => ()
  let width/2 = shape.shape-width / 2.0;
  let height/2 = shape.shape-height / 2.0;

  glBegin ($GL-QUADS);
    glVertex (                 0.0,                  0.0);
    glVertex (                 0.0, shape.shape-height);
    glVertex (shape.shape-width, shape.shape-height);
    glVertex (shape.shape-width, 0.0);
  glEnd ();
end;

define method draw-outline (rectangle :: <rectangle>) => ()
  let width/2 = rectangle.shape-width / 2.0;
  let height/2 = rectangle.shape-height / 2.0;
  let shape = rectangle;

  glBegin ($GL-LINE-LOOP);
    glVertex (                 0.0,                  0.0);
    glVertex (                 0.0, shape.shape-height);
    glVertex (shape.shape-width, shape.shape-height);
    glVertex (shape.shape-width, 0.0);
  glEnd ();
end;

define method contains-point? (rectangle :: <rectangle>, point :: <point>) => (result :: <boolean>)
  let width/2 = rectangle.shape-width / 2.0;
  let height/2 = rectangle.shape-height / 2.0;

  (point.point-x > 0 & point.point-x < rectangle.shape-width)
    & (point.point-y > 0 & point.point-y < rectangle.shape-height);
end;

