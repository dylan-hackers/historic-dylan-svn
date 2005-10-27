module:    inertia-shapes
synopsis:  Core UI shapes
author:    Mike Austin
copyright: Copyright (C) 2005 Mike L. Austin.  All rights reserved.
license:   MIT/BSD, see LICENCE.txt for details

//
// inertia-widgets
//

define class <widget> (<rectangle>)
end;

define class <shape-editor> (<widget>)
  inherited slot line-width = 1.0;
  //inherited slot fill-color = vector (1.0, 1.0, 1.0, 1.0);
  inherited slot line-color = vector (0.25, 0.25, 0.25, 1.0);
end;

define class <label> (<widget>)
end;

define class <button> (<widget>)
end;

define class <push-button> (<button>)
  inherited slot line-width = 1.0;
  inherited slot fill-color = vector (0.9, 0.9, 0.9, 1.0);
  inherited slot line-color = vector (0.4, 0.4, 0.4, 1.0);
  //inherited slot extent = make (<point>, x: 100.0, y: 25.0);
  keyword width: = 100.0;
  keyword height: = 25.0;
  slot caption :: <string> = "", init-keyword: caption:;
end;

define method class-name (button :: <push-button>) "<push-button>" end;

// ---------------------------------------------------------------------------------------------- //

define method draw-overlay (shape :: <shape>, menu :: <push-button>)
  next-method ();
  glColor (0.0, 0.0, 0.0, 1.0);
  glPushMatrix ();
  glTranslate (shape.extent.point-x / 2.0, shape.extent.point-y / 2.0, 0.0);
    draw-centered-string (0, 5, shape.caption);
  glPopMatrix ();
end;

define method xsend-event
    (shape :: <shape-editor>, event :: <mouse-event>, button :: <integer>)
 => (result :: <shape>)
  format-out ("send-event (<shape-editor>)\n");
  shape;
end;

define method xdraw-shape (editor :: <shape-editor>) => ()
end;

