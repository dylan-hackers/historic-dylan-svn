module:    inertia-shapes
synopsis:  Core UI widgets
author:    Mike Austin
copyright: Copyright (C) 2005 Mike L. Austin.  All rights reserved.
license:   MIT/BSD, see LICENCE.txt for details

//
// widget-button.dylan
//

define class <button> (<widget>)
  slot on-click;
end;

define class <button-clicked-event> (<mouse-event>)
end;

// - push-button -------------------------------------------------------------------------------- //

define class <push-button> (<button>)
  inherited slot line-width = 1.0;
  inherited slot fill-color = vector (0.9, 0.9, 0.9, 1.0);
  inherited slot effects = vector (make (<gradient-effect>));
  slot caption :: <string> = "", init-keyword: caption:;
  keyword width: = 100.0;
  keyword height: = 25.0;
end;

define method class-name (button :: <push-button>) "<push-button>" end;

define method make-from-symbol (symbol == #"<push-button>", #rest init-args, #key)
  let object = apply (make, <push-button>, init-args);
  object.fields[#"caption"] := make (<field>, getter: caption, setter: caption-setter);
  object.fields[#"on-click"] := make (<field>, getter: on-click, setter: on-click-setter);
  object;
end;

// ---------------------------------------------------------------------------------------------- //
// methods definitions
// ---------------------------------------------------------------------------------------------- //

define method draw-overlay (shape :: <shape>, menu :: <push-button>)
  next-method ();
  glColor (0.0, 0.0, 0.0, 1.0);
  glPushMatrix ();
  glTranslate (shape.extent.point-x / 2.0, shape.extent.point-y / 2.0, 0.0);
    draw-centered-string (0, 5, shape.caption);
  glPopMatrix ();
end;

define method on-mouse-event (button :: <push-button>, event :: <mouse-enter-event>, mouse-button :: <mouse-button>)
  button.fill-color := vector (0.75, 0.87, 1.0, 1.0);
  next-method ();
  glutPostRedisplay ();
end;

define method on-mouse-event (button :: <push-button>, event :: <mouse-exit-event>, mouse-button :: <mouse-button>)
  button.fill-color := vector (0.95, 0.95, 0.95, 1.0);
  next-method ();
  glutPostRedisplay ();
end;

