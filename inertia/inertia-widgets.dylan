module:    inertia-shapes
synopsis:  Core UI widgets
author:    Mike Austin
copyright: Copyright (C) 2005 Mike L. Austin.  All rights reserved.
license:   MIT/BSD, see LICENCE.txt for details

//
// inertia-widgets.dylan
//

// ---------------------------------------------------------------------------------------------- //
// all class definitions
// ---------------------------------------------------------------------------------------------- //

// A <widget> is a shape that more closely resembles a standard control, such as a button, window
// or scroll-bar.

define class <widget> (<rectangle>)
  inherited slot fill-color = vector (0.95, 0.95, 0.95, 1.0);
  inherited slot line-color = vector (0.75, 0.75, 0.75, 1.0);
  inherited slot line-width = 1.0;
end;

// A <shape-editor> is used to edit shape geometries.  It defines a few send-events so that it can
// capture mouse events.

define class <shape-editor> (<widget>)
  inherited slot line-width = 1.0;
  //inherited slot fill-color = vector (1.0, 1.0, 1.0, 1.0);
  slot grabbed-shape :: false-or (<shape>) = #f;
  slot first-mouse :: <point>;
end;

define class <label> (<widget>)
end;

define class <button> (<widget>)
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

// - title-bar ---------------------------------------------------------------------------------- //

define class <title-bar> (<widget>)
  inherited slot fill-color = vector (0.75, 0.87, 1.0, 1.0);
  inherited slot effects = vector (make (<gradient-effect>));
  inherited slot reshape = #[#"size", #"none"];
  slot first-mouse :: <point>;
end;

define method class-name (title-bar :: <title-bar>) "<title-bar>" end;

// - window-sizer ------------------------------------------------------------------------------- //

define class <window-sizer> (<widget>)
  inherited slot reshape = #[#"move", #"move"];
  slot first-mouse :: <point>;
  keyword width: = 20.0;
  keyword height: = 20.0;
end;

define method class-name (window-sizer :: <window-sizer>) "<window-sizer>" end;

// - window ------------------------------------------------------------------------------------- //

define class <window> (<widget>)
  inherited slot line-width = 1.0;
  inherited slot fill-color = vector (0.95, 0.95, 0.95, 1.0);
  inherited slot effects = vector (make (<shadow-effect>));
  slot caption :: <string> = "", init-keyword: caption:;
  keyword width: = 300.0;
  keyword height: = 200.0;
end;

define method class-name (window :: <window>) "<window>" end;

define method initialize (window :: <window>, #rest init-args, #key) => ()
  next-method ();
  add-child (window, make (<title-bar>, width: 300.0, height: 25.0));
  add-child (window, make (<window-sizer>, left: 280.0, top: 180.0));
end;

// ---------------------------------------------------------------------------------------------- //
// push-button methods definitions
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

// ---------------------------------------------------------------------------------------------- //
// title-bar methods definitions
// ---------------------------------------------------------------------------------------------- //

define method draw-overlay (shape :: <shape>, menu :: <title-bar>)
  next-method ();
  glColor (0.0, 0.0, 0.0, 1.0);
  glPushMatrix ();
  glTranslate (shape.extent.point-x / 2.0, shape.extent.point-y / 2.0, 0.0);
    draw-centered-string (0, 5, shape.parent.caption);
  glPopMatrix ();
end;

define method on-mouse-event (title-bar :: <title-bar>, event :: <mouse-down-event>, button == $left-button)
  title-bar.first-mouse := event.origin;
  next-method ();
end;

define method on-mouse-event (title-bar :: <title-bar>, event :: <mouse-drag-event>, button == $left-button)
  title-bar.parent.origin := title-bar.parent.origin + (event.origin - title-bar.first-mouse);
  next-method ();
end;

// ---------------------------------------------------------------------------------------------- //
// window-sizer methods definitions
// ---------------------------------------------------------------------------------------------- //

define method on-mouse-event (window-sizer :: <window-sizer>, event :: <mouse-down-event>, mouse-button == $left-button)
  window-sizer.first-mouse := event.origin;
  next-method ();
end;

define method on-mouse-event (window-sizer :: <window-sizer>, event :: <mouse-drag-event>, mouse-button == $left-button)
  window-sizer.parent.extent := window-sizer.parent.extent + (event.origin - window-sizer.first-mouse);
  //window-sizer.origin := window-sizer.origin + (event.origin - window-sizer.first-mouse);
  next-method ();
end;

// ---------------------------------------------------------------------------------------------- //
// window methods definitions
// ---------------------------------------------------------------------------------------------- //

define method send-event
    (window :: <window>, event :: <mouse-down-event>, button :: <mouse-button>)
 => (result :: <shape>)
  remove-child (window.parent, window);
  add-child (window.parent, window);
  next-method ();
end;

// ---------------------------------------------------------------------------------------------- //
// shape-editor methods definitions
// ---------------------------------------------------------------------------------------------- //

define method draw-overlay (shape :: <shape>, editor :: <shape-editor>)
  if (editor.grabbed-shape)
    draw-grabber (editor.grabbed-shape);
  end;
end;

define method send-event
    (editor :: <shape-editor>, event :: <mouse-down-event>, button == $left-button)
 => (result :: <shape>)
  editor.grabbed-shape := next-method ();
  editor.first-mouse := event.origin - editor.grabbed-shape.origin;
  editor;
end;

define method send-event
    (editor :: <shape-editor>, event :: <mouse-down-event>, button == $right-button)
 => (result :: <shape>)
  *menu*.origin := event.origin - *menu*.extent / 2.0;
  editor;
end;

define method on-mouse-event
    (editor :: <shape-editor>, event :: <mouse-drag-event>, button == $left-button)
 => ()
  if (editor.grabbed-shape.mouse-mode == #"gripper")
    let delta = event.origin - editor.grabbed-shape.screen-origin;

    block ()
      editor.grabbed-shape.z-angle := atan2 (delta.point-y, delta.point-x) / ($PI / 180.0);
    exception (<error>)
      // Don't care if x and y are both 0
    end;
    editor.grabbed-shape.z-scale := point-length (delta) / 50.0;
  else
    editor.grabbed-shape.origin := event.origin - editor.first-mouse;
  end;
  editor;
end;

define method on-mouse-event
    (editor :: <shape-editor>, event :: <mouse-up-event>, button :: <mouse-button>)
 => (result :: <shape>)
  editor.grabbed-shape.mouse-mode := #"normal";
  editor;
end;

define method send-event
    (editor :: <shape-editor>, event :: <mouse-up-event>, button == $right-button)
 => (result :: <shape>)
  *menu*.origin := *screen*.origin;
  editor;
end;

define method draw-grabber (shape :: <shape>)
    glColor (0.0, 0.0, 0.0);
    glPushMatrix ();
    glTranslate (shape.shape-left, shape.shape-top, 0.0);
    glRotate (shape.z-angle, 0.0, 0.0, 1.0);

    glLineWidth (1.0s0);
    glBegin ($GL-LINES);
      glVertex (0.0, 0.0);
      glVertex (50.0 * shape.z-scale, 0.0, 0.0);
    glEnd ();
    
    glBegin ($GL-QUADS);
      glVertex (50.0 * shape.z-scale +  0.0, -5.0);
      glVertex (50.0 * shape.z-scale +  0.0,  5.0);
      glVertex (50.0 * shape.z-scale + 10.0,  5.0);
      glVertex (50.0 * shape.z-scale + 10.0, -5.0);
    glEnd ();
    glPopMatrix ();
end;

