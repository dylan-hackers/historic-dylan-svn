module:    inertia-shapes
synopsis:  Core UI widgets
author:    Mike Austin
copyright: Copyright (C) 2005 Mike L. Austin.  All rights reserved.
license:   MIT/BSD, see LICENCE.txt for details

//
// inertia-widgets.dylan
//

// A <widget> is a shape that more closely resembles a standard control, such as a button, window
// or scroll-bar.

define class <widget> (<rectangle>)
  inherited slot line-width = 1.0;
  inherited slot line-color = vector (0.5, 0.5, 0.5, 1.0);
end;

// A <shape-editor> is used to edit shape geometries.  It defines a few send-event so that it can
// capture mouse events.

define class <shape-editor> (<widget>)
  inherited slot line-width = 1.0;
  //inherited slot fill-color = vector (1.0, 1.0, 1.0, 1.0);
  inherited slot line-color = vector (0.5, 0.5, 0.5, 1.0);
  slot grabbed-shape :: <shape>;
  slot first-mouse :: <point>;
end;

define class <label> (<widget>)
end;

define class <button> (<widget>)
end;

define class <push-button> (<button>)
  inherited slot line-width = 1.0;
  inherited slot fill-color = vector (0.9, 0.9, 0.9, 1.0);
  inherited slot line-color = vector (0.5, 0.5, 0.5, 1.0);
  slot caption :: <string> = "", init-keyword: caption:;
  //inherited slot extent = make (<point>, x: 100.0, y: 25.0);
  keyword width: = 100.0;
  keyword height: = 25.0;
end;

define method class-name (button :: <push-button>) "<push-button>" end;

define class <title-bar> (<widget>)
  inherited slot fill-color = vector (0.75, 0.87, 1.0, 1.0);
  slot first-mouse :: <point>;
end;

define method class-name (button :: <title-bar>) "<title-bar>" end;

define class <window> (<widget>)
  inherited slot line-width = 1.0;
  inherited slot fill-color = vector (0.95, 0.95, 0.95, 1.0);
  inherited slot line-color = vector (0.5, 0.5, 0.5, 1.0);
  slot caption :: <string> = "", init-keyword: caption:;
  keyword width: = 300.0;
  keyword height: = 200.0;
end;

define method class-name (button :: <window>) "<window>" end;

define method initialize (window :: <window>, #rest init-args, #key) => ()
  next-method ();
  add-child (window, make (<title-bar>, width: 300.0, height: 25.0));
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

define method on-mouse-event (title-bar :: <title-bar>, event :: <mouse-down-event>, button == $GLUT-LEFT-BUTTON)
  title-bar.first-mouse := event.origin;
  next-method ();
end;

//define method on-mouse-event (title-bar :: <title-bar>, event :: <mouse-drag-event>, button == $left-button)
define method on-mouse-event (title-bar :: <title-bar>, event :: <mouse-drag-event>, button == $GLUT-LEFT-BUTTON)
  title-bar.parent.origin := title-bar.parent.origin + (event.origin - title-bar.first-mouse);
  next-method ();
end;

// ---------------------------------------------------------------------------------------------- //
// window methods definitions
// ---------------------------------------------------------------------------------------------- //

define method send-event
    (window :: <window>, event :: <mouse-down-event>, button :: <integer>)
 => (result :: <shape>)
  remove-child (window.parent, window);
  add-child (window.parent, window);
  next-method ();
end;

// ---------------------------------------------------------------------------------------------- //
// shape-editor methods definitions
// ---------------------------------------------------------------------------------------------- //

define method send-event
    (editor :: <shape-editor>, event :: <mouse-down-event>, button == $GLUT-LEFT-BUTTON)
 => (result :: <shape>)
  editor.grabbed-shape := next-method ();
  editor.first-mouse := event.origin - editor.grabbed-shape.origin;
  editor;
end;

define method send-event
    (editor :: <shape-editor>, event :: <mouse-down-event>, button == $GLUT-RIGHT-BUTTON)
 => (result :: <shape>)
  *menu*.origin := event.origin - *menu*.extent / 2.0;
  editor;
end;

define method on-mouse-event
    (editor :: <shape-editor>, event :: <mouse-drag-event>, button == $GLUT-LEFT-BUTTON)
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
    (editor :: <shape-editor>, event :: <mouse-up-event>, button :: <integer>)
 => (result :: <shape>)
  editor.grabbed-shape.mouse-mode := #"normal";
  editor;
end;

define method send-event
    (editor :: <shape-editor>, event :: <mouse-up-event>, button == $GLUT-RIGHT-BUTTON)
 => (result :: <shape>)
  *menu*.origin := *screen*.origin;
  editor;
end;

