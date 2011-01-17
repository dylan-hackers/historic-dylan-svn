module:    inertia-shapes
synopsis:  Core UI widgets
author:    Mike Austin
copyright: Copyright (C) 2005 Mike L. Austin.  All rights reserved.
license:   MIT/BSD, see LICENCE.txt for details

//
// widget-shape-editor.dylan
//

// A <shape-editor> is used to edit shape geometries.  It defines a few send-events so that it can
// capture mouse events.

define class <shape-editor> (<widget>)
  inherited slot line-width = 1.0;
  inherited slot fill-color = vector (1.0, 1.0, 1.0, 1.0);
  slot grabbed-shape :: false-or (<shape>) = #f;
  slot first-mouse :: <point>;
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

