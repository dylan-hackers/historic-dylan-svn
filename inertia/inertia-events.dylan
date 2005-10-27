module:    inertia-shapes
synopsis:  Core UI shapes
author:    Mike Austin
copyright: Copyright (C) 2005 Mike L. Austin.  All rights reserved.
license:   MIT/BSD, see LICENCE.txt for details

define class <event> (<object>)
end;

define class <mouse-event> (<event>)
  slot origin :: <point>, init-keyword: origin:;
end;

define class <mouse-down-event> (<mouse-event>)
end;

define class <mouse-up-event> (<mouse-event>)
end;

define class <mouse-motion-event> (<mouse-event>)
end;

define class <mouse-drag-event> (<mouse-event>)
end;

define class <mouse-gripper-event> (<mouse-event>)
end;

// ---------------------------------------------------------------------------------------------- //
// event method definitions
// ---------------------------------------------------------------------------------------------- //

define method send-event (shape :: <shape>, event :: <mouse-event>, button :: <integer>) => (result :: <shape>)
  //format-out ("send-event (<shape>, <mouse-event>)\n");
  block (return)
    for (child in shape.children)
      let x = event.origin.point-x - child.origin.point-x;
      let y = event.origin.point-y - child.origin.point-y;

      let angle = -child.z-angle * ($PI / 180.0);
      let new-x = (x) * cos (angle) - (y) * sin (angle);
      let new-y = (x) * sin (angle) + (y) * cos (angle);

      let new-origin = make (<point>, x: new-x, y: new-y);

      //if (new-x > 50.0 & new-x < 60.0 & new-y > -5 & new-y < 5)
      if (((new-x / child.z-scale > 50.0) & (new-x / child.z-scale < 60.0))
          & (new-y > -5 & new-y < 5))
        if (instance? (event, <mouse-down-event>))
          format-out ("gripper\n");
          child.mouse-mode := #"gripper";
          event.origin := new-origin;
          //return (send-event (child, event));
          return (child);
        end;
      end;

      if (contains-point? (child, new-origin / child.z-scale))
        return (send-event (child, make (object-class (event), origin: new-origin), button));
      end;
    end;

    on-mouse-event (shape, event, 0);
    shape;
  end;
end;

// ---------------------------------------------------------------------------------------------- //

define method send-event (screen :: <screen>, event :: <mouse-event>, button :: <integer>)
  screen.mouse-origin := event.origin;
  next-method ();
end;

define method send-event (screen :: <screen>, event :: <mouse-down-event>, button :: <integer>)
  *grabbed-shape* := next-method (screen, event, button);
  *grabbed-shape*.first-mouse := event.origin - *grabbed-shape*.origin;
  //*grabbed-shape*.first-mouse := event.origin - *grabbed-shape*.screen-origin;
end;

define method send-event (screen :: <screen>, event :: <mouse-down-event>, button == $GLUT-RIGHT-BUTTON)
  *menu*.origin := event.origin - *menu*.extent / 2.0;
end;

define method send-event (screen :: <screen>, event :: <mouse-up-event>, button :: <integer>)
  *grabbed-shape*.mouse-mode := #"normal";
  on-mouse-event (*grabbed-shape*, event, 0);
  let shape = *grabbed-shape*;
  *grabbed-shape* := #f;
end;

define method send-event (screen :: <screen>, event :: <mouse-drag-event>, button :: <integer>)
  if (*grabbed-shape*.mouse-mode == #"gripper")
    //let delta = event.origin - *grabbed-shape*.origin;
    let delta = event.origin - *grabbed-shape*.screen-origin;

    block ()
      *grabbed-shape*.z-angle := atan2 (delta.point-y, delta.point-x) / ($PI / 180.0);
    exception (<error>)
      // Don't care if x and y are both 0
    end;
    *grabbed-shape*.z-scale := point-length (delta) / 50.0;
  else
    *grabbed-shape*.origin := event.origin - *grabbed-shape*.first-mouse;
  end;
  on-mouse-event (*grabbed-shape*, event, 0);
  *grabbed-shape*;
end;

define method send-event (screen :: <screen>, event :: <mouse-drag-event>, button == $GLUT-RIGHT-BUTTON)
end;

define method send-event (screen :: <screen>, event :: <mouse-up-event>, button == $GLUT-RIGHT-BUTTON)
  *menu*.origin := *screen*.origin;
end;

