module:    inertia-shapes
synopsis:  Core events
author:    Mike Austin
copyright: Copyright (C) 2005 Mike L. Austin.  All rights reserved.
license:   MIT/BSD, see LICENCE.txt for details

//
// inertia-events.dylan
//

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

// send-event is called from the windows event callbacks with the topmost level shape, the *screen*,
// and is handled by classes in different ways depending on the type of shape and the type of event.

define generic send-event (shape :: <shape>, event :: <event>, data :: <object>) => (result :: <shape>);

// <shape> handles converting between coordinate systems, locating the shape in the children
// hierarchy, and finally sending an on-mouse-event() to the shape.

define method send-event (shape :: <shape>, event :: <mouse-event>, button :: <integer>) => (result :: <shape>)
  format-out ("send-event (%=, %=, %=)\n", shape, event, button);
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

// <screen> handles mouse grabbing and setting the mouse location in mouse-origin

define method send-event (screen :: <screen>, event :: <mouse-event>, button :: <integer>) => (result :: <shape>)
  screen.mouse-origin := event.origin;
  next-method ();
end;

define method send-event (screen :: <screen>, event :: <mouse-down-event>, button :: <integer>) => (result :: <shape>)
  screen.grabbed-shape := next-method (screen, event, button);
  screen.grabbed-shape;
end;

define method send-event (screen :: <screen>, event :: <mouse-drag-event>, button :: <integer>) => (result :: <shape>)
  event.origin := event.origin - screen.grabbed-shape.screen-origin;
  on-mouse-event (screen.grabbed-shape, event, button);
  screen.grabbed-shape;
end;

define method send-event (screen :: <screen>, event :: <mouse-up-event>, button :: <integer>) => (result :: <shape>)
  on-mouse-event (screen.grabbed-shape, event, button);
  screen.grabbed-shape;
end;

