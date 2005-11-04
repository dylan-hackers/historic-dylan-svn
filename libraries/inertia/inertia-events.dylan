module:    inertia-shapes
synopsis:  Core events
author:    Mike Austin
copyright: Copyright (C) 2005 Mike L. Austin.  All rights reserved.
license:   MIT/BSD, see LICENCE.txt for details

//
// inertia-events.dylan
//

define constant $left-button = #"left-button";
define constant $middle-button = #"middle-button";
define constant $right-button = #"right-button";

define constant <mouse-button> = one-of (#"none-button", $left-button, $middle-button, $right-button);
define constant <reshape-mode> = one-of (#"none", #"move", #"size");

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

define class <mouse-enter-event> (<mouse-event>)
end;

define class <mouse-exit-event> (<mouse-event>)
end;

define class <mouse-gripper-event> (<mouse-event>)
end;

define class <parent-reshape-event> (<event>)
end;

// ---------------------------------------------------------------------------------------------- //
// event method definitions
// ---------------------------------------------------------------------------------------------- //

// send-event is called from the windows event callbacks with the topmost level shape, the *screen*,
// and is handled by classes in different ways depending on the type of shape and the type of event.

//define generic send-event (shape, event, data) => (result);

// <shape> handles converting between coordinate systems, locating the shape in the children
// hierarchy, and finally sending an on-mouse-event() to the shape.

define method send-event (shape :: <shape>, event :: <event>, data :: <object>)
 => (result :: <shape>)
  format-out ("send-event (%=, %=, %=)\n", shape, event, data);
  shape;
end;

define method send-event (shape :: <shape>, event :: <parent-reshape-event>, difference :: <point>)
 => (result :: <shape>)
  select (shape.reshape[0])
    #"move" => shape.shape-left  := shape.shape-left + difference.point-x;
    #"size" => shape.shape-width := shape.shape-width + difference.point-x;
    #"none" => ;
  end;

  select (shape.reshape[1])
    #"move" => shape.shape-top    := shape.shape-top + difference.point-y;
    #"size" => shape.shape-height := shape.shape-height + difference.point-y;
    #"none" => ;
  end;

  next-method ();
end;

define method send-event (shape :: <shape>, event :: <mouse-event>, button :: <mouse-button>)
 => (result :: <shape>)
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

    on-mouse-event (shape, event, button);
    shape;
  end;
end;

// ---------------------------------------------------------------------------------------------- //

// <screen> handles mouse grabbing and setting the mouse location in mouse-origin

define method send-event (screen :: <screen>, event :: <mouse-event>, button :: <mouse-button>)
 => (result :: <shape>)
  screen.mouse-origin := event.origin;
  next-method ();
end;

define method send-event (screen :: <screen>, event :: <mouse-motion-event>, button :: <mouse-button>)
 => (result :: <shape>)
  let shape = next-method ();
  if (screen.grabbed-shape ~= #f & shape ~= screen.grabbed-shape)
    format-out ("here\n");
    on-mouse-event (screen.grabbed-shape, make (<mouse-exit-event>, origin: event.origin), button);
    screen.grabbed-shape := shape;
    on-mouse-event (screen.grabbed-shape, make (<mouse-enter-event>, origin: event.origin), button);
  end;
  shape;
end;

define method send-event (screen :: <screen>, event :: <mouse-down-event>, button :: <mouse-button>)
 => (result :: <shape>)
  screen.grabbed-shape := next-method (screen, event, button);
  screen.grabbed-shape;
end;

define method send-event (screen :: <screen>, event :: <mouse-drag-event>, button :: <mouse-button>)
 => (result :: <shape>)
  event.origin := event.origin - screen.grabbed-shape.screen-origin;
  on-mouse-event (screen.grabbed-shape, event, button);
  screen.grabbed-shape;
end;

define method send-event (screen :: <screen>, event :: <mouse-up-event>, button :: <mouse-button>)
 => (result :: <shape>)
  on-mouse-event (screen.grabbed-shape, event, button);
  screen.grabbed-shape;
end;

