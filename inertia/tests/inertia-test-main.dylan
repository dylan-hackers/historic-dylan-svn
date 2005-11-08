module:    inertia-test
synopsis:  Inertia test program
author:    Mike Austin
copyright: Copyright (C) 2005 Mike L. Austin.  All rights reserved.
license:   MIT/BSD, see LICENCE.txt for details

//
// inertia-test.dylan
//

define method make-from-string (string == #"<polygon>", #rest args) => (object :: <object>)
  apply (make, <polygon>, args);
end;

define method xadd-child (window :: <window>)
  curry (<window>_xadd-child, window);
end;

define method <window>_xadd-child (window :: <window>, child :: <shape>)
end;

// -------------------------------------------------------------------------- //
// inertia-test class definitions
// -------------------------------------------------------------------------- //

define class <clock> (<widget>)
  inherited slot fill-color = vector (1.0, 1.0, 1.0, 1.0);
  inherited slot reshape = #[#"size", #"size"];
end;

define method initialize (clock :: <clock>, #rest init-args, #key) => ()
  next-method ();
  glutTimerFunc (1000, clock-timer, 1000);
end;

define variable clock-timer = callback-method (n :: <integer>) => ();
  glutTimerFunc (n, clock-timer, n);
  glutPostRedisplay ();
end;

// -------------------------------------------------------------------------- //
// clock methods definitions
// -------------------------------------------------------------------------- //

define method \/ (a :: <integer>, b :: <integer>)
  as(<double-float>, a) / b;
end;

define constant hours-in-radians = (1 / 60) * (1 / 60) * ($double-pi * 2 / 12);
define constant minutes-in-radians = (1 / 60) * ($double-pi * 2 / 60);
define constant seconds-in-radians = ($double-pi * 2 / 60);

define method draw-overlay (shape :: <shape>, clock :: <clock>)
  let time = get-decoded-time (timezone: -8 * 60 * 60);
  let size = min (shape.shape-width, shape.shape-height) / 2.0;
  let ratio = $double-pi * 2 / 60.0;
  let secs = modulo (time.hours, 12) * 60 * 60 + time.minutes * 60 + time.seconds;
  glPushMatrix ();
  glTranslate (shape.shape-width / 2.0, shape.shape-height / 2.0, 0.0);
  glRotate (-90.0, 0.0, 0.0, 1.0);

  glColor (0.3, 0.3, 0.3);
  with-glBegin ($GL-LINES)
    for (tick from 0 below 12)
      glVertex (cos (tick * ratio * 5) * (size - 6.0),
                sin (tick * ratio * 5) * (size - 6.0));
      glVertex (cos (tick * ratio * 5) * (size - 3.0),
                sin (tick * ratio * 5) * (size - 3.0));
    end;
  end;

  glColor (0.3, 0.3, 0.3);
  with-glBegin ($GL-LINES)
    glVertex (0.0, 0.0);
    glVertex (cos (secs * hours-in-radians) * (size * 0.6),
              sin (secs * hours-in-radians) * (size * 0.6));
    glVertex (0.0, 0.0);
    glVertex (cos (secs * minutes-in-radians) * (size - 9.0),
              sin (secs * minutes-in-radians) * (size - 9.0));
    glColor (0.7, 0.7, 0.7);
    glVertex (0.0, 0.0);
    glVertex (cos (secs * seconds-in-radians) * (size - 3.0),
              sin (secs * seconds-in-radians) * (size - 3.0));
  end;
  glPopMatrix ();
end;

define method button-clicked (button :: <push-button>)
end;

*function-table*[#"button-clicked"] := button-clicked;

begin
  define variable *editor* = make (<shape-editor>, left: 10.0, top: 10.0,
                                                   width: 580.0, height: 380.0,
                                                   reshape: #[#"size", #"size"]);
  add-child (*editor*, make (<polygon>, left: 50.0, top: 50.0));
  add-child (*editor*, make (<spinning-polygon>, left: 150.0, top: 150.0));
  add-child (*editor*, make (<rectangle>, left: 250.0, top: 250.0));
  add-child (*editor*, make (<push-button>, caption: "Another"));

  define variable *window* = make (<window>, caption: "Window 1",
                                             left: 50.0, top: 50.0,
                                             width: 600.0, height: 400.0);
  add-child (*window*, *editor*);
  add-child (*window*, make (<push-button>, caption: "Press Me",
                             reshape: #[#"move", #"move"], left: 190.0, top: 165.0));
  add-child (*screen*, make (<window>, caption: "Window 2", left: 500.0, top: 300.0));
  add-child (*screen*, make (<window>, client: make (<clock>), angle: 12.0,
                             caption: "Clock", left: 700.0, top: 50.0));
  add-child (*screen*, *window*);

  build-object (*screen*, "layout.ui");

  format-out ("%=\n", *function-table*[#"button"].on-click);

  glutMainLoop ();
end;

