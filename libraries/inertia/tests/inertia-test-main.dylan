module:    inertia-test
synopsis:  Core UI shapes
author:    Mike Austin
copyright: Copyright (C) 2005 Mike L. Austin.  All rights reserved.
license:   MIT/BSD, see LICENCE.txt for details

//
// inertia-test.dylan
//

define method make-from-string (string == #"<polygon>", #rest args) => (object :: <object>)
  apply (make, <polygon>, args);
end;

begin
  format-out ("%=\n", make-from-string (as(<symbol>, "<polygon>")));
  
  glutMainLoop ();
end;


define class <ship> (<object>)
  slot state = #"exploding";
end;

define method draw-actor (ship :: <ship>, state == #"normal") => ()
  format-out ("normal");
end;

define method draw-actor (ship :: <ship>, state == #"exploding") => ()
  format-out ("exploding");
end;

let ship = make (<ship>);
draw-actor (ship, ship.state);

