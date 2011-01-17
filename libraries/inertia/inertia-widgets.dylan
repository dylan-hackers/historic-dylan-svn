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

define open class <widget> (<rectangle>)
  inherited slot fill-color = vector (0.95, 0.95, 0.95, 1.0);
  inherited slot line-color = vector (0.75, 0.75, 0.75, 1.0);
  inherited slot line-width = 1.0;
end;

define class <label> (<widget>)
end;


