Module:    dylan-user
Synopsis:  A dynamic tool for drawing circles.
Author:    Chris Double
Copyright: Copyright (c) 2001, Chris Double.  All rights reserved.

define library circle-tool
  use functional-dylan;
  use duim;
  use dynamic-draw-tool;
  use dynamic-library-protocol;

  // Add any more module exports here.
  export circle-tool;
end library circle-tool;
