Module:    dylan-user
Synopsis:  Drawing program that loads Dylan functionality at run time
Author:    Chris Double
Copyright: Copyright (c) 2001, Chris Double.  All rights reserved.

define library dynamic-draw
  use functional-dylan;
  use duim;
  use dynamic-library-protocol;
  use dynamic-draw-tool;

  // Add any more module exports here.
  export dynamic-draw;
end library dynamic-draw;
