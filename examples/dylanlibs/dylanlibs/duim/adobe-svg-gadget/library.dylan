Module:    dylan-user
Synopsis:  Gadget that hosts the Adobe SVG Control
Author:    Chris Double
Copyright: Copyright (c) 2001, Chris Double.  All rights reserved.

define library adobe-svg-gadget
  use functional-dylan;
  use duim;
  use win32-duim;
  use atl-activex-gadget;
  use adobe-svg-dispatch;

  // Add any more module exports here.
  export adobe-svg-gadget;
end library adobe-svg-gadget;
