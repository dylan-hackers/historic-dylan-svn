Module:    dylan-user
Synopsis:  Gadget that hosts the Adobe SVG Control
Author:    Chris Double
Copyright: Copyright (c) 2001, Chris Double.  All rights reserved.

define module adobe-svg-gadget
  use functional-dylan;
  use duim, exclude: { make-node };
  use win32-duim;
  use atl-activex-gadget;
  use adobe-svg-dispatch;

  export
    <adobe-svg-gadget>,
    adobe-svg-gadget-initialize,
    adobe-svg-gadget-uninitialize;
end module adobe-svg-gadget;
