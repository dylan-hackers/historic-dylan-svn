Module:    dylan-user
Synopsis:  A gadget that displays an instance of Microsoft Internet Explorer
Author:    Chris Double
Copyright: Copyright (c) 2001, Chris Double.  All rights reserved.

define module msie-gadget
  use functional-dylan;
  use duim;
  use win32-duim;
  use atl-activex-gadget;
  use ms-internet;

  export
    <msie-gadget>,
    msie-gadget-initialize,
    msie-gadget-uninitialize;
end module msie-gadget;
