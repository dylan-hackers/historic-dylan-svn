Module:    dylan-user
Synopsis:  A gadget that displays an instance of Microsoft Internet Explorer
Author:    Chris Double
Copyright: Copyright (c) 2001, Chris Double.  All rights reserved.

define library msie-gadget
  use functional-dylan;
  use duim;
  use win32-duim;
  use atl-activex-gadget;
  use ms-internet;


  // Add any more module exports here.
  export msie-gadget;
end library msie-gadget;
