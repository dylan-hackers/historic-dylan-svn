Module:    dylan-user
Synopsis:  DUIM gadgets for selecting and entering dates and times
Author:    Chris Double
Copyright: (C) 1999, Chris Double.  All rights reserved.

define module date-gadgets
  use functional-dylan, exclude: { position };
  use c-ffi;
  use duim;
  use win32-common, exclude: { <point> };
  use win32-controls;
  use win32-kernel, exclude: { beep };
  use win32-user;
  use DUIM-internals;
  use win32-duim;
  use win32-common-controls-ex;
  use date;

  // Add binding exports here.
  export <date-gadget>,
         <date-selection-field>,
         $no-date-entered;
  
end module date-gadgets;
