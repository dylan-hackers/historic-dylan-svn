Module:    dylan-user
Synopsis:  DUIM gadgets for selecting and entering dates and times
Author:    Chris Double
Copyright: (C) 1999, Chris Double.  All rights reserved.

define library date-gadgets
  use functional-dylan;
  use c-ffi;
  use duim;
  use win32-duim;
  use win32-common;
  use win32-controls;
  use win32-kernel;
  use win32-common-controls-ex;
  use win32-user;

  use date;

  // Add any more module exports here.
  export date-gadgets;
end library date-gadgets;
