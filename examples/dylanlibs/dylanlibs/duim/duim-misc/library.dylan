Module:    dylan-user
Synopsis:  Extensions to DUIM library
Author:    Chris Double
Copyright: (C) 1999, Chris Double.  All rights reserved.

define library duim-misc
  use functional-dylan;
  use duim;
  use win32-duim;
  use win32-user;
  use c-ffi;

  export duim-misc;
end library duim-misc;
