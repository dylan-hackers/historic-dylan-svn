Module:    dylan-user
Synopsis:  Extensions to DUIM library
Author:    Chris Double
Copyright: (C) 1999, Chris Double.  All rights reserved.

define module duim-misc
  use functional-dylan;
  use duim;
  use win32-duim;
  use win32-user, import: { SendMessage };
  use c-ffi;

  export set-extended-table-style;
end module duim-misc;
