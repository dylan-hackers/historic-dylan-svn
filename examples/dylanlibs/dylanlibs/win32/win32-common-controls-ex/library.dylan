Module:    dylan-user
Synopsis:  Additional Win32 Common Control functionality
Author:    Chris Double
Copyright: (C) 1999, Chris Double.  All rights reserved.

define library win32-common-controls-ex
  use dylan;
  use c-ffi;
  use win32-kernel;
  use win32-common;
  use win32-controls;

  // Add any more module exports here.
  export win32-common-controls-ex;
end library win32-common-controls-ex;
