Module:    dylan-user
Synopsis:  C-FFI wrappers for the WinInet libraries.
Author:    Chris Double
Copyright: Copyright (c) 2001, Chris Double.  All rights reserved.

define library win32-wininet
  use functional-dylan;
  use c-ffi;
  use win32-common;

  export win32-wininet;
end library win32-wininet;
