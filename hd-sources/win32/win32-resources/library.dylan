Module:    dylan-user
Synopsis:  Windows resource decoding
Author:    Roman Budzianowski, Scott McKay
Copyright: 1996-1998 Harlequin Group plc.  All rights reserved.

define library win32-resources
  use harlequin-dylan;
  use threads;
  use collections;
  use C-FFI;
  use win32-common;
  use win32-user;
  use win32-gdi;
  use win32-kernel;

  export win32-resources;
end library win32-resources;
