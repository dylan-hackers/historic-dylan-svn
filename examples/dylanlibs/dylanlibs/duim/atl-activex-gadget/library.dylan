Module:    dylan-user
Synopsis:  A gadget that hosts ActiveX controls by using the ATL DLL
Author:    Chris Double
Copyright: Copyright (c) 2001, Chris Double.  All rights reserved.

define library atl-activex-gadget
  use functional-dylan;
  use c-ffi;
  use win32-duim;
  use win32-common;
  use win32-controls;
  use win32-kernel;
  use win32-user;
  use com;
  use ole-automation;
  use duim;


  // Add any more module exports here.
  export atl-activex-gadget;
end library atl-activex-gadget;
