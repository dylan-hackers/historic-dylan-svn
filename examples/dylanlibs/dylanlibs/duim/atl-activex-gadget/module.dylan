Module:    dylan-user
Synopsis:  A gadget that hosts ActiveX controls by using the ATL DLL
Author:    Chris Double
Copyright: Copyright (c) 2001, Chris Double.  All rights reserved.

define module atl-activex-gadget
  use functional-dylan, exclude: { position };
  use c-ffi;
  use duim;
  use win32-common, exclude: { <point> };
  use win32-controls;
  use win32-kernel, exclude: { beep };
  use win32-user;
  use DUIM-internals;
  use win32-duim;
  use com;
  use ole-automation;

  export
    atl-activex-gadget-initialize,
    atl-activex-gadget-uninitialize,
    <atl-activex-gadget>,
    atl-activex-class-id,
    atl-activex-interface-class,    
    atl-activex-interface;
end module atl-activex-gadget;


