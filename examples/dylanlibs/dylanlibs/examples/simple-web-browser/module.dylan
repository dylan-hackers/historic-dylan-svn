Module:    dylan-user
Synopsis:  Building a browser using the ATL ActiveX support
Author:    Chris Double
Copyright: (C) 2000, Chris Double.  All rights reserved.

define module simple-web-browser
  use functional-dylan, exclude: { position };
  use c-ffi;
  use duim;
  use win32-common, exclude: { <point> };
  use win32-controls;
  use win32-kernel, exclude: { beep };
  use win32-user;
  use DUIM-internals;
  use win32-duim;
  use ms-internet;
  use com;
  use ole-automation;

  // Add binding exports here.

end module simple-web-browser;
