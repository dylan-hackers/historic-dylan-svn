Module:    dylan-user
Synopsis:  Building a browser using the ATL ActiveX support
Author:    Chris Double
Copyright: (C) 2000, Chris Double.  All rights reserved.

define library simple-web-browser
  use functional-dylan;
  use duim;
  use c-ffi;
  use win32-duim;
  use win32-common;
  use win32-controls;
  use win32-kernel;
  use win32-user;
  use ms-internet;
  use com;
  use ole-automation;


  // Add any more module exports here.
  export simple-web-browser;
end library simple-web-browser;
