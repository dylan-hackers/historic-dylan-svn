module: dylan-user
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Copyright 1996 Functional Objects, Inc.  All rights reserved.

define library example-plugin
  use dylan;
  use c-ffi;
  use win32-common;
  use win32-user;
  use netscape-plugin;
  export example-plugin;
end;

define module example-plugin
  use dylan;
  use c-ffi;
  use win32-common;
  use win32-user;
  use netscape-plugin;
end;
  
