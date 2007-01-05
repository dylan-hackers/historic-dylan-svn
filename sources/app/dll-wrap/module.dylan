Module:    dylan-user
Synopsis:  Wrapper for executing .dll projects.
Author:    1998/7/31 Seth LaForge
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module dll-wrap
  use win32-kernel;
  use format-out;
  use format;
  use finalization;
  use functional-dylan;
  use simple-random;
  use operating-system;
end module dll-wrap;