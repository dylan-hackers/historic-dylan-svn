Module:    Dylan-User
Author:    Andy Armstrong, Scott McKay
Synopsis:  A regression test-suite for Win32 DUIM
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library win32-duim-regression-test-suite
  use functional-dylan;

  use duim;
  use win32-duim;

  export win32-duim-regression-test-suite
end library win32-duim-regression-test-suite;
