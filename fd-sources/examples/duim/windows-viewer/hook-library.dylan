Module:    dylan-user
Author:    Andy Armstrong
Synopsis:  Windows hook
Copyright: Copyright (c) 1998-2000 Functional Objects, Inc. All rights reserved.

define library windows-hook
  use functional-dylan;

  use c-ffi;
  use win32-common;
  use win32-kernel;
  use win32-user;

  export windows-hook;
end library windows-hook;
