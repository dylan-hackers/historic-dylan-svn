Module:    dylan-user
Author:    Andy Armstrong
Synopsis:  Windows hook
Copyright: Copyright (c) 1998-2000 Functional Objects, Inc. All rights reserved.

define module windows-hook
  use functional-dylan;
  use simple-format;

  use c-ffi;
  use win32-common;
  use win32-kernel;
  use win32-user;

  create $false, $true;

  create $message-id,
         $description-id;

  create $hook-class,
         $hook-window;

  create $application,
         $version,
         $copyright,
         $dll-name;

  export Hook-Proc;
end module windows-hook;
