Module:    dylan-user
Synopsis:  Notification of system level events like system time changing
Author:    Chris Double
Copyright: Copyright (c) 2001, Chris Double.  All rights reserved.

define library win32-notifications
  use functional-dylan;
  use win32-common;
  use win32-kernel;
  use win32-user;


  // Add any more module exports here.
  export win32-notifications;
end library win32-notifications;
