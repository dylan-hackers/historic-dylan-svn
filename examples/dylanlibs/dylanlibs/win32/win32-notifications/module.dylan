Module:    dylan-user
Synopsis:  Notification of system level events like system time changing
Author:    Chris Double
Copyright: Copyright (c) 2001, Chris Double.  All rights reserved.

define module win32-notifications
  use functional-dylan;
  use win32-common;
  use win32-kernel, exclude: { sleep };
  use win32-user;
  use threads;

  export
    $time-changed-lock,
    $system-time-changed,
    start-system-time-monitor,
    wait-for-time-change;
end module win32-notifications;
