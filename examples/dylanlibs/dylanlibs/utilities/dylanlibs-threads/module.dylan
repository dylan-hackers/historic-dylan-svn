Module:    dylan-user
Synopsis:  Threading routines used by Dylanlibs
Author:    Chris Double
Copyright: Copyright (c) 2001, Chris Double.  All rights reserved.

define module blocking-queue
  use functional-dylan;
  use threads;

  export
    <blocking-queue>,
    push-on-queue,
    get-from-queue,
    queue-empty?;
end module blocking-queue;

define module scheduler
  use functional-dylan;
  use date;
  use threads;  
  use blocking-queue;
  use win32-notifications;
  use dylanlibs-utilities;
  use format, import: { format-to-string };

  export
    <schedule>,
    schedule-items,
    schedule-abort?,
    schedule-abort?-setter,
    notify-schedule-changed,
    wait-for-schedule-changed,
    <schedule-item>,
    schedule-item-action-date,
    schedule-item-function,
    duration-until-action,
    add-schedule-item!,
    remove-schedule-item!,
    schedule-at-time,
    schedule-after-duration,
    start-schedule,
    start-schedule-on-thread,
    stop-schedule;
end module scheduler;


