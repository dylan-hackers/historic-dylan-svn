Module:    time-change-notification-example
Synopsis:  Demonstrates receving notification of system time changes
Author:    Chris Double
Copyright: Copyright (c) 2001, Chris Double.  All rights reserved.

define method main () => ()
  start-system-time-monitor();
  format-out("Starting...\n");
  if(wait-for-time-change(timeout: 10))
    format-out("Time changed.\n");
  else
    format-out("Time not changed.\n");
  end if;

  if(wait-for-time-change(timeout: 10))
    format-out("Time changed again.\n");
  else
    format-out("Time not changed.\n");
  end if; 
end method main;

begin
  main();
end;
