Module:    dylan-user
Synopsis:  Threading routines used by Dylanlibs
Author:    Chris Double
Copyright: Copyright (c) 2001, Chris Double.  All rights reserved.

define library dylanlibs-threads
  use functional-dylan;
  use date;
  use win32-notifications;
  use dylanlibs-utilities;
  use io;

  // Add any more module exports here.
  export 
    blocking-queue,
    scheduler;
end library dylanlibs-threads;
