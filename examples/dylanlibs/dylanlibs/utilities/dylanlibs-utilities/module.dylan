Module:    dylan-user
Synopsis:  Various utilities that don't fit into other libraries
Author:    Chris Double
Copyright: Copyright (c) 2001, Chris Double.  All rights reserved.

define module dylanlibs-utilities
  use functional-dylan;

  // Add binding exports here.
  export
    formatted-string-to-float,
    float-to-formatted-string,
    \with-abort-handler,
    \with-restart-block-handler,
    \inc!,
    \dec!;
end module dylanlibs-utilities;

define module event-queue
  use functional-dylan;
  use threads;

  // Add binding exports here.
  export
    <event-queue>,
    push-on-queue,
    get-from-queue,
    queue-empty?;
end module event-queue;


