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

// A module to handle logging of information to a file.
// Simple, but useful and to be expanded in the future
// to cater for levels of logging, etc.
define module logging
  use functional-dylan;
  use streams;
  use format;
  use standard-io;
  use date;
  use threads;

  export
    *log-directory*,
    *log-system-name*,
    *logging-enabled*,
    \with-logging-system,
    \without-logging,
    \with-logging,
    \log-block,
    \log-block-out,
    bind-logging-vars,
    log-message,
    log-message-out;
end module logging;

