Module:    dylan-user
Synopsis:  HTTP client routines - for reading web pages from HTTP servers.
Author:    Chris Double
Copyright: Copyright (c) 2001, Chris Double.  All rights reserved.

define module http-client
  use functional-dylan;
  use format;
  use threads;
  use c-ffi;
  use lib-curl;
  use streams;
  use finalization;

  export
    <http-session>,
    make-http-session,
    cleanup-http-session,
    \with-http-session,
    do-http-request,
    start-http-client,
    stop-http-client;
end module http-client;

