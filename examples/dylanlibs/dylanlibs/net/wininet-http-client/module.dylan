Module:    dylan-user
Synopsis:  HTTP client library that uses the WinInet as a back end.
Author:    Chris Double
Copyright: Copyright (c) 2001, Chris Double.  All rights reserved.

define module wininet-http-client
  use functional-dylan;
  use c-ffi;
  use win32-wininet;
  use win32-common;
  use threads;
  use format;

  export
    <http-session>,
    make-http-session,
    cleanup-http-session,
    \with-http-session,
    do-http-request*,
    start-http-client,
    stop-http-client;
end module wininet-http-client;
