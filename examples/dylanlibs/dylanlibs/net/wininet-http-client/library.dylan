Module:    dylan-user
Synopsis:  HTTP client library that uses the WinInet as a back end.
Author:    Chris Double
Copyright: Copyright (c) 2001, Chris Double.  All rights reserved.

define library wininet-http-client
  use functional-dylan;
  use io;
  use c-ffi;
  use win32-wininet;
  use win32-common;

  // Add any more module exports here.
  export wininet-http-client;
end library wininet-http-client;
