Module:    dylan-user
Synopsis:  HTTP client routines - for reading web pages from HTTP servers.
Author:    Chris Double
Copyright: Copyright (c) 2001, Chris Double.  All rights reserved.

define library http-client
  use functional-dylan;
  use io;
  use lib-curl;
  use c-ffi; 

  // Add any more module exports here.
  export http-client;
end library http-client;
