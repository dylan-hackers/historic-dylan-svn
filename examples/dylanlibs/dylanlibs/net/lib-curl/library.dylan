Module:    dylan-user
Synopsis:  An FFI wrapper for Curl.
Author:    Chris Double
Copyright: Copyright (c) 2001, Chris Double.  All rights reserved.

define library lib-curl
  use functional-dylan;
  use c-ffi;
  use network;

  // Add any more module exports here.
  export lib-curl;
end library lib-curl;
