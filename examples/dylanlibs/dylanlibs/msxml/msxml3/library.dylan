Module:    dylan-user
Synopsis:  Dylan MSXML3 wrapper
Author:    Chris Double
Copyright: Copyright (c) 2001, Chris Double.  All rights reserved.

define library msxml3
  use functional-dylan;
  use msxml3-dispatch;
  use c-ffi;
  use com;
  use ole-automation;

  // Add any more module exports here.
  export com-utilities, msxml3;
end library msxml3;
