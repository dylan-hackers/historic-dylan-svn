Module:    dylan-user
Synopsis:  Library to send keystrokes to the keyboard buffer
Author:    Chris Double
Copyright: (C) 2000, Chris Double.  All rights reserved.

define library send-keys
  use functional-dylan;
  use c-ffi;
  use win32-common;
  use win32-kernel;
  use win32-user;

  // Add any more module exports here.
  export send-keys-internal, send-keys;
end library send-keys;
