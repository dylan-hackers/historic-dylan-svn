Module:    dylan-user
Synopsis:  Example of reading bitmaps from files.
Author:    Chris Double
Copyright: (C) 2000, Chris Double. All rights reserved.

define library bitmap-example
  use functional-dylan;
  use duim;
  use c-ffi;
  use win32-duim;
  use win32-common;
  use win32-user;
  use win32-gdi;

  // Add any more module exports here.
  export bitmap-example;
end library bitmap-example;
