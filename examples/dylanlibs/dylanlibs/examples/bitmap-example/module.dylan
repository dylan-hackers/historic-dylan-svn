Module:    dylan-user
Synopsis:  Example of reading bitmaps from files.
Author:    Chris Double
Copyright: (C) 2000, Chris Double. All rights reserved.

define module bitmap-example
  use functional-dylan;
  use duim;
  use c-ffi;
  use win32-duim;
  use win32-common, exclude: { <point> };
  use win32-user;
  use win32-gdi, exclude: { <pattern> };

  // Add binding exports here.

end module bitmap-example;
