Module:    dylan-user
Synopsis:  Example of using icons.
Author:    Chris Double
Copyright: (C) 2000, Chris Double.  All rights reserved.

define module icon-play
  use functional-dylan;
  use simple-format;
  use threads;
  use duim;
  use win32-duim, import: { <win32-icon> };
  // Add binding exports here.

end module icon-play;
