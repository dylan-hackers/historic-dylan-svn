Module:    dylan-user
Synopsis:  Interface to the Creatures 3 engine
Author:    Chris Double
Copyright: (C) 1999, Chris Double. All rights reserved.
License:   See License.txt


define library c3-engine
  use functional-dylan;
  use c-ffi;
  use win32-common;
  use win32-kernel;

  // Add any more module exports here.
  export c3-engine;
end library c3-engine;
