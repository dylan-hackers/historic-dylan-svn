Module:    dylan-user
Synopsis:  CAOS Injector for Creatures Adventures
Author:    Chris Double
Copyright: (C) 1999, Chris Double. All rights reserved.
License:   See License.txt

define library c3-injector
  use functional-dylan;
  use duim;
  use c-ffi;
  use win32-common;
  use win32-kernel;
  use double-rich-text-gadget;
  use c3-engine;

  // Add any more module exports here.
  export c3-injector;
end library c3-injector;
