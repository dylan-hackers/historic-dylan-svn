Module:    dylan-user
Synopsis:  CAOS Injector for Creatures Adventures
Author:    Chris Double
Copyright: (C) 1999, Chris Double. All rights reserved.
License:   See License.txt

define module c3-injector
  use byte-vector;
  use finalization;
  use functional-dylan;
  use machine-word;
  use simple-format;
  use simple-random;
  use transcendentals;
  use duim;
  use c-ffi;
  use win32-common, exclude: { <point> };
  use win32-kernel, exclude: { beep };
  use rich-text-gadget;
  use c3-engine;

  // Add binding exports here.

end module c3-injector;
