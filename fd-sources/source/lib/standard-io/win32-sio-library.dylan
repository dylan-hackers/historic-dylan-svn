Module:       dylan-user
Synopsis:     The standard-io library
Author:       Gary Palter
Copyright:    Original Code is Copyright (c) 1995-2000 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library standard-io
  use functional-dylan;
  use streams;
  export standard-io;
end library standard-io;

define module standard-io
  use functional-dylan;
  use dylan-direct-c-ffi;
  use streams-internals;
  export *standard-input*, *standard-output*, *standard-error*;
end module standard-io;
