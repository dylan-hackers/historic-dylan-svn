Module:       dylan-user
Synopsis:     The standard-io library
Author:       Gary Palter
Copyright:    Original Code is Copyright (c) 1995-1999 Harlequin Group plc.
	      All rights reserved.
License:      Harlequin Library Public License Version 1.0
Dual License: GNU Library General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library standard-io
  use harlequin-dylan;
  use streams;
  export standard-io;
end library standard-io;

define module standard-io
  use harlequin-dylan;
  use dylan-direct-c-ffi;
  use streams-internals;
  export *standard-input*, *standard-output*, *standard-error*;
end module standard-io;
