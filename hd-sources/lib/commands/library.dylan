Module:       Dylan-User
Synopsis:     Commands library
Author:       Scott McKay, Hugh Greene
Copyright:    Original Code is Copyright (c) 1998-1999 Harlequin Group plc.
	      All rights reserved.
License:      Harlequin Library Public License Version 1.0
Dual License: GNU Library General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library commands
  use harlequin-dylan;
  use threads;

  export commands,
	 commands-internals;
end library commands;
