Module:       Dylan-User
Synopsis:     DUIM core + Vanilla back-end
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-1999 Harlequin Group plc.
	      All rights reserved.
License:      Harlequin Library Public License Version 1.0
Dual License: GNU Library General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library duim
  use dylan;

  // Use the DUIM core and re-export all its modules,
  // then add the Vanilla back-end
  use duim-core, export: all;
  use vanilla-duim;
end library duim;
