Module:       Dylan-User
Synopsis:     DUIM display device contexts
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-1999 Harlequin Group plc.
	      All rights reserved.
License:      Harlequin Library Public License Version 1.0
Dual License: GNU Library General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library duim-DCs
  use dylan;

  use duim-utilities;
  use duim-geometry;

  export duim-DCs;
  export duim-DCs-internals;
end library duim-DCs;
