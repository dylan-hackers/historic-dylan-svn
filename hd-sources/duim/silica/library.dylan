Module:       Dylan-User
Synopsis:     DUIM sheets
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-1999 Harlequin Group plc.
	      All rights reserved.
License:      Harlequin Library Public License Version 1.0
Dual License: GNU Library General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library duim-sheets
  use dylan;

  use duim-utilities;
  use duim-geometry;
  use duim-DCs;

  export duim-sheets;
  export duim-sheets-internals;
end library duim-sheets;
