Module:       Dylan-User
Synopsis:     DUIM concrete gadget panes
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-1999 Harlequin Group plc.
	      All rights reserved.
License:      Harlequin Library Public License Version 1.0
Dual License: GNU Library General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library duim-gadget-panes
  use dylan;

  use duim-utilities;
  use duim-geometry;
  use duim-DCs;
  use duim-sheets;
  use duim-graphics;  
  use duim-layouts;
  use duim-gadgets;
  use duim-frames;

  export duim-gadget-panes;
  export duim-gadget-panes-internals;
end library duim-gadget-panes;
