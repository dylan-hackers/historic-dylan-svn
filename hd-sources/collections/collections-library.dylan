Module:       dylan-user
Synopsis:     Collection extensions library
Author:       Andy Armstrong
Copyright:    Original Code is Copyright (c) 1998-1999 Harlequin Group plc.
	      All rights reserved.
License:      Harlequin Library Public License Version 1.0
Dual License: GNU Library General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library collections
  use bit-vector, export: all;
  use byte-vector, export: all;
  use collectors, export: all;
  use plists, export: all;
  use set, export: all;
  use bit-set, export: all;
  use table-extensions, export: all;
end;
