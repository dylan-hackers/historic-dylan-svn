Module:       dylan-user
Synopsis:     A portable IO library
Author:       Gail Zacharias
Copyright:    Original Code is Copyright (c) 1998-1999 Harlequin Group plc.
	      All rights reserved.
License:      Harlequin Library Public License Version 1.0
Dual License: GNU Library General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND
Version:      $HopeName: D-io!io-library.dylan(D-kan.2) $

define library io
  use streams, export: all;
  use print, export: all;
  use standard-io, export: all;
  use format-out, export: all;
  use format, export: all;
end;
