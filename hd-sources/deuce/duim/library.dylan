Module:       Dylan-User
Synopsis:     DUIM back-end for Deuce
Author:       Scott McKay
Copyright:    Original Code is Copyright (c) 1996-1999 Harlequin Group plc.
	      All rights reserved.
License:      Harlequin Library Public License Version 1.0
Dual License: GNU Library General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library duim-deuce
  use harlequin-dylan;
  use threads;
  use collections;

  use io;

  use duim;
  use deuce;

  export duim-deuce,
	 duim-deuce-internals;
end library duim-deuce;
