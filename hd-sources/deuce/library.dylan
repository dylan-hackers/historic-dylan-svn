Module:       Dylan-User
Synopsis:     The Deuce editor
Author:       Scott McKay
Copyright:    Original Code is Copyright (c) 1996-1999 Harlequin Group plc.
	      All rights reserved.
License:      Harlequin Library Public License Version 1.0
Dual License: GNU Library General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library deuce
  use harlequin-dylan;
  use threads;
  use collections;
  use io;
  use system;
  use locators;

  export deuce-commands,
	 deuce,
	 deuce-internals;
end library deuce;
