Module:       Dylan-User
Synopsis:     The Deuce editor
Author:       Scott McKay
Copyright:    Original Code is Copyright (c) 1996-2000 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library deuce
  use common-dylan;
  use io;
  use system;
  use locators;

  export deuce-commands,
	 deuce,
	 deuce-internals;
end library deuce;
