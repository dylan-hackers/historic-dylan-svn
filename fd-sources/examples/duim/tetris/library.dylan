Module:       dylan-user
Synopsis:     DUIM implementation of the game Tetris
Author:       Richard Tucker
Copyright:    Original Code is Copyright (c) 1998-2000 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library tetris
  use system;
  use io;
  use duim;
  use threads;
  use collections;
  use functional-dylan;

  export tetris;
end library tetris;
