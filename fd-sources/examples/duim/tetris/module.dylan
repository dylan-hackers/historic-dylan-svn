Module:       dylan-user
Synopsis:     DUIM implementation of the game Tetris
Author:       Richard Tucker
Copyright:    Original Code is Copyright (c) 1998-2000 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module tetris
  use operating-system;
  use streams;
  use standard-io;
  use print;
  use format-out;
  use format;
  use duim;
  use threads;
  use table-extensions;
  use machine-integer-user;
  use finalization;
  use functional-dylan;
  use simple-random;
end module tetris;
