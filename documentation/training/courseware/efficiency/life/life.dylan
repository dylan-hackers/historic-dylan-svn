Module: life
Author: Carl Gay
Synopsis: The game of Life
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


// Type life() to start.

define method life () => ()
  let frame = make(<life-frame>, title: "Life");
  start-frame(frame);
end method life;


life();

