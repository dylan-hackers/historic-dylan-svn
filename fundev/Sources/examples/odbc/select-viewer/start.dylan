Module:   select-viewer
Synopsis: A simple sql query viewer
Author:   Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define method main () => ()
  spawn-select-viewer();
  wait-for-shutdown();
end method main;

begin
  main();
end;

// eof

