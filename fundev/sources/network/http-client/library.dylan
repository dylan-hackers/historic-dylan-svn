Module:    dylan-user
Synopsis:  Thin wrapper around http
Author:    Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library http-client
  use functional-dylan;
  use io;
  use network;

  export http-client;
end library http-client;