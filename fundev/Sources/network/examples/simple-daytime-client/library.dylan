Module:       dylan-user
Author:       Toby Weinberg, Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library daytime-client
  use functional-dylan;
  use io;
  use system;
  use network;
  export daytime-client;
end library;

define module daytime-client
  use functional-dylan;
  use streams;
  use standard-io;
  use format;
  use format-out;
  use date;
  use sockets;
end module;
