Module:       dylan-user
Synopsis:     broadcast-calling library
Author:       Jason Trenouth
Copyright:    Original Code is Copyright (c) 1996-2000 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library channels
  use functional-dylan;
  use threads;
  export
	channels;
end library;

define module channels
  use functional-dylan;
  use threads;
  export
    <channel>,
    broadcast,
    override-channel,
    tune-in,
    tune-out;
end;
