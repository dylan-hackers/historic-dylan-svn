Module:       dylan-user
Synopsis:     broadcast-calling library
Author:       Jason Trenouth
Copyright:    Original Code is Copyright (c) 1996-1999 Harlequin Group plc.
	      All rights reserved.
License:      Harlequin Library Public License Version 1.0
Dual License: GNU Library General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library channels
  use harlequin-dylan;
  use threads;
  export
	channels;
end library;

define module channels
  use harlequin-dylan;
  use threads;
  export
    <channel>,
    broadcast,
    override-channel,
    tune-in,
    tune-out;
end;
