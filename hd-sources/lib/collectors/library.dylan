Module:       dylan-user
Synopsis:     The collectors library and its modules
Author:       Keith Playford
Copyright:    Original Code is Copyright (c) 1996-1999 Harlequin Group plc.
	      All rights reserved.
License:      Harlequin Library Public License Version 1.0
Dual License: GNU Library General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library collectors
  use harlequin-dylan;
  export collectors;
end library;

define module collectors
  use harlequin-dylan;
  use dylan-extensions,
    import: { <limited-sequence-type> };
  export
    collector-protocol,
    \collecting, 
    \collect-into, \collect-first-into, \collect-last-into,
    \collect, \collect-first, \collect-last,
    \collected;
  export
    assert-collector-protocol-ok;
end module;

// eof
