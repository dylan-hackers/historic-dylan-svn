Module:       dylan-user
Synopsis:     The collectors library and its modules
Author:       Keith Playford
Copyright:    Original Code is Copyright (c) 1996-2000 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library collectors
  use functional-dylan;
  export collectors;
end library;

define module collectors
  use functional-dylan;
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
