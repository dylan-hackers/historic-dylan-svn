Module:       dylan-user
Synopsis:     Common Dylan library definition
Author:       Andy Armstrong
Copyright:    Original Code is Copyright (c) 1999-2000 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND
Version:      $HostName$

define library common-dylan
  use dylan,
    import: { dylan },
    export: all;
  use common-extensions, export: all;
  use machine-word, export: all;
  use byte-vector, export: all;
  use transcendentals, export: all;
  use threads, export: all;

  export common-dylan;
end library common-dylan;

define module common-dylan
  use dylan, export: all;
  use common-extensions, export: all;
end module common-dylan;
