module: dylan-user
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library dfmc-c-linker
  use functional-dylan;
  use system;
  use dfmc-core;
  use dfmc-conversion;
  use dfmc-back-end;
  use dfmc-c-back-end;
  use dfmc-linker;
  use dfmc-execution;
  use dfmc-management;

  export dfmc-c-linker;
end library;

define module dfmc-c-linker
  use functional-dylan;
  use dfmc-core;
  use dfmc-imports;
  use dfmc-conversion;
  use dfmc-back-end;
  use dfmc-c-back-end;
  use dfmc-linker;
  use dfmc-execution;
  use dfmc-management;
end module;

