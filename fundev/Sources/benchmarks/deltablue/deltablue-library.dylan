module: dylan-user
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library deltablue
  use functional-dylan;
  use system;
  use dispatch-profiler;
end library;

define module deltablue
  use functional-dylan;
  use dylan-extensions;
  use dispatch-engine;
  use simple-format;
  use operating-system;
  use dispatch-profiler;
end module;

