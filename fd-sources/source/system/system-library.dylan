Module:       dylan-user
Synopsis:     Portable operating system API
Author:       Andy Armstrong
Copyright:    Original Code is Copyright (c) 1998-2000 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library system
  use date, export: all;
  use file-system, export: all;
  use operating-system, export: all;
  use locators, export: all;
  use settings, export: all;
end;
