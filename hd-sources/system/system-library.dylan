Module:       dylan-user
Synopsis:     Portable operating system API
Author:       Andy Armstrong
Copyright:    Original Code is Copyright (c) 1998-1999 Harlequin Group plc.
              All rights reserved.
License:      Harlequin Library Public License Version 1.0
Dual License: GNU Library General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library system
  use date, export: all;
  use file-system, export: all;
  use operating-system, export: all;
  use locators, export: all;
  use settings, export: all;
end;
