Module:       dylan-user
Synopsis:     Collection extensions library
Author:       Andy Armstrong
Copyright:    Original Code is Copyright (c) 1998-2000 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library collections
  use bit-vector, export: all;
  use byte-vector, export: all;
  use collectors, export: all;
  use plists, export: all;
  use set, export: all;
  use bit-set, export: all;
  use table-extensions, export: all;
end;
