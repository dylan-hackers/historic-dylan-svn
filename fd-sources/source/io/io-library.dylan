Module:       dylan-user
Synopsis:     A portable IO library
Author:       Gail Zacharias
Copyright:    Original Code is Copyright (c) 1998-2000 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library io
  use streams, export: all;
  use print, export: all;
  use standard-io, export: all;
  use format-out, export: all;
  use format, export: all;
end;
