module:    dylan-user
Synopsis:  The module definition for the LINUX-PENTIUM-RTG library
Author:    Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND



define module linux-pentium-rtg
  use dylan;
  use functional-extensions;
  use streams;
  use format;
  use print;
  use locators;
  use dfmc-back-end-protocol;
  use harp;
  use pentium-harp, export: { <linux-pentium-back-end> };
  use pentium-rtg, export: all;
  use threads;
end module;
