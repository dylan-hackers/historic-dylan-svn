module:    pentium-rtg
Synopsis:  Support for generic calling conventions
Author:    Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define sideways method return-address-on-stack?
    (be :: <pentium-back-end>) => (on-stack? :: <boolean>)
  #t
end method;

