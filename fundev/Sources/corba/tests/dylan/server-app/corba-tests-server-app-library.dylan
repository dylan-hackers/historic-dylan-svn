Module: dylan-user
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library corba-tests-server-app
  use functional-dylan;
  use dylan-orb;
  use corba-tests-server;
  use corba-tests-utilities;
  export corba-tests-server-app;
end library;

define module corba-tests-server-app
  use functional-dylan;
  use dylan-orb;
  use corba-tests-utilities;
end module;
