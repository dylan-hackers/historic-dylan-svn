Module:    dylan-user
Synopsis:  Protocol for supporting loading of dynamic Dylan libraries
Author:    Chris Double
Copyright: Copyright (c) 2001, Chris Double.  All rights reserved.

define library dynamic-library-protocol
  use functional-dylan;
  use win32-kernel;

  export dynamic-library-protocol;
end library dynamic-library-protocol;
