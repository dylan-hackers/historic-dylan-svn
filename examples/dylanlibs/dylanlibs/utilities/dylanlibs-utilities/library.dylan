Module:    dylan-user
Synopsis:  Various utilities that don't fit into other libraries
Author:    Chris Double
Copyright: Copyright (c) 2001, Chris Double.  All rights reserved.

define library dylanlibs-utilities
  use functional-dylan;

  export 
    dylanlibs-utilities,
    event-queue;
end library dylanlibs-utilities;
