Module:    dylan-user
Synopsis:  Various utilities that don't fit into other libraries
Author:    Chris Double
Copyright: Copyright (c) 2001, Chris Double.  All rights reserved.

define library dylanlibs-utilities
  use functional-dylan;
  use io;
  use date;

  export 
    dylanlibs-utilities,
    logging;
end library dylanlibs-utilities;
