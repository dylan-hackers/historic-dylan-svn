Module:    dylan-user
Synopsis:  NNTP Protocol implementation
Author:    Chris Double
Copyright: Copyright (c) 2001, Chris Double.  All rights reserved.

define library nntp
  use functional-dylan;
  use io;
  use network;
  use date;
  use sequence-utilities;

  // Add any more module exports here.
  export nntp;
end library nntp;
