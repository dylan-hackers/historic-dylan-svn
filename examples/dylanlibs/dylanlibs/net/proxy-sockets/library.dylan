Module:    dylan-user
Synopsis:  Support for sockets through a proxy server
Author:    Chris Double
Copyright: (C) 2000, Chris Double.  All rights reserved.
License:   See license.txt

define library proxy-sockets
  use functional-dylan;
  use io;
  use x-sockets;

  // Add any more module exports here.
  export proxy-sockets;
end library proxy-sockets;
