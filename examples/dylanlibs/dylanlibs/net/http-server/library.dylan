Module:    dylan-user
Synopsis:  Simple HTTP Server
Author:    Chris Double
Copyright: (C) 2000, Chris Double.  All rights reserved.

define library http-server
  use functional-dylan;
  use io;
  use network;
  use sequence-utilities;
  use dom-builder;

  export http-server, encode-decode;
end library http-server;
