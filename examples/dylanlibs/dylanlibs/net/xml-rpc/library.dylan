Module:    dylan-user
Synopsis:  XML-RPC routines for Dylan 
Author:    Chris Double
Copyright: (C) 2000, Chris Double.  All rights reserved.
License:   See License.txt

define library xml-rpc
  use functional-dylan;
  use date;
  use meta;
  use io;
  use x-sockets;
  use proxy-sockets;
  use double-base64;

  export xml-rpc-internals, 
    xml-rpc-client;
end library xml-rpc;
