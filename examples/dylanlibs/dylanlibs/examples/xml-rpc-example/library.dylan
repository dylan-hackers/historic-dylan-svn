Module:    dylan-user
Synopsis:  An example of using xml-rpc
Author:    Chris Double
Copyright: (C) 2000, Chris Double.  All rights reserved.
License:   See License.txt

define library xml-rpc-example
  use functional-dylan;
  use proxy-sockets;
  use xml-rpc;
  use io;

  // Add any more module exports here.
  export xml-rpc-example;
end library xml-rpc-example;
