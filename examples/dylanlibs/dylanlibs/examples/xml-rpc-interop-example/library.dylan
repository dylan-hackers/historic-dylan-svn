Module:    dylan-user
Synopsis:  Demonstrates making calls against an XML-RPC interop test server
Author:    Chris Double
Copyright: Copyright (c) 2001, Chris Double.  All rights reserved.

define library xml-rpc-interop-example
  use functional-dylan;
  use xml-rpc;
  use io;
  use double-base64;

  // Add any more module exports here.
  export xml-rpc-interop-example;
end library xml-rpc-interop-example;
