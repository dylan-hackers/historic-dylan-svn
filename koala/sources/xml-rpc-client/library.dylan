Module:    dylan-user
Synopsis:  XML-RPC client
Author:    Carl Gay
Copyright: (C) 2002, Carl L Gay.  All rights reserved.
License:   Functional Objects Library Public License Version 1.0
Warranty:  Distributed WITHOUT WARRANTY OF ANY KIND

define library xml-rpc-client
  use functional-dylan;
  use io;
  use network;
  use xml-parser;
  use xml-rpc-common;
  export xml-rpc-client;
end;


define module xml-rpc-client
  use functional-dylan;
  use format;
  use format-out;  // for debugging only
  use sockets;
  use streams;
  use xml-parser,
    prefix: "xml$";
  use xml-rpc-common,
    export: { <xml-rpc-error>, <xml-rpc-parse-error>, <xml-rpc-fault> };
  export
    xml-rpc-call,      // standard interface
    xml-rpc-call-2;    // accepts port and url arguments
end;


