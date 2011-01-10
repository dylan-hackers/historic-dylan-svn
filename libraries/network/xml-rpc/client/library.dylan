Module:    dylan-user
Synopsis:  XML-RPC client
Author:    Carl Gay
Copyright: (C) 2002, Carl L Gay.  All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:  Distributed WITHOUT WARRANTY OF ANY KIND

define library xml-rpc-client
  use common-dylan;
  use http-client;
  use http-common;
  use io;
  use network;
  use uncommon-dylan;
  use uri;
  use xml-parser;
  use xml-rpc-common;

  export xml-rpc-client;
end;


define module xml-rpc-client
  use common-dylan, exclude: { format-to-string };
  use format;
  use format-out;  // for debugging only
  use http-client;
  use http-common;
  use sockets;
  use streams;
  use uncommon-dylan;
  use uri;
  use xml-parser,
    prefix: "xml$";
  use xml-rpc-common,
    export: {
      <xml-rpc-error>, <xml-rpc-parse-error>,
      <xml-rpc-fault>, xml-rpc-fault,
      fault-code,
      base64-encode, base64-decode,
    };

  export
    xml-rpc-call;
end;
