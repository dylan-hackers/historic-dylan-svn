Module:    dylan-user
Synopsis:  XML-RPC routines for Dylan 
Author:    Chris Double
Copyright: (C) 2000, Chris Double.  All rights reserved.
License:   See License.txt

define module xml-rpc-internals
  use functional-dylan;
  use date;
  use print;
  use streams;
  use base64;
  use msxml3;

  // Add binding exports here.
  export parse-response,
         encode-string,
         decode-string,         
	<xml-rpc-condition>,
	xml-rpc-fault-code,
	xml-rpc-fault-string;
end module xml-rpc-internals;

define module xml-rpc-client
  use functional-dylan;
  use base64;
  use xml-rpc-internals, 
    export: { <xml-rpc-condition>, 
              xml-rpc-fault-code,
              xml-rpc-fault-string };
  use sockets;
  use streams;
  use format;
  use msxml3;
  use proxy-sockets;
  use date;

  export 
    as-xml-rpc-type, 
    xml-rpc-send,
    <xml-rpc-value>,
    xml-rpc-encoded-value,
    xml-rpc-encoded-value-setter,
    make-xml-rpc-value,
    start-xml-rpc;
end module xml-rpc-client;
