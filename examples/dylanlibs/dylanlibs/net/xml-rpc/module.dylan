Module:    dylan-user
Synopsis:  XML-RPC routines for Dylan 
Author:    Chris Double
Copyright: (C) 2000, Chris Double.  All rights reserved.
License:   See License.txt

define module xml-rpc-internals
  use functional-dylan;
  use date;
  use print;
  use meta;
  use streams;
  use base64;

  // Add binding exports here.
  export parse-response, 
	parse-xml-decl,
	parse-method-response,
	parse-response-params,
	parse-response-param,
	parse-response-value,
	parse-response-int,
	parse-response-i4,
	parse-response-boolean,
	parse-response-string,
	parse-response-double,
	parse-response-date-time,
	parse-response-base64,
	parse-response-struct,
	parse-response-struct-member,
	parse-response-struct-member-name,
	parse-response-struct-member-value,
	parse-response-array,
	parse-response-array-data,
	parse-response-fault,
	<xml-rpc-condition>,
	xml-rpc-fault-code,
	xml-rpc-fault-string,
	parse-response-integer-value,
        encode-string,
        decode-string;
end module xml-rpc-internals;

define module xml-rpc-client
  use functional-dylan;
  use xml-rpc-internals, 
    export: { <xml-rpc-condition>, 
              xml-rpc-fault-code,
              xml-rpc-fault-string };
  use sockets;
  use streams;
  use format;
  use proxy-sockets;
  use date;

  export as-xml-rpc-type, 
    xml-rpc-send,
    <xml-rpc-value>,
    xml-rpc-encoded-value,
    xml-rpc-encoded-value-setter,
    make-xml-rpc-value,
    start-xml-rpc;
end module xml-rpc-client;
