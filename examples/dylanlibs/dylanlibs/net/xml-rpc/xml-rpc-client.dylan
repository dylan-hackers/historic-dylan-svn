Module:    xml-rpc-client
Synopsis:  XML-RPC routines for Dylan 
Author:    Chris Double
Copyright: (C) 2000, Chris Double.  All rights reserved.
License:   See License.txt

define class <xml-rpc-value> (<object>)
  slot xml-rpc-encoded-value :: <string>, init-keyword: value:;
end class <xml-rpc-value>;

define method make-xml-rpc-value(tag :: <string>, value :: <string>)
  make(<xml-rpc-value>, value: format-to-string("<%s>%s</%s>", tag, value, tag));
end method make-xml-rpc-value;

// Convert a Dylan object to an XML representation to be used in XML-RPC.
// Arguments to XML-RPC methods use this generic function to convert the
// argument types to the correct XML. For an argument to be able to be passed
// to an XML-RPC method it must have a specialised method added to this
// generic.
define open generic as-xml-rpc-type(value :: <object>) => (s :: <string>);

define method as-xml-rpc-type(value :: <xml-rpc-value>) => (s :: <string>)
  value.xml-rpc-encoded-value;
end method as-xml-rpc-type;

define method as-xml-rpc-type(value :: <integer>) => (s :: <string>)
  format-to-string("<i4>%d</i4>", value);
end method as-xml-rpc-type;

define method as-xml-rpc-type(value :: <boolean>) => (s :: <string>)
  format-to-string("<boolean>%c</boolean>", if(value) '1' else '0' end);
end method as-xml-rpc-type;

define method as-xml-rpc-type(value :: <string>) => (s :: <string>)
  format-to-string("<string>%s</string>", encode-string(value));                     
end method as-xml-rpc-type;

define method xml-float-to-string(value :: <float>) => (s :: <string>)
  let s = format-to-string("%d", value);
  let dp = subsequence-position(s, ".");
  let tp = subsequence-position(s, "d") | subsequence-position(s, "s");
  let lhs = copy-sequence(s, end: dp);
  let rhs = copy-sequence(s, start: dp + 1, end: tp);
  let shift = string-to-integer(s, start: tp + 1);
  let result = "";
  if(shift <= 0)
    result := concatenate(result, lhs, ".");
    for(n from 0 below abs(shift))
      result := concatenate(result, "0");
    end for;
    result := concatenate(result, rhs);
  else
    result := concatenate(result, lhs);
    if(rhs.size < shift)
      result := concatenate(result, rhs);
      for(n from 0 below shift - rhs.size)
        result := concatenate(result, "0");
      end for;
    else
      error("Error parsing float");
    end if;
  end if;
  result;    
end method xml-float-to-string;

define method as-xml-rpc-type(value :: <float>) => (s :: <string>)
  format-to-string("<double>%s</double>", xml-float-to-string(value));                     
end method as-xml-rpc-type;

define method as-xml-rpc-type(value :: <date>) => (s :: <string>)
  format-to-string("<dateTime.is8601>%s</dateTime.is8601>", as-iso8601-string(value));
end method as-xml-rpc-type;

define method as-xml-rpc-type(value :: <table>) => (s :: <string>)
  let start = "<struct>";
  let finish = "</struct>";
  let members = "";
  for(v keyed-by k in value)
    members := concatenate(members,
                           format-to-string("<member><name>%s</name><value>%s</value></member>",
                                            k,
                                            as-xml-rpc-type(v)));
  end for;
  concatenate(start, members, finish);
end method as-xml-rpc-type;

define method as-xml-rpc-type(value :: <sequence>) => (s :: <string>)
  let start = "<array><data>";
  let finish = "</data></array>";
  let values = "";
  for(v  in value)
    values := concatenate(values,                          
                          format-to-string("<value>%s</value>",
                                           as-xml-rpc-type(v)));
  end for;
  concatenate(start, values, finish);
end method as-xml-rpc-type;


define method build-method-call(rpc-method :: <string>, args :: <sequence>) => (s :: <string>)
  let request = format-to-string(
    "<?xml version='1.0'?>"
    "<methodCall>"
    "<methodName>%s</methodName>", rpc-method);

  unless(zero?(args.size))
    request := concatenate(request, "<params>");
    for(arg in args)
      request := concatenate(request, 
      format-to-string(
        "<param><value>%s</value></param>",
                       as-xml-rpc-type(arg)));
    end for;
    request := concatenate(request, "</params>");
  end unless;

  concatenate(request, "</methodCall>");
end method build-method-call;

define constant $content-length-header = "Content-Length:";

define method send-method-call(host, port, url, method-call) => (s :: <string>)
  let s = make-proxy-socket(host: host, port: port);
  block()
    write(s, format-to-string("POST %s HTTP/1.0\r\n", url));
    write(s, "User-Agent: Dylan XML-RPC Client\r\n");
    write(s, format-to-string("Host: %s\r\n", host));
    write(s, "Pragma: no-cache\r\n");
    write(s, "Content-Type: text/xml\r\n");
    write(s, format-to-string("Content-Length: %d\r\n", method-call.size));
    write(s, "\r\n");
    write(s, method-call);
    force-output(s);
	
    // Process headers
    let content-length = #f;
    for(line = read-line(s, on-end-of-stream: #f) 
        then read-line(s, on-end-of-stream: #f),
        until: ~line | zero?(line.size))
      let cl-pos = subsequence-position(line, $content-length-header);
      when(cl-pos)
        content-length := string-to-integer(line, start: cl-pos + $content-length-header.size);
      end when;	    
    end for;

    // collect and return response.
    let result = make(<string>, size: content-length);
    for(index from 0 below content-length)
      result[index] := read-element(s);
    end for;
    result;
  cleanup
    close(s);
  end block;    
end method send-method-call;

// Calls an XML-RPC method on a server.
define method xml-rpc-send(host, port, url, rpc-method :: <string>, #rest args)
  let method-call = build-method-call(rpc-method, args);
  let method-response = send-method-call(host, port, url, method-call);
  let (index, parsed-response) = parse-response(method-response);
  parsed-response;
end method xml-rpc-send;

// Must be called by client libraries to initialize sockets.
define method start-xml-rpc()
  start-sockets();
end method start-xml-rpc;

