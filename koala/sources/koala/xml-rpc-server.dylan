Module:    httpi
Synopsis:  XML-RPC server
Author:    Carl Gay
Copyright: Copyright (c) 2001-2002 Carl L. Gay.  All rights reserved.
License:   Functional Objects Library Public License Version 1.0
Warranty:  Distributed WITHOUT WARRANTY OF ANY KIND


// Determines whether the server will respond to XML-RPC requests.
// This variable is configurable in koala-config.xml
//
define variable *xml-rpc-enabled?* :: <boolean> = #t;

// This variable is configurable in koala-config.xml
//
define variable *xml-rpc-server-url* :: <string> = "/RPC2";

// This is the fault code that will be returned to the caller if
// any error other than <xml-rpc-fault> is thrown during the execution
// of the RPC.  For example, if there's a parse error in the XML
// that's received.  If users want to return a fault code they
// should use the xml-rpc-fault method.
//
// This variable is configurable in koala-config.xml
//
define variable *xml-rpc-internal-error-fault-code* :: <integer> = 0;

// ---TODO: Shouldn't really even register the responder if XML-RPC is disabled.
//
define responder respond-to-xml-rpc-request (*xml-rpc-server-url*)
    (request :: <request>, response :: <response>)
  when (*xml-rpc-enabled?*)
    // All responses start with a valid XML document header.
    write(output-stream(response),
          "<?xml version=\"1.0\" encoding=\"iso-8859-1\" ?>");
    block ()
      let xml = request-content(request);
      when (*debugging-xml-rpc*)
        format-out("\nReceived XML-RPC call:\n%s\n", xml);
      end;
      let doc = xml$parse-document(xml);
      let (method-name, args) = parse-xml-rpc-call(doc);
      let fun = lookup-xml-rpc-method(method-name)
        | xml-rpc-fault(*xml-rpc-internal-error-fault-code*,
                        "Method not found.");
      send-xml-rpc-result(response, apply(fun, args));
    exception (err :: <xml-rpc-fault>)
      send-xml-rpc-fault-response(response, err);
    exception (err :: <error>)
      send-xml-rpc-fault-response
        (response,
         make(<xml-rpc-fault>,
              fault-code: *xml-rpc-internal-error-fault-code*,
              format-string: condition-format-string(err),
              format-arguments: condition-format-arguments(err)));
    end;
 end when;
end;

define constant $xml-rpc-methods :: <string-table> = make(<string-table>);

define method lookup-xml-rpc-method
    (method-name :: <string>)
 => (f :: false-or(<function>))
  element($xml-rpc-methods, method-name, default: #f)
end;

// ---TODO: xml-rpc-method-definer
//
define method register-xml-rpc-method
    (name :: <string>, f :: <function>, #key replace?)
  if (~replace? & lookup-xml-rpc-method(name))
    signal(make(<xml-rpc-error>,
                format-string: "An XML-RPC method named %= already exists.",
                format-arguments: vector(name)))
  else
    $xml-rpc-methods[name] := f;
  end;
end;

define method send-xml-rpc-fault-response
    (response :: <response>, fault :: <xml-rpc-fault>)
  let stream = output-stream(response);
  let value = make(<table>);
  value["faultCode"] := fault-code(fault);
  value["faultString"] := condition-to-string(fault);
  write(stream, "<methodResponse><fault><value>");
  to-xml(value, stream);
  write(stream, "</value></fault></methodResponse>\r\n");
end;

define method send-xml-rpc-result
    (response :: <response>, result :: <object>)
  let stream = output-stream(response);
  write(stream, "<methodResponse><params><param><value>");
  to-xml(result, stream);
  write(stream, "</value></param></params></methodResponse>\r\n");
end;

define method parse-xml-rpc-call
    (node :: xml$<document>)
 => (method-name :: <string>, args :: <sequence>)
  let method-call = find-child(node, #"methodcall")
    | xml-rpc-parse-error("Bad method call, no <methodCall> node found");
  let name-node = find-child(method-call, #"methodname")
    | xml-rpc-parse-error("Bad method call, no <methodName> node found");
  break("find the method name here");
  let method-name = lookup-xml-rpc-method("foo");
  let params = find-child(method-call, #"params")
    | xml-rpc-parse-error("Bad method call, no <params> node found");
  let args = map-as(<vector>,
                    method (param)
                      from-xml(param, xml$name(param))
                    end,
                    params);
  values(method-name, args)
end;


