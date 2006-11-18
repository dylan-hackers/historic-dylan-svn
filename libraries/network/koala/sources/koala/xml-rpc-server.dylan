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

// This is the registered XML-RPC responder function.
// ---TODO: Shouldn't really even register the responder if XML-RPC is disabled.
//
define function respond-to-xml-rpc-request
    (request :: <request>, response :: <response>)
  when (*xml-rpc-enabled?*)
    set-content-type(response, "text/xml");
    // All responses start with a valid XML document header.
    write(output-stream(response),
          "<?xml version=\"1.0\" encoding=\"iso-8859-1\" ?>");
    block ()
      let xml = request-content(request);
      when (*debugging-xml-rpc*)
        log-debug("Received XML-RPC call:\n   %s", xml);
      end;
      let doc = xml$parse-document(xml);
      let (method-name, args) = parse-xml-rpc-call(doc);
      log-debug("method-name = %=, args = %=", method-name, args);
      let fun = lookup-xml-rpc-method(method-name)
        | xml-rpc-fault(*xml-rpc-internal-error-fault-code*,
                        "Method not found: %=",
                        method-name);
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
    (name :: <string>, f :: <function>, #key replace? :: <boolean>)
  if (~replace? & lookup-xml-rpc-method(name))
    signal(make(<xml-rpc-error>,
                format-string: "An XML-RPC method named %= already exists.",
                format-arguments: vector(name)))
  else
    $xml-rpc-methods[name] := f;
    log-info("XML-RPC method registered: %=", name);
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
  let xml = with-output-to-string(s)
              to-xml(result, s);
            end;
  *debugging-xml-rpc*
    & log-debug("Sending XML: %=", xml);
  write(stream, xml);
  //to-xml(result, stream);
  write(stream, "</value></param></params></methodResponse>\r\n");
end;

define method parse-xml-rpc-call
    (node :: xml$<document>)
 => (method-name :: <string>, args :: <sequence>)
  let method-call = find-child(node, #"methodcall")
    | xml-rpc-parse-error("Bad method call, no <methodCall> node found");
  let name-node = find-child(method-call, #"methodname")
    | xml-rpc-parse-error("Bad method call, no <methodName> node found");
  let method-name = xml$text(name-node)
    | xml-rpc-parse-error("Bad method call, invalid methodName");
  let params-node = find-child(method-call, #"params")
    | xml-rpc-parse-error("Bad method call, no <params> node found");
  let args = map-as(<vector>,
                    method (param-node)
                      let value-node = find-child(param-node, #"value");
                      from-xml(value-node, xml$name(value-node))
                    end,
                    xml$node-children(params-node));
  values(method-name, args)
end;

define function init-xml-rpc-server
    () => ()
  when (*xml-rpc-enabled?*)
    register-url(*xml-rpc-server-url*, respond-to-xml-rpc-request);

    // Provide a basic way to test the server.
    register-xml-rpc-method("ping", method () #t end, replace?: #t);
    register-xml-rpc-method("echo", method (#rest args) args end, replace?: #t);
  end;
end;


