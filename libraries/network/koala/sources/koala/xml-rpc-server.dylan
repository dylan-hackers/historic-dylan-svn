Module:    httpi
Synopsis:  XML-RPC server
Author:    Carl Gay
Copyright: Copyright (c) 2001-2002 Carl L. Gay.  All rights reserved.
License:   Functional Objects Library Public License Version 1.0
Warranty:  Distributed WITHOUT WARRANTY OF ANY KIND


// Exported
define class <xml-rpc-configuration> (<object>)

  // This is the fault code that will be returned to the caller if
  // any error other than <xml-rpc-fault> is thrown during the execution
  // of the RPC.  For example, if there's a parse error in the XML
  // that's received.  If users want to return a fault code they
  // should use the xml-rpc-fault method.
  // Exported
  slot internal-error-fault-code :: <integer> = 0,
    init-keyword: internal-error-fault-code:;

  // Maps method names to response functions.  If namespaces are used then
  // the value may be another <string-table> containing the mapping for that
  // namespace.
  // Exported
  constant slot xml-rpc-methods :: <string-table> = make(<string-table>),
    init-keyword: methods:;

  // Exported
  slot debugging-enabled? :: <boolean> = #f,
    init-keyword: debug?:;

end class <xml-rpc-configuration>;


define method respond-to-xml-rpc-request
    (xml-rpc-config :: <xml-rpc-configuration>,
     request :: <request>,
     response :: <response>)
  set-content-type(response, "text/xml");
  // All responses start with a valid XML document header.
  write(output-stream(response),
        "<?xml version=\"1.0\" encoding=\"iso-8859-1\" ?>");
  block ()
    let xml = request-content(request);
    when (xml-rpc-config.debugging-enabled?)
      // Could probably do with some more fine-grained control over debug content.
      log-debug("XML-RPC: Received call:\n   %s", xml);
    end;
    let doc = xml$parse-document(xml);
    let (method-name, args) = parse-xml-rpc-call(doc);
    log-debug("XML-RPC: method-name = %=, args = %=", method-name, args);
    let fun = lookup-xml-rpc-method(xml-rpc-config, method-name)
              | xml-rpc-fault(xml-rpc-config.internal-error-fault-code,
                              "Method not found: %=",
                              method-name);
    send-xml-rpc-result(xml-rpc-config, response, apply(fun, args));
  exception (err :: <xml-rpc-fault>)
    send-xml-rpc-fault-response(xml-rpc-config, response, err);
  exception (err :: <error>)
    let fault = make(<xml-rpc-fault>,
                     fault-code: xml-rpc-config.internal-error-fault-code,
                     format-string: condition-format-string(err),
                     format-arguments: condition-format-arguments(err));
    send-xml-rpc-fault-response(xml-rpc-config, response, fault);
  end;
end method respond-to-xml-rpc-request;

define method lookup-xml-rpc-method
    (xml-rpc-config :: <xml-rpc-configuration>, method-name :: <string>)
 => (f :: false-or(<function>))
  // todo -- Implement namespaces (methods named x.y.z)
  element(xml-rpc-config.xml-rpc-methods, method-name, default: #f)
end;

// todo -- xml-rpc-method-definer
//
// Exported
define method register-method
    (xml-rpc-config :: <xml-rpc-configuration>, name :: <string>, f :: <function>,
     #key replace? :: <boolean>)
  if (~replace? & lookup-xml-rpc-method(xml-rpc-config, name))
    signal(make(<xml-rpc-error>,
                format-string: "An XML-RPC method named %= already exists.",
                format-arguments: vector(name)))
  else
    xml-rpc-config.xml-rpc-methods[name] := f;
    log-info("XML-RPC method registered: %=", name);
  end;
end;

define method send-xml-rpc-fault-response
    (xml-rpc-config :: <xml-rpc-configuration>, response :: <response>,
     fault :: <xml-rpc-fault>)
  let stream = output-stream(response);
  let value = make(<table>);
  value["faultCode"] := fault-code(fault);
  value["faultString"] := condition-to-string(fault);
  write(stream, "<methodResponse><fault><value>");
  to-xml(value, stream);
  write(stream, "</value></fault></methodResponse>\r\n");
end;

define method send-xml-rpc-result
    (xml-rpc-config :: <xml-rpc-configuration>, response :: <response>,
     result :: <object>)
  let stream = output-stream(response);
  write(stream, "<methodResponse><params><param><value>");
  let xml = with-output-to-string(s)
              to-xml(result, s);
            end;
  if (xml-rpc-config.debugging-enabled?)
    log-debug("XML-RPC: Sending %=", xml);
  end;
  write(stream, xml);
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

// Exported
define method register-url
    (config :: <http-server-configuration>,
     url :: <string>,
     xml-rpc-config :: <xml-rpc-configuration>,
     #key replace?)
  register-url(config, url, curry(respond-to-xml-rpc-request, xml-rpc-config),
               replace?: replace?, prefix?: #f);
  log-info("URL %s is an XML-RPC server.", url);
end;

// Exported
define method register-test-methods
    (xml-rpc-config :: <xml-rpc-configuration>)
  register-method(xml-rpc-config, "ping", method () #t end, replace?: #t);
  register-method(xml-rpc-config, "echo", method (#rest args) args end, replace?: #t);
end;

// Exported
define method register-introspection-methods
    (xml-rpc-config :: <xml-rpc-configuration>)
  // todo --
  signal(make(<xml-rpc-error>,
              format-string: "XML-RPC introspection not yet implemented"));
end;

