Module:    xml-rpc-client
Author:    Carl Gay
Synopsis:  XML RPC client library
Copyright: (C) 2002, Carl L Gay.  All rights reserved.

define generic xml-rpc-call
    (url :: <object>, method-name :: <string>, #rest args)
 => (response :: <object>);

define method xml-rpc-call
    (url :: <string>, method-name :: <string>, #rest args)
 => (response :: <object>)
  apply(xml-rpc-call, parse-url(url), method-name, args)
end;

define method xml-rpc-call
    (url :: <uri>, method-name :: <string>, #rest args)
 => (response :: <object>)
  let xml = apply(create-method-call-xml, method-name, args);
  with-http-connection(conn = url)
    when (*debugging-xml-rpc*)
      format-out("%s\n\n", xml);
    end;
    let headers = #[#["User-Agent", "Koala XML-RPC client"],
                    #["Content-Type", "text/xml"],
                    #["Pragma", "no-cache"]];
    send-request(conn, #"post", url, content: xml, headers: headers);
    let response :: <http-response> = read-response(conn);
    parse-xml-rpc-response(response.response-content)
  end with-http-connection
end method xml-rpc-call;


define table $html-quote-map
  = { '<' => "&lt;",
      '>' => "&gt;",
      '&' => "&amp;",
      '"' => "&quot;"
      };

define function create-method-call-xml
    (method-name :: <string>, #rest args)
 => (xml :: <string>)
  with-output-to-string (s)
    write(s, "<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?><methodCall><methodName>");
    quote-html(method-name, stream: s);
    write(s, "</methodName><params>");
    for (arg in args)
      write(s, "<param><value>");
      to-xml(arg, s);
      write(s, "</value></param>");
    end;
    write(s, "</params></methodCall>");
  end
end function create-method-call-xml;


/*
// Quick and dirty.  Should import the string utils from Koala instead.
//
define function char-equal?
    (c1 :: <character>, c2 :: <character>) => (b :: <boolean>)
  as-lowercase(c1) = as-lowercase(c2)
end;
*/

/*
define function read-response
    (stream :: <tcp-socket>)
 => (response :: <object>)
  let content-length :: <integer> = -1;
  let cl = "Content-length: ";
  let line :: false-or(<string>) = #f;
  while ((line := read-line(stream, on-end-of-stream: #f))
         & ~empty?(line))
    when (*debugging-xml-rpc*)
      format-out("%s\n", line);
    end;
    if (subsequence-position(line, cl, test: char-equal?) == 0)
      // ---TODO: robustify this to skip whitespace and handle errors.
      content-length := string-to-integer(line, start: cl.size);
    end;
  end;
  if (content-length == -1)
    signal(make(<xml-rpc-error>,
                format-string: "No Content-Length header was received."));
  else
    let seq = block ()
                read(stream, content-length)
              exception (ex :: <incomplete-read-error>)
                if (strict-mode?())
                  signal(make(<xml-rpc-error>,
                              format-string: "Content was shorter than expected.  "
                                "Content-length header: %d, actual length: %d",
                              format-arguments: list(content-length,
                                                     ex.stream-error-count)))
                else
                  ex.stream-error-sequence
                end
              end;
    parse-xml-rpc-response(seq)
  end
end function read-response;
*/

define method parse-xml-rpc-response
    (xml :: <string>)
 => (response :: <object>)
  when (*debugging-xml-rpc*)
    format-out("Received response:\n%s\n", xml);
  end;
  let doc :: xml$<document> = xml$parse-document(xml);
  parse-xml-rpc-response(doc);
end method parse-xml-rpc-response;

define method parse-xml-rpc-response
    (node :: xml$<document>)
  let method-response = find-child(node, #"methodresponse")
    | xml-rpc-parse-error("Bad method response, no <methodResponse> node found");
  let params = find-child(method-response, #"params");
  let fault = find-child(method-response, #"fault");
  let value = #f;
  if (params)
    // signal an error here if more than one param present?
    let param = find-child(params, #"param")
      | xml-rpc-parse-error("Bad method response, no param element found.");
    value := find-child(param, #"value");
  elseif (fault)
    value := find-child(fault, #"value");
  else
    xml-rpc-parse-error("Bad method response, neither params nor fault found.");
  end;
  // signal an error here if more than one value present?
  if (~value)
    xml-rpc-parse-error("Bad method response, no value element found.");
  end;
  let result = from-xml(value, xml$name(value));
  if (fault)
    let code = element(result, "faultCode", default: 0);
    let fault-string = element(result, "faultString",
                               default: "<no explanation provided>");
    signal(make(<xml-rpc-fault>,
                fault-code: code,
                format-string: "XML-RPC error (fault code = %d): %=",
                format-arguments: vector(code, fault-string)))
  else
    result
  end
end;

define function init-xml-rpc ()
  start-sockets();
end;

begin
  init-xml-rpc();
end;
