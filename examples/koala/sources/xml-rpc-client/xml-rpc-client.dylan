Module:    xml-rpc-client
Author:    Carl Gay
Copyright: (C) 2002, Carl L Gay.  All rights reserved.

// An XML-RPC client.
//
// Status:
// There are a few TODO items left to complete here (search for "TODO" below)
// but I have tested it against a Java XML-RPC server with no problems so far.


define thread variable *xml-rpc-port* :: <integer> = 80;
define thread variable *xml-rpc-url* :: <string> = "/RPC2";

define function xml-rpc-call
    (host :: <string>, method-name :: <string>, #rest args)
 => (response :: <object>)
  apply(xml-rpc-call-2, host, *xml-rpc-port*, *xml-rpc-url*, method-name, args)
end;

// xml-rpc-call-2("192.168.26.73", 8502, "/RPC2", "psapi.getAvailableAgentSlas", 10);
//
define function xml-rpc-call-2
    (host :: <string>, port :: <integer>, url :: <string>, method-name :: <string>, #rest args)
 => (response :: <object>)
  let xml = apply(create-method-call-xml, method-name, args);
  when (*debugging-xml-rpc*)
    format-out("%s\n\n", xml);
  end;
  let stream = make(<TCP-socket>, host: host,  port: port);
  format(stream, "POST %s HTTP/1.0\r\n", url);
  format(stream, "Host: %s\r\n", host);
  write (stream, "User-Agent: Koala XML-RPC client\r\n");
  write (stream, "Content-Type: text/xml\r\n");
  format(stream, "Content-Length: %d\r\n", xml.size);
  write(stream, "Pragma: no-cache\r\n");
  write(stream, "\r\n");
  write(stream, xml);
  force-output(stream);
  read-response(stream)
end;



define table $html-quote-map
  = { '<' => "&lt;",
      '>' => "&gt;",
      '&' => "&amp;",
      '"' => "&quot;"
      };

// This is copied from Koala's utils.dylan.  If you fix it here, fix
// it there.
// I'm sure this could use a lot of optimization.
define function quote-html
    (text :: <string>, #key stream)
  if (~stream)
    with-output-to-string (s)
      quote-html(text, stream: s)
    end
  else
    for (char in text)
      let translation = element($html-quote-map, char, default: char);
      iff(instance?(translation, <sequence>),
          write(stream, translation),
          write-element(stream, translation));
    end;
  end;
end quote-html;

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
end;


define function read-response
    (stream :: <tcp-socket>)
 => (response :: <object>)
  let content-length :: <integer> = -1;
  let cl = "Content-length: ";
  let line :: false-or(<string>) = #f;
  while ((line := read-line(stream, on-end-of-stream: #f))
         & ~zero?(size(line)))
    when (*debugging-xml-rpc*)
      format-out("%s\n", line);
    end;
    // --TODO: Are HTTP headers case-sensitive?  Can't remember...
    if (subsequence-position(line, cl) == 0)
      // ---TODO: robustify this to skip whitespace and handle errors.
      content-length := string-to-integer(line, start: cl.size);
    end;
  end;
  if (content-length == -1)
    signal(make(<xml-rpc-error>,
                format-string: "No Content-Length header was received."));
  else
    // TODO: signal <xml-rpc-error> on end of stream.
    //let xml = read(stream, content-length);
    let xml = make(<byte-string>, size: content-length, fill: ' ');

    // kludge to work around hideous bug in the read method.  This is 
    // fixed in the FunDev sources, so the fix should be in the next
    // release after 2.0 SP1.
    block (continue)
      for (i from 0 below content-length)
        let elem = read-element(stream, on-end-of-stream: #f);
        if (elem)
          xml[i] := elem;
        else
          continue();
        end;
      end;
    end block;

    parse-response(xml)
  end if;
end;

define function parse-response
    (xml :: <string>)
 => (response :: <object>)
  when (*debugging-xml-rpc*)
    format-out("Received response:\n%s\n", xml);
  end;
  let doc :: xml$<document> = xml$parse-document(xml);
  parse-xml-rpc-response(doc);
end;

define method parse-xml-rpc-response (node :: xml$<document>)
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
  value
    | xml-rpc-parse-error("Bad method response, no value element found.");
  let result = from-xml(value, xml$name(value));
  if (fault)
    let code = element(result, "faultCode", default: 0);
    signal(make(<xml-rpc-fault>,
                fault-code: code,
                format-string: "XML-RPC error (code = %d): %=",
                format-arguments: vector(code,
                                         element(result, "faultString",
                                                 default: "<no explanation provided>"))))
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


