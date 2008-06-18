Module:    xml-rpc-common
Synopsis:  Definitions shared by the XML-RPC client and server
Author:    Carl Gay
Copyright: (C) 2002, Carl L Gay.  All rights reserved.


//---TODO: Verify that <value>foo</value> parses the same as
//         <value><string>foo</string></value> since string is
//         the default type.

// If true, output useless debugging statements.
//
define variable *debugging-xml-rpc* :: <boolean> = #f;

// ---TODO: Parameterize this in the Koala config file.
// If this is true, err when we receive data that we can't parse,
// even if there's a way to continue.  Mostly here for debugging 
// the server/client, but could be used to validate other servers
// or clients also.  Not used much yet.
//
define variable *strict?* :: <boolean> = #t;

define function set-strict-mode
    (strict? :: <boolean>)
  *strict?* := strict?
end;

define function strict-mode?
    () => (strict? :: <boolean>)
  *strict?*
end;


// All XML-RPC errors inherit from this class.
//
define class <xml-rpc-error> (<format-string-condition>, <error>)
end;

// An error of this type will be thrown if a valid <fault> response
// is returned.
//

define class <xml-rpc-fault> (<xml-rpc-error>)
  constant slot fault-code :: <integer>, required-init-keyword: #"fault-code";
end;

define function xml-rpc-fault
    (code :: <integer>, message :: <string>, #rest format-args)
  signal(make(<xml-rpc-fault>,
              fault-code: code,
              format-string: message,
              format-arguments: format-args));
end;

// This error will be thrown if a malformed XML call/response is
// encountered.
//
define class <xml-rpc-parse-error> (<xml-rpc-error>)
end;

define method xml-rpc-parse-error
    (format-string, #rest format-args)
  signal(make(<xml-rpc-parse-error>,
              format-string: format-string,
              format-arguments: format-args))
end;

// Convert a Dylan object to XML-RPC.
// Note that base64 strings are just <string>s, so the user is responsible
// for explicitly converting TO base64 via base64-encode-string.  Conversion
// FROM base64 is done automatically.
//
define generic to-xml
    (arg :: <object>, stream :: <stream>)
 => ();

// Default method
define method to-xml (arg :: <object>, stream :: <stream>) => ()
  xml-rpc-parse-error("The type of %= couldn't be converted to a valid XML-RPC"
                        "parameter type.",
                      arg);
end;

define method to-xml (arg :: <string>, stream :: <stream>) => ()
  write(stream, "<string>");
  quote-html(arg, stream: stream);
  write(stream, "</string>");
end;

define method to-xml (arg :: <integer>, stream :: <stream>) => ()
  format(stream, "<int>%d</int>", arg);
end;

define method to-xml (arg :: <boolean>, stream :: <stream>) => ()
  format(stream, "<boolean>%s</boolean>",
         if (arg) "1" else "0" end);
end;

define method to-xml (arg :: <float>, stream :: <stream>) => ()
  format(stream, "<double>%s</double>",
         float-to-formatted-string(arg));
end;

define method to-xml (arg :: <date>, stream :: <stream>) => ()
  format(stream, "<dateTime.iso8601>%s</dateTime.iso8601>",
         as-iso8601-string(arg));
end;

define method to-xml (arg :: <table>, stream :: <stream>) => ()
  write(stream, "<struct>");
  for (key in key-sequence(arg))
    write(stream, "<member><name>");
    // ---TODO: The XML-RPC spec seems to assume keys are strings, since
    // the example doesn't show any type specified.  Should verify that.
    quote-html(iff(instance?(key, <string>),
                   key,
                   format-to-string("%=", key)),
               stream: stream);
    write(stream, "</name><value>");
    to-xml(arg[key], stream);
    write(stream, "</value></member>");
  end;
  write(stream, "</struct>");
end;

define method to-xml (arg :: <sequence>, stream :: <stream>) => ()
  write(stream, "<array><data>");
  for (item in arg)
    write(stream, "<value>");
    to-xml(item, stream);
    write(stream, "</value>");
  end;
  write(stream, "</data></array>");
end;

define function find-child (node :: xml$<node-mixin>, name :: <symbol>)
  block (return)
    for (child in xml$node-children(node))
      if (xml$name(child) = name)
        return(child);
      end;
    end;
    #f
  end;
end;


// Convert an XML response into a Dylan object.
define generic from-xml
    (node :: <object>, name :: <object>)
 => (object :: <object>);

define method from-xml
    (node :: xml$<element>, name :: <symbol>)
 => (object :: <object>);
  xml-rpc-parse-error("Unrecognized node type.");
end;

define method from-xml
    (node :: xml$<element>, name == #"boolean")
 => (object :: <boolean>)
  let it = xml$text(node);
  if (it = "1")
    #t
  elseif (it = "0")
    #f
  else
    *strict?*
      & xml-rpc-parse-error("Malformed <boolean> element.");
  end if
end;

define method from-xml
    (node :: xml$<element>, name == #"string")
 => (object :: <string>)
  xml$text(node)
end;

define method from-xml
    (node :: xml$<element>, name == #"int")
 => (object :: <integer>)
  string-to-integer(xml$text(node))
end;

define method from-xml
    (node :: xml$<element>, name == #"i4")
 => (object :: <integer>)
  from-xml(node, #"int")
end;

define method from-xml
    (node :: xml$<element>, name == #"double")
 => (object :: <double-float>)
  string-to-float(xml$text(node));
end;

define method from-xml
    (node :: xml$<element>, name == #"datetime.iso8601")
 => (object :: <date>)
  // Note the colons and dashes are removed because <date> doesn't handle them.
  // It looks to me like the ISO8601 standard allows both characters so this
  // is a bug, but I haven't been able to download the spec so not 100% sure.
  let str = remove(remove(xml$text(node), ':'), '-');
  make(<date>, iso8601-string: str);
end;

define method from-xml
    (node :: xml$<element>, name == #"base64")
 => (object :: <string>)
  base64-decode(xml$text(node))
end;


// <array><data><value>...</value><value>...</value></data></array>
//
define method from-xml
    (node :: xml$<element>, name == #"array")
 => (object :: <sequence>)
  // I wonder why the redundant "<data>" node is used in XML-RPC...
  let data-node = find-child(node, #"data")
    | xml-rpc-parse-error("Bad array element, data element not found.");
  from-xml(data-node, xml$name(data-node))
end;

define method from-xml
    (node :: xml$<element>, name == #"data")
 => (object :: <sequence>)
  let children = xml$node-children(node);
  let v = make(<vector>, size: children.size);
  map-into(v,
           method (child)
             from-xml(child, xml$name(child))
           end,
           children)
end;

define method from-xml
    (node :: xml$<element>, name == #"value")
 => (object :: <object>)
  let children = xml$node-children(node);
  select (children.size)
    0 => xml-rpc-parse-error("Value element contains no data.");
    1 => from-xml(children[0], xml$name(children[0]));
    // ---TODO: Don't err here if ~*strict?*.
    //          Collect values into a vector instead?
    otherwise => xml-rpc-parse-error("Value element contains multiple subelements.");
  end
end;

define method from-xml
    (node :: xml$<char-string>, name /* == "chars" */)
 => (object :: <string>)
  xml$text(node)
end;

// <struct><member><name>foo</name><value>...</value></member></struct>
//
define method from-xml
    (node :: xml$<element>, name == #"struct")
 => (object :: <table>)
  let children = xml$node-children(node);
  let tbl = make(<string-table>, size: children.size);
  for (child in children)
    if (xml$name(child) = #"member")
      let name-node = find-child(child, #"name")
        | xml-rpc-parse-error("Struct member has no name element.");
      let value-node = find-child(child, #"value")
        | xml-rpc-parse-error("Struct member has no value element.");
      let key = xml$text(name-node);
      tbl[key] := from-xml(value-node, xml$name(value-node));
    end;
  end;
  tbl
end;

define table $html-quote-map
  = { '<' => "&lt;",
      '>' => "&gt;",
      '&' => "&amp;",
      '"' => "&quot;"
      };

// Copied from Koala's utils.dylan.  Fix it there if you fix it here!
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


