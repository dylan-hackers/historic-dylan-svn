module: xml-parser

define abstract open class <xml-builder> (<object>)
end class <xml-builder>;

define class <xml-element> (<object>)
  slot name, init-keyword: name:;
  slot attributes, init-keyword: attributes:;
end class <xml-element>;

define method print-object(element :: <xml-element>, s :: <stream>) => ()
  format(s, "<%s", element.name);
  for(value keyed-by key in element.attributes)
    format(s, " %s='%s'", key, value);
  end for;
  format(s, ">\n");
end method print-object;

define class <xml-parse-error> (<error>)
  slot message;
  slot source-location;
end class <xml-parse-error>;

define generic start-element (<xml-builder>, <xml-element>);

define generic end-element (<xml-builder>, <xml-element>);

define generic text (<xml-builder>, <string>);