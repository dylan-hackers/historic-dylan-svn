module: interface
author: Andreas Bogk, Chris Double
copyright: (c) 2001, LGPL
translated-into-its-own-module-by: Douglas M. Auclair

// Interface bindings into the xml-parser library

define abstract open class <xml-builder> (<object>)
end class <xml-builder>;

define class <xml-element> (<object>)
  slot name :: <string>, init-keyword: name:;
  slot attributes :: <table>, init-keyword: attributes:;
  slot char-data :: <string>;
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

define open generic start-element(bldr :: <xml-builder>, elt :: <xml-element>);
define open generic end-element(bldr :: <xml-builder>, elt :: <xml-element>);
define open generic text (bldr :: <xml-builder>, txt :: <string>);
