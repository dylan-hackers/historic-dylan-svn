module: dylan-user

define library xml-parser
  use common-dylan;
  use io;
  use table-extensions;
  use meta;

  export xml-parser;
end library;

define module xml-parser
  create <xml-builder>, <xml-element>, <xml-parse-error>;
  create start-element, end-element;
  create name, attributes; // slots for <xml-element>

  create display-node;

// I really don't want the below defs -- parse-document should
// do everything, but until I get that working, I need to test
// the system piecemeal.
  create parse-xml-element-start;
  create parse-element, parse-attribute, parse-pi,
         parse-stag, parse-content, parse-etag, parse-empty-elem-tag,
         parse-char-data, parse-comment, parse-system-literal,
         parse-version-num, parse-pubid-literal, parse-cd-sect,
         parse-elementdecl, parse-char-ref, parse-content,
         parse-entity-decl, parse-def|content;

// all the above parse-foos will be replaced with:
  create parse-document;
// however, parse-document REQUIRES there be the prologue (some
// mal-formed xml docs skip that, THEN the root element, and 
// FINALLY, AT LEAST ONE misc (which is a space, comment or 
// processing instruction).  I feel the [ ] parse ("and" parse)
// should be replaced with an { } parse ("or").  Thinking on that
// while I test the rest of the system.

// beginning to integrate Chris' parse engine
  create <document>, <element>, <attribute>, 
    name, value, children, tag-name, attributes;

end module xml-parser;

define module interface
  use common-dylan, exclude: { format-to-string };
  use streams;
  use format;

  use meta;
  use xml-parser;

  export <letter>, <digit>, <hex-digit>, <version-number>,
         <node>, <text-node>;
  export node-children, attribute-name, attribute-value, 
         element-tag-name, element-attributes, text;
end module interface;

define module %productions
  use common-dylan, exclude: { format-to-string };
  use streams;
  use format;
  use table-extensions;
  use print;

  use meta;
  use interface;
  use xml-parser;

  export parse-beginning-of-tag;
end module %productions;

define module builder-impl
  use common-dylan, exclude: { format-to-string };
  use streams;
  use format;
  use table-extensions;
  use print;

  use meta;
  use xml-parser;
  use %productions;
  use interface;
end module builder-impl;

define module display
  use common-dylan, exclude: {format-to-string };
  use streams;
  use format;
  use format-out;
  use xml-parser;
  use interface;
end module display;

