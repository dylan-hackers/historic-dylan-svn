module: dylan-user

define library xml-parser
  use common-dylan;
  use io;
  use table-extensions;
  use meta;

  export xml-parser;
end library;

define module xml-parser
  create transform, transform-document,
         before-transform, *depth*;

/*
// I really don't want the below defs -- parse-document should
// do everything, but until I get that working, I need to test
// the system piecemeal.
  create parse-element, parse-attribute, parse-pi,
         parse-stag, parse-content, parse-etag, parse-empty-elem-tag,
         parse-char-data, parse-comment, parse-system-literal,
         parse-version-num, parse-pubid-literal, parse-cd-sect,
         parse-elementdecl, parse-char-ref, parse-content,
         parse-entity-decl; */

// all the above parse-foos will be replaced with:
  create parse-document;

// beginning to integrate Chris' parse engine
  create <document>, <element>, <attribute>, <entity-reference>,
    <char-reference>, <char-string>, <xml>, text, char, name;
  create entity-value, element-attributes, attribute-value;
end module xml-parser;

define module interface
  use common-dylan, exclude: { format-to-string };
  use streams;
  use format;

  use meta;
  use xml-parser;

  export <letter>, <digit>, <hex-digit>, <version-number>,
         <node>;
  export node-children;
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
end module %productions;

define module transform
  use common-dylan, exclude: {format-to-string };
  use streams;
  use format;
  use standard-io;
  use format-out;
  use xml-parser;
  use interface;
  use %productions;
end module transform;

