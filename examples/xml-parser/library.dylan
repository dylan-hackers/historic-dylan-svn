module: dylan-user

define library xml-parser
  use common-dylan;
  use io;
  use table-extensions;
  use meta;

  export xml-parser;
end library;

define module xml-parser
  create transform, transform-document, before-transform, 
         <xform-state>, *xml-depth*;

  create parse-document;

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

