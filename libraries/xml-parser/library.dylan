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
    <char-reference>, <char-string>, <xml>, <node>, text, char, name;
  create entity-value, element-attributes, attribute-value, 
    node-children, element-parent, collect-elements;
end module xml-parser;

define module interface
  use common-dylan, exclude: { format-to-string };
  use streams;
  use format;

  use meta;
  use xml-parser;

  export <letter>, <digit>, <hex-digit>, <version-number>,
    element-parent-setter;
end module interface;

define module transform
  use common-dylan, exclude: {format-to-string };
  use streams;
  use format;
  use standard-io;
  use xml-parser;
  use interface;
end module transform;

define module collect
  use common-dylan;
  use streams;
  use format;
  use xml-parser, rename: { attribute-value => value };
  use interface;
end module collect;

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

