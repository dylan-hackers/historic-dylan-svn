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
  create start-element, end-element, text;

  create parse-xml-element-start;
  create children, children-setter, name, attributes;
end module xml-parser;

define module interface
  use common-dylan, exclude: { format-to-string };
  use streams;
  use format;

  use meta;
  use xml-parser;
end module interface;

define module xml-parser-implementation
  use common-dylan, exclude: { format-to-string };
  use streams;
  use format;
  use table-extensions;
  use print;

  use meta;
  use xml-parser;
  use interface;
end module xml-parser-implementation;


