Module:    dylan-user
Synopsis:  Reading XML using Meta.
Author:    Chris Double
Copyright: (C) 2000, Chris Double.  All rights reserved.

define library xml-parser2
  use common-dylan;
  use io;
  use meta;

  // Add any more module exports here.
  export xml-parser, %productions;
end library xml-parser2;

define module xml-parser
  create parse-document, display-node;
  create <node>, <document>, <element>, <attribute>, <text-node>;
  create <entity-reference>, <char-reference>;

  create node-children, element-tag-name, element-attributes, text, 
    attribute-name, attribute-value;
end module xml-parser;


define module classes
  use common-dylan;
  use xml-parser;

  export <letter>, <digit>, <hex-digit>, <other-name-char>,
    <space>, <version-number>, <all-chars>;
end module xml-meta-play;

define module %productions
  use common-dylan, exclude: {format-to-string};

  use format;
  use format-out;
  use standard-io;
  use streams;

  use meta;

  use classes;
  use xml-parser;

// these exports are for test purposes only
  export parse-prolog, parse-misc, parse-pi, parse-doctype;
  export parse-element, parse-content, 
end module %productions;

define module display
  use common-dylan;
  use xml-parser;
  use format-out;
end module display;



