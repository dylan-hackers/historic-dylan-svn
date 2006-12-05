Module:    dylan-user
Synopsis:  Exercises the XML library
Author:    Douglas M. Auclair, doug@cotilliongroup.com
Copyright: (c) 2001, LGPL
Version:   1.0

define library xml-test
  use common-dylan;
  use io;
  use xml-parser;
  use system;
  use getopt;
  use collection-extensions;
end library xml-test;

define module entity-pass
  use common-dylan, exclude: { format-to-string };
  use xml-parser;
  use streams;
  use format;

  export collect-entity-defs, referenced-entities, print-in, header-comment;
end module entity-pass;

define module html-xform
  use common-dylan, exclude: { format-to-string };
  use xml-parser;
  use entity-pass;
  use format-out;
  use streams;
  use format;

  export $html, *substitute?*;
end module html-xform;

define module xml-test
  use common-dylan;
  use format-out;
  use standard-io;
  use format;
  use xml-parser;
  use html-xform;
  use streams;
//  use date;
  use getopt;
  use sequence-utilities;
end module xml-test;
