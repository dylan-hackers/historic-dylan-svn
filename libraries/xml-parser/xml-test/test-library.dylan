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
end library xml-test;

define module entity-pass
  use common-dylan, exclude: { format-to-string };
  use xml-parser;
  use streams;
  use format;

  export collect-entity-defs, referenced-entities;
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
  use format;
  use xml-parser;
  use html-xform;
  use streams;
  use date;
end module xml-test;

