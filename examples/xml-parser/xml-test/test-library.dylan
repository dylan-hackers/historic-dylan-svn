Module:    dylan-user
Synopsis:  Exercises the XML library
Author:    Douglas M. Auclair, doug@cotilliongroup.com
Copyright: (c) 2001, LGPL
Version:   1.0

define library xml-test
  use common-dylan;
  use io;
  use xml-parser;
end library xml-test;

define module entity-pass
  use common-dylan;
  use xml-parser;
  use streams;
  use format;

  export collect-entity-defs, referenced-entities;
end module entity-pass;

define module html-xform
  use common-dylan;
  use xml-parser;
  use entity-pass;
  use streams;
  use format;

  export $html;
end module html-xform;

define module xml-test
  use common-dylan;
  use format-out;
  use xml-parser;
  use html-xform;
  use streams;
end module xml-test;

