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

define module xml-test
  use common-dylan;
  use format-out;
  use xml-parser;
end module xml-test;

