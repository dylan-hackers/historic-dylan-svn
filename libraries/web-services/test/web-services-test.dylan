Module:    web-services-test
Synopsis:  Tests for the XML converter.
Author:    Dr. Matthias Hölzl
Copyright: (C) 2005, Dr. Matthias Hölzl.  All rights reserved.

define suite web-services-suite ()
  suite xml-schema-suite;
  suite convert-to-xml-suite;
end suite web-services-suite;

define method main () => ()
  perform-suite(web-services-suite);
//  format-out("Done.\n");
end method main;

begin
  main();
end;
