Module:    web-services-test
Synopsis:  Tests for the Schema definitions.
Author:    Dr. Matthias Hölzl
Copyright: (C) 2005, Dr. Matthias Hölzl.  All rights reserved.

define test only-one-ur-type-test ()
  let u1 = make(<ur-type-definition>);
  check("ur-type", instance?, u1, <ur-type-definition>);
  let u2 = make(<ur-type-definition>);
  check-equal("only one", u1, u2);
  check-equal("same as constant", u1, $any-type);
end test only-one-ur-type-test;

define test ur-type-name-test ()
  let qname = xml-name($any-type);
  check-equal("ur-type name", "anyType", qname.name-local-name);
  check-equal("ur-type namespace", $xml-schema-namespace,
              qname.name-namespace);
end test ur-type-name-test;

define test simple-ur-type-name-test ()
  let qname = xml-name($any-simple-type);
  check-equal("ur-type name", "anySimpleType", qname.name-local-name);
  check-equal("ur-type namespace", $xml-schema-namespace,
              qname.name-namespace);
end test simple-ur-type-name-test;

define suite xml-schema-suite ()
  test only-one-ur-type-test;
  test ur-type-name-test;
  test simple-ur-type-name-test;
end suite xml-schema-suite;
