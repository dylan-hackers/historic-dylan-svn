Module: dylan-user
Author: Carl Gay

define library xml-test-suite
  use common-dylan;
  use system,
    import: { locators };
  use testworks;
  use testworks-specs;
  use xml-parser;

  export xml-test-suite;
end library xml-test-suite;

define module xml-test-suite
  use common-dylan;
  use locators,
    import: { locator-name, <file-locator> };
  use testworks;
  use testworks-specs;

  use xml-parser;
  use simple-xml;
  use xml-stream-parser;
  use printing;
  use %productions;

  export xml-test-suite;
end module xml-test-suite;

