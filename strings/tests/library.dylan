Module:    dylan-user
Synopsis:  Test suite for the strings library
Author:    Carl Gay


define library strings-test-suite
  use common-dylan;
  use regular-expressions;
  use strings;
  use system;
  use testworks;
  use testworks-specs;
  export strings-test-suite;
end;

define module strings-test-suite
  use common-dylan;
  use regular-expressions,
    import: { compile-regex,
              match-group };
  use strings;
  use locators,
    import: { locator-name,
              <file-locator> };
  use testworks;
  use testworks-specs;
  export strings-test-suite;
end;

