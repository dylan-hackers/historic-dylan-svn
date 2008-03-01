Module:    dylan-user
Synopsis:  Test suite for the strings library
Author:    Carl Gay


define library strings-test-suite
  use common-dylan;
  use strings;
  use testworks;
  use testworks-specs;
  use regular-expressions;
  export strings-test-suite;
end;

define module strings-test-suite
  use common-dylan;
  use strings;
  use testworks;
  use testworks-specs;
  use regular-expressions,
    import: { compile-regex,
              match-group };
  export strings-test-suite;
end;

