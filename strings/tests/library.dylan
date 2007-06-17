Module:    dylan-user
Synopsis:  Test suite for the strings library
Author:    Carl Gay


define library strings-test-suite
  use common-dylan;
  use strings;
  use testworks;
  use testworks-specs;
  export strings-test-suite;
end;

define module strings-test-suite
  use common-dylan, exclude: { split };
  use strings;
  use testworks;
  use testworks-specs;
  export strings-test-suite;
end;

