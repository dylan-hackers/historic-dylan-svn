Module:    dylan-user
Synopsis:  Test suite for the strings library
Author:    Carl Gay


define library strings-tests
  use common-dylan;
  use strings;
  use testworks;
  use testworks-specs;
  export strings-tests;
end;

define module strings-tests
  use common-dylan, exclude: { split };
  use strings;
  use testworks;
  use testworks-specs;
end;

