Module: Dylan-user

define library source-location-test-suite
  use common-dylan;
  use testworks;
  use testworks-specs;

  use source-location;

  export source-location-test-suite;
end library source-location-test-suite;

define module source-location-test-suite
  use common-dylan;
  use testworks;
  use testworks-specs;

  use source-location;
  use source-location-rangemap;

  export source-location-test-suite;
end module source-location-test-suite;
