module: dylan-user

define library uri-test
  use common-dylan;
  use testworks;
  use uri;

  export uri-test;
end library uri-test;

define module uri-test
  use common-dylan;
  use testworks;  
  use uri;

  export uri-test-suite;
end module uri-test;
