module: dylan-user

define library http-protocol-test-suite
  use common-dylan;
  use testworks;
  use http-client;
  export http-protocol-test-suite;
end library http-protocol-test-suite;

define module http-protocol-test-suite
  use common-dylan;
  use testworks;
  use http-client;
end module http-protocol-test-suite;
