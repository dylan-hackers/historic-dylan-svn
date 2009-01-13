Module: dylan-user
Author: Carl Gay

define library http-test-suite
  use http-client-test-suite;
  use http-common-test-suite;
  use http-protocol-test-suite;
  use koala-test-suite;
  use testworks;

  export http-test-suite;
end library http-test-suite;

define module http-test-suite
  use http-client-test-suite;
  use http-common-test-suite;
  use http-protocol-test-suite;
  use koala-test-suite;
  use testworks;

  export http-test-suite;
end module http-test-suite;
