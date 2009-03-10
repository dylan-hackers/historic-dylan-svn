Module: dylan-user
Author: Carl Gay

define library http-test-suite
  use common-dylan;
  use http-client-test-suite;
  use http-common-test-suite;
  use http-protocol-test-suite;
  use koala-test-suite;
  use system,
    import: { locators };
  use testworks;

  export http-test-suite;
end library http-test-suite;

define module http-test-suite
  use common-dylan;
  use http-client-test-suite;
  use http-common-test-suite;
  use http-protocol-test-suite;
  use koala-test-suite;
  use locators,
    import: { <file-locator>, locator-name };
  use testworks;

  export http-test-suite;
end module http-test-suite;
