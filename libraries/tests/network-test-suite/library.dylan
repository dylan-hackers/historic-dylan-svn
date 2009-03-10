Module: dylan-user
Author: Carl Gay

define library network-test-suite
  use common-dylan;
  use http-test-suite;
  use system,
    import: { locators };
  use testworks;
  use wiki-test-suite;
  use xml-rpc-client-test-suite;
  // use xmpp-test-suite;  // convert to testworks

  export network-test-suite;
end library network-test-suite;

define module network-test-suite
  use common-dylan;
  use http-test-suite;
  use locators,
    import: { <file-locator>, locator-name };
  use testworks;
  use wiki-test-suite;
  use xml-rpc-client-test-suite;
  // use xmpp-test-suite;  // convert to testworks

  export network-test-suite;
end module network-test-suite;
