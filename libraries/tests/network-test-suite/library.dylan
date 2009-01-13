Module: dylan-user
Author: Carl Gay

define library network-test-suite
  use http-test-suite;
  use testworks;
  use wiki-test-suite;
  use xml-rpc-client-test-suite;  // todo -- currently not using testworks
  use xmpp-test-suite;

  export network-test-suite;
end library network-test-suite;

define module network-test-suite
  use http-test-suite;
  use testworks;
  use wiki-test-suite;
  use xml-rpc-client-test-suite;
  use xmpp-test-suite;

  export network-test-suite;
end module network-test-suite;
