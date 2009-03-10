Module:   dylan-user
Synopsis: Koala test suite
Author:   Carl Gay

define library koala-test-suite
  use common-dylan,
    import: { common-dylan,
              threads };
  use http-client;
  use http-common;
  use koala,
    import: { koala,
              koala-unit };
  use network,
    import: { sockets };
  use system,
    import: { date,
              locators };
  use testworks;
  use uri;
  use xml-rpc-client;

  export koala-test-suite;
end library koala-test-suite;

define module koala-test-suite
  use common-dylan;
  use date;
  use http-client;
  use http-common;
  use koala;
  use koala-unit;
  use locators,
    import: { <directory-locator>,
              <file-locator>,
              locator-name };
  use sockets,
    import: { <connection-failed>,
              <address-in-use>,
              all-addresses,
              host-address,
              start-sockets,
              $local-host };
  use testworks;
  use threads;
  use uri;
  use xml-rpc-client;

  export koala-test-suite;
end module koala-test-suite;

