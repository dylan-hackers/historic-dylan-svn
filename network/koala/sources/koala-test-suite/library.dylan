Module:   dylan-user
Synopsis: Koala test suite
Author:   Carl Gay

define library koala-test-suite
  use common-dylan,
    import: { common-dylan,
              threads };
  use system,
    import: { date };
  use koala,
    import: { koala, koala-unit };
  use http-client;
  use network,
    import: { sockets };
  use testworks;
  export koala-test-suite;
end library koala-test-suite;

define module koala-test-suite
  use common-dylan;
  use threads;
  use date;
  use testworks;
  use koala;
  use koala-unit;
  use http-client;
  use sockets,
    import: { <connection-failed>,
              <address-in-use>,
              all-addresses,
              host-address,
              start-sockets,
              $local-host };
end module koala-test-suite;

