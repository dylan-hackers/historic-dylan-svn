Module:   dylan-user
Synopsis: Koala test suite
Author:   Carl Gay

define library koala-test-suite
  use collection-extensions,
    import: { collection-utilities };
  use common-dylan,
    import: { common-dylan, threads };
  use http-client;
  use http-common;
  use io,
    import: { format-out };
  use koala,
    import: { koala, koala-unit };
  use network,
    import: { sockets };
  use strings;
  use system,
    import: {
      date,
      file-system,
      locators,
      operating-system
    };
  use testworks;
  use uri;
  use xml-rpc-client;

  export koala-test-suite;
end library koala-test-suite;

define module koala-test-suite
  use collection-utilities,
    import: { key-exists? };
  use common-dylan;
  use date;
  use file-system;
  use format-out;
  use http-client;
  use http-common;
  use koala;
  use koala-unit;
  use locators,
    exclude: { <http-server>, <url> };
  use operating-system,
    import: { environment-variable };
  use sockets,
    import: {
      <connection-failed>,
      <address-in-use>,
      all-addresses,
      host-address,
      start-sockets,
      $local-host
    };
  use strings;
  use testworks;
  use threads;
  use uri;
  use xml-rpc-client;

  export koala-test-suite;
end module koala-test-suite;

