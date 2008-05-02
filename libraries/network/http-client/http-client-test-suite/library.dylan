module: dylan-user

define library http-client-test-suite
  use common-dylan;
  use testworks;
  use http-client;
  use koala,
    import: { koala };
  use command-line-parser;
  use io,
    import: { standard-io };
  use system,
    import: { threads };

  export http-client-test-suite;

end library http-client-test-suite;

define module http-client-test-suite
  use common-dylan;
  use testworks;
  use http-client;
  use koala;
  use command-line-parser;
  use standard-io,
    import: { *standard-output* };
  use threads,
    import: { sleep };

  export http-client-test-suite;

end module http-client-test-suite;

