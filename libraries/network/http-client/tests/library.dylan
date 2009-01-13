module: dylan-user

define library http-client-test-suite
  use collections,
    import: { table-extensions };
  use command-line-parser;
  use common-dylan;
  use http-client;
  use http-common;
  use koala,
    import: { koala };
  use io,
    import: { standard-io,
              streams };
  use logging;
  use regular-expressions;
  use system,
    import: { threads };
  use testworks;

  export http-client-test-suite;

end library http-client-test-suite;

define module http-client-test-suite
  use command-line-parser;
  use common-dylan;
  use http-client-internals;
  use http-common;
  use koala,
    rename: { log-error => log-koala-error,
              log-warning => log-koala-warning,
              log-info => log-koala-info,
              log-debug => log-koala-debug,
              log-trace => log-koala-trace };
  use logging;
  use regular-expressions;
  use standard-io,
    import: { *standard-output* };
  use streams;
  use table-extensions;
  use testworks;
  use threads,
    import: { sleep };

  export http-client-test-suite;

end module http-client-test-suite;

