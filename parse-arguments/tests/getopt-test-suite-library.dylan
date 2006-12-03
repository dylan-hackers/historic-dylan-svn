module: dylan-user

define library getopt-test-suite
  use common-dylan;
  use io;
  use parse-arguments;
  use testworks;

  export getopt-test-suite;
end library;

define module getopt-test-suite
  use common-dylan, exclude: { format-to-string };
  use format;
  use parse-arguments;
  use testworks;

  export getopt-test-suite;
end module;
