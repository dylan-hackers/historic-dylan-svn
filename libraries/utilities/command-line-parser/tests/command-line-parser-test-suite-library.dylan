module: dylan-user

define library command-line-parser-test-suite
  use common-dylan;
  use io;
  use command-line-parser;
  use testworks;

  export command-line-parser-test-suite;
end library;

define module command-line-parser-test-suite
  use common-dylan, exclude: { format-to-string };
  use format;
  use command-line-parser;
  use testworks;

  export command-line-parser-test-suite;
end module;
