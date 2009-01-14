Module: dylan-user
Author: Carl Gay

define library all-libraries-test-suite
  use common-dylan;
  use testworks;

  // Test suite libraries
  use collection-extensions-test;  // rename to -test-suite
  use command-line-parser-test-suite;
  use logging-test-suite;
  use network-test-suite;
  use regular-expressions-test-suite;
  use strings-test-suite;
  use uri-test;

  export all-libraries-test-suite;
end;

define module all-libraries-test-suite
  use common-dylan;
  use testworks;

  // Test suite modules
  use collection-extensions-test;  // rename to -test-suite
  use command-line-parser-test-suite;
  use logging-test-suite;
  use network-test-suite;
  use regular-expressions-test-suite;
  use strings-test-suite;
  use uri-test;

  export all-libraries-test-suite;
end;
