Module: dylan-user

define library logging-test-suite
  use common-dylan;
  use io;
  use logging;
  use system,
    import: { date, file-system, locators };
  use testworks;
  use testworks-specs;
  use uncommon-dylan;
  export logging-test-suite;
end;

define module logging-test-suite
  use common-dylan;
  use date;
  use logging;
  use logging-impl;
  use streams;
  use file-system;
    //import: { <pathname>, with-open-file };
  use locators;
  use testworks;
  use testworks-specs;
  use uncommon-dylan;
end;


