Module: dylan-user

define library logging-test-suite
  use common-dylan;
  use io;
  use logging;
  use system,
    import: { date };
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
  use testworks;
  use testworks-specs;
  use uncommon-dylan;
end;


