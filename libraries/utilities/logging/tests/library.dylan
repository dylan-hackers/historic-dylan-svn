Module: dylan-user
Author: Carl Gay

define library logging-test-suite
  use common-dylan;
  use generic-arithmetic;
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
  use generic-arithmetic,
    import: { <integer> => <double-integer>, + => plus, * => mul, / => div };
  use logging;
  use logging-impl;
  use streams;
  use file-system;
    //import: { <pathname>, with-open-file };
  use locators;
  use testworks;
  use testworks-specs;
  use uncommon-dylan;

  export logging-test-suite;
end;


