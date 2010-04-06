Module: dylan-user
Author: Carl Gay

define library uncommon-dylan-test-suite
  use common-dylan;
  use io;
  use system;
  use testworks;
  use testworks-specs;
  use uncommon-dylan;

  export uncommon-dylan-test-suite;
end;

define module uncommon-dylan-test-suite
  use common-dylan;
  use locators, import: { locator-name, <file-locator> };
  use standard-io, import: { *standard-output* };
  use streams, import: { force-output };
  use testworks;
  use testworks-specs;
  use uncommon-dylan;

  export uncommon-dylan-test-suite;
end;
