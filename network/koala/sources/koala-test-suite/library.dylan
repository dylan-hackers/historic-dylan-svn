Module:   dylan-user
Synopsis: Koala test suite
Author:   Carl Gay

define library koala-test-suite
  use common-dylan;
  use system,
    import: { date };
  use koala,
    import: { httpi };
  use testworks;
  export koala-test-suite;
end library koala-test-suite;

define module koala-test-suite
  use common-dylan;
  use date;
  use httpi;
  use testworks;
end module koala-test-suite;

