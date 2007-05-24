Module: dylan-user

define library regular-expressions-test-suite
  use common-dylan;
  use testworks;
  use regular-expressions;

  export
    regular-expressions-test-suite;
end;

define module regular-expressions-test-suite
  use common-dylan, exclude: { split };
  use regular-expressions;
  use testworks;

  export
    //pcre-test-suite,
    regular-expressions-test-suite;
end;
