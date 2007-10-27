Module: dylan-user

define library regular-expressions-test-suite
  use common-dylan;
  use testworks;
  use regular-expressions,
    import: { regular-expressions, regexp };
  export
    regular-expressions-test-suite;
end;

define module old-api-test-suite
  use common-dylan, exclude: { split };
  use regular-expressions;
  use testworks;
  export
    //pcre-test-suite,
    old-api-test-suite;
end;

define module new-api-test-suite
  use common-dylan, exclude: { split };
  use regexp;
  use testworks;
  export
    new-api-test-suite;
end;

define module regular-expressions-test-suite
  use testworks;
  use old-api-test-suite;
  use new-api-test-suite;
  export regular-expressions-test-suite;
end;
