Module: dylan-user

define library regular-expressions-test-suite
  use common-dylan;
  use io,
    import: {
      streams
    };
  use regular-expressions,
    import: {
      regular-expressions,
      regexp
    };
  use system,
    import: {
      file-system,
      locators,
      operating-system
    };
  use strings;
  use testworks;
  export
    regular-expressions-test-suite;
end library regular-expressions-test-suite;

define module old-api-test-suite
  use common-dylan,
    exclude: { 
      split
    };
  use regular-expressions;
  use testworks;
  export
    //pcre-test-suite,
    old-api-test-suite;
end module old-api-test-suite;

define module new-api-test-suite
  use common-dylan,
    exclude: {
      split
    };
  use regexp;
  use file-system;
  use locators,
    import: {
      <directory-locator>,
      <file-locator>,
      subdirectory-locator
    };
  use operating-system,
    import: {
      environment-variable
    };
  use testworks;
  use streams;
  use strings,
    import: {
      trim
    };
  export
    new-api-test-suite;
end module new-api-test-suite;

define module regular-expressions-test-suite
  use testworks;
  use old-api-test-suite;
  use new-api-test-suite;
  export regular-expressions-test-suite;
end module regular-expressions-test-suite;


