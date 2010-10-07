Module: dylan-user

define library regular-expressions-test-suite
  use common-dylan;
  use io,
    import: { streams };
  use system,
    import: { file-system,
              locators,
              operating-system };
  use strings;
  use testworks;
  use testworks-specs;
  use regular-expressions,
    import: { regex-implementation };

  export regular-expressions-test-suite;
end library regular-expressions-test-suite;

define module regular-expressions-test-suite
  use common-dylan,
    rename: { format-to-string => sprintf };
  use regex-implementation;
  use file-system;
  use locators,
    import: { <directory-locator>,
              <file-locator>,
              locator-name,
              subdirectory-locator };
  use operating-system,
    import: { environment-variable };
  use testworks;
  use testworks-specs;
  use streams;
  use strings,
    import: { trim };

  export regular-expressions-test-suite;
end module regular-expressions-test-suite;


