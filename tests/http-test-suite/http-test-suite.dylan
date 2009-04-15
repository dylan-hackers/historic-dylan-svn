Module: http-test-suite
Author: Carl Gay
Synopsis: Tests for the libraries that implement HTTP.

define suite http-test-suite ()
  suite http-client-test-suite;
  suite http-common-test-suite;
  suite http-protocol-test-suite;
  suite koala-test-suite;
end;

define method main () => ()
  let filename = locator-name(as(<file-locator>, application-name()));
  if (split(filename, ".")[0] = "http-test-suite")
    run-test-application(http-test-suite);
  end;
end method main;

begin
  main()
end;
