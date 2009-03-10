Module: network-test-suite
Author: Carl Gay

define suite network-test-suite ()
  suite http-test-suite;
  suite wiki-test-suite;
  suite xml-rpc-client-test-suite;
end;

define method main () => ()
  let filename = locator-name(as(<file-locator>, application-name()));
  if (split(filename, ".")[0] = "network-test-suite")
    run-test-application(network-test-suite);
  end;
end method main;

begin
  main()
end;
