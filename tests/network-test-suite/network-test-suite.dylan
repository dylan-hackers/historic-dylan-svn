Module: network-test-suite
Author: Carl Gay

define suite network-test-suite ()
  suite http-test-suite;
  suite wiki-test-suite;
  suite xml-rpc-client-test-suite;
end;

