Module: http-test-suite
Author: Carl Gay

define suite http-test-suite ()
  suite http-client-test-suite;
  suite http-common-test-suite;
  suite http-protocol-test-suite;
  suite koala-test-suite;
end;
