Module:   dylan-user
Synopsis: Tests the xml-rpc-client library
Author:   Carl Gay

define library xml-rpc-test
  use common-dylan;
  use xml-rpc-client;
end;


define module xml-rpc-test
  use common-dylan;
  use simple-io, import: { format-out };
  use xml-rpc-client;
end;

