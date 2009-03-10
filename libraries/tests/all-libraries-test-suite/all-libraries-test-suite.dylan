Module: all-libraries-test-suite
Author: Carl Gay

define suite all-libraries-test-suite ()
  //suite anaphora-test-suite;
  //suite base64-test-suite;
  suite collection-extensions-suite; // rename to collection-extensions-test-suite
  //suite crypto-hashes-test-suite;

  // The SQL-ODBC test suite appears to require inputs and test database.
  // It also runs as an app.
  //suite sql-odbc-test-suite; // rename from sql-odbc-test (in database/)

  //suite doctower-test-suite;
  //suite wrapper-streams-test-suite; // rename from wrapper-streams-tester.  needs an -app
  //suite flow-test-suite;
  //suite flow-printer-test-suite;
  //suite graph-viewer-test-suite;
  //suite graphviz-renderer-test-suite;
  //suite gui-sniffer-test-suite;
  //suite http-hacking-11932-11957.diff-test-suite;
  //suite id3v2-test-suite;
  //suite inertia-test-suite;   // needs conversion to testworks
  //suite layer-test-suite;
  //suite libopengl-dylan-test-suite;  // convert to testworks
  //suite libpdf-dylan-test-suite;     // convert to testworks
  //suite libpng-dylan-test-suite;     // convert to testworks
  //suite libpostgresql-dylan-test-suite;
  //suite libsdl-dylan-test-suite;
  //suite mathematics-test-suite;
  //suite meta-test-suite;
  //suite monday-test-suite;
  suite network-test-suite;
  //suite network-flow-test-suite;
  //suite network-interfaces-test-suite;
  //suite packetizer-test-suite;
  //suite priority-queue-test-suite;
  //suite programming-tools-test-suite;
  //suite protocols-test-suite;
  //suite registry-test-suite;
  suite regular-expressions-test-suite;
  //suite sniffer-test-suite;
  suite strings-test-suite;
  //suite tcp-command-server-test-suite;
  //suite tests-test-suite;
  //suite timer-test-suite;
  //suite ui-test-suite;
  suite uri-test-suite;
  //suite utilities-test-suite;
  suite command-line-parser-test-suite;
  suite logging-test-suite;
  //suite vector-table-test-suite;
  suite xml-test-suite;
  //suite xsd-generator-test-suite;
  //suite zlib-test-suite;
end suite all-libraries-test-suite;

define method main () => ()
  let filename = locator-name(as(<file-locator>, application-name()));
  if (split(filename, ".")[0] = "all-libraries-test-suite")
    run-test-application(all-libraries-test-suite);
  end;
end method main;

begin
  main()
end;
