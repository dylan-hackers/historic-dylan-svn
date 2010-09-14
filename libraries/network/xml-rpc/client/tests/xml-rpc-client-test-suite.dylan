Module:   xml-rpc-client-test-suite
Synopsis: Tests the xml-rpc-client library
Author:   Carl Gay


define suite xml-rpc-client-test-suite ()
  test test-basic-types;
end;

define test test-basic-types ()
  let host = "localhost";
  let port = 7020;
  let url = "/RPC2";
  for (val in vector(-1,
                     0,
                     1,
                     3.1415927d0,
                     "a <string>",
                     vector("one", 2),
                     begin
                       let t = make(<string-table>);
                       t["one"] := 1;
                       t["two"] := 2;
                       t
                     end))
    // "echo" returns its argument(s) in an array...
    let url = parse-uri(host, port, "/RPC2");
    let result = xml-rpc-call(url, "echo", val);
    let val2 = result[0];
    check-equal(format-to-string("Sent value %= = received value %=", val, val2),
                val, val2);
  end;
end test test-basic-types;
