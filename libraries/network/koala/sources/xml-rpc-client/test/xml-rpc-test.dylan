Module:   xml-rpc-test
Synopsis: Tests the xml-rpc-client library
Author:   Carl Gay


define method main () => ()
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
    let result = xml-rpc-call-2(host, port, "/RPC2", "echo", val);
    let val2 = result[0];
    format-out("%sSent: %=, Received: %=\n",
               if (val = val2) "" else "ERROR: " end, val, val2);
  end;
  let s = "my dog has fleas";
  if (s ~= base64-decode(base64-encode(s)))
    format-out("base64 encoding/decoding is broken.\n");
  end;
end;

begin
  main();
end;
