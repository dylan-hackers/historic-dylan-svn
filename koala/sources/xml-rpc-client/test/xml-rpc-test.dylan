Module:   xml-rpc-test
Synopsis: Tests the xml-rpc-client library
Author:   Carl Gay


define method main () => ()
  let host = "localhost";
  let port = 7020;
  let url = "/RPC2";
  for (val in vector(-1, 0, 1,
                     3.14,
                     "a string",
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
    if (val ~= val2)
      format-out("%= is not the same as %=\n", val, val2);
    else
      format-out("%= transmitted okay\n", val);
    end;
  end;
  let s = "my dog has fleas";
  if (s ~= base64-decode(base64-encode(s)))
    format-out("base64 encoding/decoding is broken.\n");
  end;
end;

begin
  main();
end;
