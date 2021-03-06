Module:    xml-rpc-interop-example
Synopsis:  Demonstrates making calls against an XML-RPC interop test server
Author:    Chris Double
Copyright: Copyright (c) 2001, Chris Double.  All rights reserved.

define variable *interop-server* = "www.soapware.org";
define variable *interop-port* = 80;
define variable *interop-url* = "/RPC2";

define method echo-string(input-string :: <string>)
  xml-rpc-send(*interop-server*, *interop-port*, *interop-url*,
               "interopEchoTests.echoString",
               input-string);             
end method echo-string;

define method echo-integer(input :: <integer>)
  xml-rpc-send(*interop-server*, *interop-port*, *interop-url*,
               "interopEchoTests.echoInteger",
               input);             
end method echo-integer;

define method echo-float(input :: <float>)
  xml-rpc-send(*interop-server*, *interop-port*, *interop-url*,
               "interopEchoTests.echoFloat",
               input);             
end method echo-float;

define method echo-base64(input :: <base64>)
  xml-rpc-send(*interop-server*, *interop-port*, *interop-url*,
               "interopEchoTests.echoBase64",
               input);             
end method echo-base64;

define method echo-boolean(input :: <boolean>)
  xml-rpc-send(*interop-server*, *interop-port*, *interop-url*,
               "interopEchoTests.echoBoolean",
               input);             
end method echo-boolean;

define method make-struct(s :: <string>, i :: <integer>, f :: <float>)
  let struct = make(<string-table>);
  struct["varString"] := s;
  struct["varInt"] := i;
  struct["varFloat"] := f;
  struct;
end method make-struct;

define method echo-struct(input :: <string-table>)
  xml-rpc-send(*interop-server*, *interop-port*, *interop-url*,
               "interopEchoTests.echoStruct",
               input);             
end method echo-struct;

define constant <string-vector> = limited(<vector>, of: <string>);
define constant <integer-vector> = limited(<vector>, of: <integer>);
define constant <float-vector> = limited(<vector>, of: <float>);
define constant <struct-vector> = limited(<vector>, of: <string-table>);

define method echo-string-array(input :: <string-vector>)
  as(<string-vector>, 
    xml-rpc-send(*interop-server*, *interop-port*, *interop-url*,
               "interopEchoTests.echoStringArray",
               input));             
end method echo-string-array;

define method echo-integer-array(input :: <integer-vector>)
  as(<integer-vector>,
    xml-rpc-send(*interop-server*, *interop-port*, *interop-url*,
                 "interopEchoTests.echoIntegerArray",
                 input));             
end method echo-integer-array;

define method echo-float-array(input :: <float-vector>)
  as(<float-vector>,
    xml-rpc-send(*interop-server*, *interop-port*, *interop-url*,
                 "interopEchoTests.echoFloatArray",
                 input));             
end method echo-float-array;

define method echo-struct-array(input :: <struct-vector>)
  as(<struct-vector>, 
    xml-rpc-send(*interop-server*, *interop-port*, *interop-url*,
                 "interopEchoTests.echoStructArray",
                 input));             
end method echo-struct-array;

define method no-in-params()
    xml-rpc-send(*interop-server*, *interop-port*, *interop-url*,
                 "interopEchoTests.noInParams");
end method no-in-params;

define generic make-random(type) => (r :: <object>);

define constant $valid-chars = 
  #[ 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
     'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
     'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M',
     'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
     '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
     '.', '-', '_', ':', ' ', '\t', '\n', '<', '&', '>', '!' ];

define method make-random(s == <string>) => (r :: <string>)
  local method is-white?(n)
    n == #x20 | n == #x9 | n == #xd | n == #xa
  end;

  let s = make(<string>, size: random(1024));
  for(n from 0 below s.size)
    // temporary workaround for leading/trailing space problem
    let x = as(<integer>, $valid-chars[random($valid-chars.size)]);
    while(is-white?(x))
      x := as(<integer>, $valid-chars[random($valid-chars.size)]);
    end;
    s[n] := as(<character>, x);
  finally
    s;
  end for;
end method make-random;

define method make-random(i == <integer>) => (r :: <integer>)
  random(9999999) - 5000000;
end method make-random;

define method make-random( i == <float>) => (r :: <float>)
  let f1 = as(<double-float>, random(100000) - 50000);
  let f2 = as(<double-float>, random(50000) + 1);
  f1 / f2;
end method make-random;

define method make-random( i == <boolean>) => (r :: <boolean>)
  random(2) == 0
end method make-random;

define method make-random ( i == <string-table>) => (r :: <string-table>)
  make-struct(make-random(<string>), make-random(<integer>), make-random(<float>));  
end method make-random;

define method make-random ( i == <string-table>) => (r :: <string-table>)
  make-struct(make-random(<string>), make-random(<integer>), make-random(<float>));  
end method make-random;

define method make-random ( i == <string-vector>) => (r :: <string-vector>)
  let result = make(<string-vector>, size: random(100) + 1, fill: "");
  for(n from 0 below result.size)
    result[n] := make-random(<string>);
  finally 
    result;
  end for;
end method make-random;

define method make-random ( i == <integer-vector>) => (r :: <integer-vector>)
  let result = make(<integer-vector>, size: random(100) + 1, fill: 0);
  for(n from 0 below result.size)
    result[n] := make-random(<integer>);
  finally 
    result;
  end for;
end method make-random;

define method make-random ( i == <float-vector>) => (r :: <float-vector>)
  let result = make(<float-vector>, size: random(100) + 1, fill: 0d0);
  for(n from 0 below result.size)
    result[n] := make-random(<float>);
  finally 
    result;
  end for;
end method make-random;

define method make-random ( i == <struct-vector>) => (r :: <struct-vector>)
  let result = make(<struct-vector>, size: random(100) + 1, fill: make(<string-table>));
  for(n from 0 below result.size)
    result[n] := make-random(<string-table>);
  finally 
    result;
  end for;
end method make-random;

define method test-echo-string() => (r :: <boolean>, r1 :: <string>, r2 :: <string>)
  let input = make-random(<string>);
  let result = echo-string(input); 
  values(input = result, input, result)
end method test-echo-string;

define method test-echo-integer() => (r :: <boolean>, r1 :: <integer>, r2 :: <integer>)
  let input = make-random(<integer>);
  let result = echo-integer(input); 
  values(input = result, input, result)
end method test-echo-integer;

define method test-echo-float() => (r :: <boolean>, r1 :: <float>, r2 :: <float>)
  let input = make-random(<float>);
  let result = echo-float(input); 
  values(abs(input - result) < 0.000001d0, input, result)
end method test-echo-float;

define method test-echo-base64() => (r :: <boolean>, r1 :: <base64>, r2 :: <base64>)
  let string = make-random(<string>);
  let b64 = base64-encode(string);
  let result = echo-base64(b64);
  values(b64.base64-string = result.base64-string , b64, result)
end method test-echo-base64;

define method test-echo-boolean() => (r :: <boolean>, r1 :: <boolean>, r2 :: <boolean>)
  let input = make-random(<boolean>);
  let result = echo-boolean(input); 
  values(input = result, input, result)
end method test-echo-boolean;

define method struct=(lhs :: <string-table>, rhs :: <string-table>)
  lhs["varInt"] = rhs["varInt"] &
  abs(lhs["varFloat"] - rhs["varFloat"]) < 0.000001d0 &
  lhs["varString"] = rhs["varString"];
end method struct=;

define method test-echo-struct() => (r :: <boolean>, r1 :: <string-table>, r2 :: <string-table>)
  let input = make-random(<string-table>);
  let result = echo-struct(input); 
  values(struct=(input, result), input, result)
end method test-echo-struct;

define method test-echo-string-array() => (r :: <boolean>, r1 :: <string-vector>, r2 :: <string-vector>)
  let input = make-random(<string-vector>);
  let result = echo-string-array(input); 
  values(input = result, input, result)
end method test-echo-string-array;

define method test-echo-integer-array() => (r :: <boolean>, r1 :: <integer-vector>, r2 :: <integer-vector>)
  let input = make-random(<integer-vector>);
  let result = echo-integer-array(input); 
  values(input = result, input, result)
end method test-echo-integer-array;

define method float-array=(array1 :: <float-vector>, array2 :: <float-vector>)
  block(return)
    when(array1.size ~= array2.size)
      return(#f)
    end;

    for(n from 0 below array1.size)
      when(abs(array1[n] - array2[n]) >= 0.000001d0)
        return(#f)
      end;
    end;
    #t;
  end;

end method float-array=;

define method test-echo-float-array() => (r :: <boolean>, r1 :: <float-vector>, r2 :: <float-vector>)
  let input = make-random(<float-vector>);
  let result = echo-float-array(input); 
  values(float-array=(input, result), input, result)
end method test-echo-float-array;

define method struct-array=(array1 :: <struct-vector>, array2 :: <struct-vector>)
  block(return)
    when(array1.size ~= array2.size)
      return(#f)
    end;

    for(n from 0 below array1.size)
      when(~struct=(array1[n], array2[n]))
        return(#f)
      end;
    end;
    #t;
  end;

end method struct-array=;

define method test-echo-struct-array() => (r :: <boolean>, r1 :: <struct-vector>, r2 :: <struct-vector>)
  let input = make-random(<struct-vector>);
  let result = echo-struct-array(input); 
  values(struct-array=(input, result), input, result)
end method test-echo-struct-array;

define method test-no-in-params() => (r :: <boolean>)
  object-class(no-in-params()) == <integer>
end method test-no-in-params;

define method do-test(s :: <string>, t :: <function>)
  format-out("Running %s: %s\n", s,
             if(t())
               "passed"
             else
               "failed"
             end);
end do-test;

define method call-with-server(server, port, url, fun)
  dynamic-bind(*interop-server* = server,
               *interop-port* = port,
               *interop-url* = url)
    fun();
  end;
end method call-with-server;

define method test-server(server, port, url)
  dynamic-bind(*interop-server* = server,
               *interop-port* = port,
               *interop-url* = url)
    format-out("Testing server %s on port %d at url %s\n", server, port, url);

    do-test("echo-string", test-echo-string);
    do-test("echo-integer", test-echo-integer);
    do-test("echo-float", test-echo-float);
    do-test("echo-base64", test-echo-base64);
    do-test("echo-boolean", test-echo-boolean);
    do-test("echo-struct", test-echo-struct);
    do-test("echo-string-array", test-echo-string-array);
    do-test("echo-integer-array", test-echo-integer-array);
    do-test("echo-float-array", test-echo-float-array);
    do-test("echo-struct-array", test-echo-struct-array);
    do-test("no-in-params", test-no-in-params);
    format-out("Test done.\n");
  end;
end method test-server;

define method main () => ()
  // all passed
  test-server( "www.soapware.org", 80, "/RPC2");

// no response
// test-server("bitsko.slc.ut.us", 80, "/cgi-bin/interop.pl");

// Passed except for base64.
// test-server("www.wc.cc.va.us", 80, "/dtod/xmlrpc/testing/interop.asp");

// No content-length
// test-server("xmlrpc.usefulinc.com", 80, "/demo/server.php");

// Passed
// test-server("aspx.securedomains.com", 80, "/cookcomputing/interopechotests.aspx");

// no content-length
// test-server("xmlrpc-c.sourceforge.net", 80, "/cgi-bin/interop.cgi");

// no content-length
// test-server("xmlrpc-epi.sourceforge.net", 80, "/xmlrpc_php/interop_server.php");

// no echo-boolean
// test-server("xmlrpc.soaplite.com", 80, "/interop.cgi");

end method main;

begin
  start-xml-rpc();

  main();
  format-out("\n");
end;
