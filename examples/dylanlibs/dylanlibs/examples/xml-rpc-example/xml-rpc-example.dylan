Module:    xml-rpc-example
Synopsis:  An example of using xml-rpc
Author:    Chris Double
Copyright: (C) 2000, Chris Double.  All rights reserved.
License:   See License.txt

define method get-state-name(number :: <integer>) => (r :: <string>)
  xml-rpc-send("betty.userland.com", 80, "/RPC2", "examples.getStateName", number);
end method get-state-name;

define method get-state-struct(struct :: <table>) => (r :: <string>)
  xml-rpc-send("betty.userland.com", 80, "/RPC2", "examples.getStateStruct", struct);
end method get-state-struct;

define method get-state-list(states :: <sequence>) => (r :: <string>)
  xml-rpc-send("betty.userland.com", 80, "/RPC2", "examples.getStateList", states);
end method get-state-list;

define method get-service-info(channel :: <integer>) => (r :: <table>)
  xml-rpc-send("aggregator.userland.com", 80, "/RPC2", "aggregator.getServiceInfo", channel);  
end method get-service-info;

define method main () => ()
  format-out("State 41 is %s\n", get-state-name(41));
  
  let struct = make(<string-table>);
  struct["state1"] := 10;
  struct["state2"] := 20;
  struct["state3"] := 41;
  format-out("States 10, 20 and 41 are: %s\n", get-state-struct(struct));

  format-out("States 12, 28, 33, 39 and 46 are: %s\n",
             get-state-list(vector(12, 28, 33, 39, 46)));

  let service-info = get-service-info(747);
  for(value keyed-by name in service-info)
    format-out("Name: [%s]\n", name);
    format-out("Value: [%=]\n", value);
  end for;
    
end method main;

begin
  start-xml-rpc();

  // Install your proxy server here
  dynamic-bind(*default-proxy-server* = #f)
    main();
  end;
end;
