Module:    dylan-user
Synopsis:  An example of using xml-rpc
Author:    Chris Double
Copyright: (C) 2000, Chris Double.  All rights reserved.
License:   See License.txt

define module xml-rpc-example
  use functional-dylan;
  use format-out;
  use threads;
  use proxy-sockets, 
    import: { <generic-proxy-server>, *default-proxy-server* };
  use xml-rpc-client;

  // Add binding exports here.

end module xml-rpc-example;
