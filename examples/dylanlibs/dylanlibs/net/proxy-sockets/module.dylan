Module:    dylan-user
Synopsis:  Support for sockets through a proxy server
Author:    Chris Double
Copyright: (C) 2000, Chris Double.  All rights reserved.
License:   See license.txt

define module proxy-sockets
  use functional-dylan;
  use streams;
  use format;
  use threads;
  use sockets;

  // Add binding exports here.
  export *default-proxy-server*, 
        <proxy-server>, 
        <generic-proxy-server>,
	proxy-server-connect,
	proxy-server-host,
	proxy-server-port,
	make-proxy-socket;
end module proxy-sockets;
