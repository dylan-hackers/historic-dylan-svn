Module:    dylan-user
Synopsis:  SSL Sockets
Author:    Chris Double
Copyright: (C) 2000, Chris Double.  All rights reserved.
License:   See License.txt

define module ssl-sockets
  use functional-dylan;
  use streams-internals;
  use sockets-internals;

  use c-ffi;
  use win32-common;
  use win32-kernel;
  use win32-user; 
  use winsock2, import: { <c-buffer-offset> };

  // Add binding exports here.
  export start-ssl-sockets, <ssl-tcp-socket>;
end module ssl-sockets;
