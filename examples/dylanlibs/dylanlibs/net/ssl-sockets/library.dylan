Module:    dylan-user
Synopsis:  SSL Sockets
Author:    Chris Double
Copyright: (C) 2000, Chris Double.  All rights reserved.
License:   See License.txt

define library ssl-sockets
  use functional-dylan;
  use io;
  use x-sockets;
  use c-ffi;
  use win32-common;
  use win32-kernel;
  use win32-user; 
  use winsock2;

  // Add any more module exports here.
  export ssl-sockets;
end library ssl-sockets;
